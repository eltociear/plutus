{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Plutus.ChainIndex.HandlersSpec (tests) where

import           Control.Lens
import           Control.Monad                     (forM, forM_)
import           Control.Monad.Freer               (Eff, interpret, reinterpret, runM)
import           Control.Monad.Freer.Error         (Error, runError)
import           Control.Monad.Freer.Extras        (LogMessage, LogMsg (..), handleLogWriter)
import           Control.Monad.Freer.State         (State, runState)
import           Control.Monad.Freer.Writer        (runWriter)
import           Control.Monad.IO.Class            (liftIO)
import           Control.Tracer                    (nullTracer)
import           Data.List                         (groupBy, sort)
import           Data.Maybe                        (fromJust, listToMaybe, mapMaybe)
import           Data.Sequence                     (Seq)
import           Database.Beam.Migrate.Simple      (autoMigrate)
import qualified Database.Beam.Sqlite              as Sqlite
import qualified Database.Beam.Sqlite.Migrate      as Sqlite
import qualified Database.SQLite.Simple            as Sqlite
import qualified Generators                        as Gen
import           Hedgehog                          (Property, assert, forAll, property, (===))
import           Ledger                            (Address (addressCredential), TxOut (txOutAddress))
import           Plutus.ChainIndex                 (ChainIndexLog,
                                                    Page (currentPageQuery, nextPageQuery, pageItems, totalItems, totalPages),
                                                    PageQuery (PageQuery), PageSize (PageSize), appendBlock, txFromTxId,
                                                    utxoSetAtAddress)
import           Plutus.ChainIndex.ChainIndexError (ChainIndexError)
import           Plutus.ChainIndex.DbStore         (DbStoreEffect, checkedSqliteDb, handleDbStore)
import           Plutus.ChainIndex.Effects         (ChainIndexControlEffect, ChainIndexQueryEffect)
import           Plutus.ChainIndex.Handlers        (ChainIndexState, handleControl, handleQuery)
import           Plutus.ChainIndex.Tx              (citxTxId, txOutsWithRef)
import           Test.Tasty
import           Test.Tasty.Hedgehog               (testProperty)

tests :: TestTree
tests = do
  testGroup "chain-index handlers"
    [ testProperty "get tx from tx id" txFromTxIdSpec
    , testProperty "get utxo set at address" utxoSetAtAddressSpec
    ]

-- | Tests we can correctly query a tx in the database using a tx id. We also
-- test with an non-existant tx id.
txFromTxIdSpec :: Property
txFromTxIdSpec = property $ do
  (tip, block@(fstTx:_)) <- forAll $ Gen.evalUtxoGenState Gen.genNonEmptyBlock
  unknownTxId <- forAll Gen.genRandomTxId
  txs <- liftIO $ Sqlite.withConnection ":memory:" $ \conn -> do
    Sqlite.runBeamSqlite conn $ autoMigrate Sqlite.migrationBackend checkedSqliteDb
    liftIO $ runChainIndex mempty conn $ do
      appendBlock tip block
      tx <- txFromTxId (view citxTxId fstTx)
      tx' <- txFromTxId unknownTxId
      pure (tx, tx')

  case txs of
    Right (Just tx, Nothing) -> fstTx === tx
    _                        -> Hedgehog.assert False

-- | Test whether with can correctly use pagination when querying for utxos
-- from a given address.
utxoSetAtAddressSpec :: Property
utxoSetAtAddressSpec = property $ do
  (tip, block) <- forAll $ Gen.evalUtxoGenState Gen.genNonEmptyBlock

  -- Find all pairs of (Credential, TxOutRef)
  let addrsWithTxOutRef =
        concatMap ( fmap (\t -> (addressCredential $ txOutAddress $ fst t, snd t))
                  . txOutsWithRef
                  ) block
  -- Keep only the number of TxOutRef per Credential
  let addrTxOutRefCount =
          mapMaybe (\group -> listToMaybe group >>= \(c, _) -> pure (c, length group))
        $ groupBy (\t1 t2 -> fst t1 == fst t2)
        $ sort addrsWithTxOutRef

  result <- liftIO $ Sqlite.withConnection ":memory:" $ \conn -> do
    Sqlite.runBeamSqlite conn $ autoMigrate Sqlite.migrationBackend checkedSqliteDb
    liftIO $ runChainIndex mempty conn $ do
      -- Append the generated block in the chain index
      appendBlock tip block

      forM addrTxOutRefCount $ \(cred, txOutRefCount) -> do
        -- In order to test pagination, we request only @txOutRefCount - 1@
        -- elements per page, where @n@ is the number of items. In that
        -- scenario, 2 things can happen:
        --
        --   1. There's only one 'TxOutRef'. In that case, we should expect 2
        --   pages (1 page with a single element and 1 page with no elements).
        --
        --   2. There are two 'TxOutRef's. In that case, we should expect 3
        --   pages (2 pages with a single element and 1 page with no elements).
        --
        --   3. There's more than two 'TxOutRef'. In that case, we should expect
        --   2 pages (1 page with @txOutRefCount - 1@ elements and 1 page with a
        --   single element).
        let pageSize = if txOutRefCount == 1 then 1 else txOutRefCount - 1
            pageQuery = PageQuery (PageSize $ fromIntegral pageSize) Nothing

        -- Here, we query 2 or 3 pages (depending on the number of tx outputs)
        (_, txOutRefPage) <- utxoSetAtAddress pageQuery cred
        (_, txOutRefPage') <- utxoSetAtAddress (fromJust $ nextPageQuery txOutRefPage) cred
        txOutRefPageM'' <- case nextPageQuery txOutRefPage' of
            Nothing -> pure Nothing
            Just q  -> Just . snd <$> utxoSetAtAddress q cred
        -- Since we can't use 'Hedgehog' functions in here, we return necessary
        -- variables needed for asserting properties.
        pure (cred, txOutRefCount, pageSize, pageQuery, txOutRefPage, txOutRefPage', txOutRefPageM'')

  case result of
    -- There was a 'ChainIndexError'. Tests fail.
    Left _ -> Hedgehog.assert False
    Right vals ->
      forM_ vals $ \(_, txOutRefCount, pageSize, pageQuery, txOutRefPage, txOutRefPage', txOutRefPageM'') -> do
        -- Property assertions for the first page
        currentPageQuery txOutRefPage === pageQuery
        nextPageQuery txOutRefPage === Just (PageQuery (PageSize $ fromIntegral pageSize) (Just $ last $ pageItems txOutRefPage))
        totalItems txOutRefPage === fromIntegral txOutRefCount
        totalPages txOutRefPage === if txOutRefCount == 1 then 1 else 2
        length (pageItems txOutRefPage) === pageSize

        -- Property assertions for the second page
        nextPageQuery txOutRefPage === Just (currentPageQuery txOutRefPage')
        nextPageQuery txOutRefPage' === if txOutRefCount == 2
                                           then Just (PageQuery (PageSize $ fromIntegral pageSize)
                                                                (Just $ last $ pageItems txOutRefPage'))
                                           else Nothing
        totalPages txOutRefPage' === if txOutRefCount == 1 then 1 else 2
        totalItems txOutRefPage' === fromIntegral txOutRefCount
        length (pageItems txOutRefPage') === if txOutRefCount == 1 then 0 else 1

        -- Property assertions for maybe the third page
        case txOutRefPageM'' of
          Nothing -> Hedgehog.assert True
          Just txOutRefPage'' -> do
            nextPageQuery txOutRefPage' === Just (currentPageQuery txOutRefPage'')
            nextPageQuery txOutRefPage'' === Nothing
            totalPages txOutRefPage'' === 2
            totalItems txOutRefPage'' === fromIntegral txOutRefCount
            length (pageItems txOutRefPage'') === 0

runChainIndex
  :: ChainIndexState
  -> Sqlite.Connection
  -> Eff '[ ChainIndexControlEffect
          , ChainIndexQueryEffect
          , DbStoreEffect
          , State ChainIndexState
          , Error ChainIndexError
          , LogMsg ChainIndexLog
          , IO
          ] a
  -> IO (Either ChainIndexError a)
runChainIndex appState conn effect = do
  r <- effect
    & interpret handleControl
    & interpret handleQuery
    & interpret (handleDbStore nullTracer conn)
    & runState appState
    & runError
    & reinterpret
         (handleLogWriter @ChainIndexLog
                          @(Seq (LogMessage ChainIndexLog)) $ unto pure)
    & runWriter @(Seq (LogMessage ChainIndexLog))
    & runM
  pure $ fmap fst $ fst r
