{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main where

import           Control.Lens                   hiding (argument, set', (<.>))
import           Control.Monad
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Lazy.Char8     as BSL
import           Data.Coerce
import qualified Data.Csv                       as Csv
import qualified Data.IntMap                    as IM
import           Data.List                      (sortOn)
import qualified Data.Text                      as T
import           Flat                           (unflat)
import           GHC.Generics
import           Options.Applicative
import qualified PlutusCore                     as PLC
import qualified PlutusCore.Name                as PLC
import           PlutusCore.Quote               (runQuoteT)
import           PlutusIR                       as PIR
import qualified PlutusIR.Analysis.RetainedSize as PIR
import qualified PlutusIR.Compiler              as PIR
import           PlutusIR.Core.Plated


data Options = Options
  { opPath      :: FilePath
  , opOptimize  :: Bool
  , opRetention :: Bool
  }

options :: Parser Options
options = Options
            <$> argument str (metavar "FILE.flat")
            <*> switch' (long "dont-optimize"
                        <> help "Don't optimize"
                        )
            <*> switch (long "retention"
                       <> help "Print retention map to stdout"
                       )
  where
    switch' :: Mod FlagFields Bool -> Parser Bool
    switch' = fmap not . switch

type PIRTerm  = PIR.Term PLC.TyName PLC.Name PLC.DefaultUni PLC.DefaultFun ()
type PLCTerm  = PLC.Term PLC.TyName PLC.Name PLC.DefaultUni PLC.DefaultFun (PIR.Provenance ())
type PIRError = PIR.Error PLC.DefaultUni PLC.DefaultFun (PIR.Provenance ())
type PIRCompilationCtx a = PIR.CompilationCtx PLC.DefaultUni PLC.DefaultFun a

compile
  :: Options -> PIRTerm -> Either PIRError PLCTerm
compile opts pirT = do
  plcTcConfig <- PLC.getDefTypeCheckConfig PIR.noProvenance
  let pirCtx = defaultCompilationCtx plcTcConfig
  runExcept $ flip runReaderT pirCtx $ runQuoteT $ PIR.compileTerm pirT

  where
    set' :: Lens' PIR.CompilationOpts b -> (Options -> b) -> PIRCompilationCtx a -> PIRCompilationCtx a
    set' pirOpt opt = set (PIR.ccOpts . pirOpt) (opt opts)

    defaultCompilationCtx :: PLC.TypeCheckConfig PLC.DefaultUni PLC.DefaultFun -> PIRCompilationCtx a
    defaultCompilationCtx plcTcConfig =
      PIR.toDefaultCompilationCtx plcTcConfig
      & set' PIR.coOptimize                     opOptimize

loadPirAndCompile :: Options -> IO ()
loadPirAndCompile opts = do
  let path = opPath opts
  putStrLn $ "!!! Loading file " ++ path
  bs <- BS.readFile path
  case unflat bs of
    Left decodeErr -> error $ show decodeErr
    Right pirT -> do
      when (opRetention opts) $ do
          let rm = PIR.termRetentionMap pirT
              bns = pirT ^.. termSubtermsDeep.termBindings.bindingNames
              btns = pirT ^.. termSubtermsDeep.termBindings.bindingTyNames.coerced
              nameTable = IM.fromList $ fmap (\ n -> (coerce $ nameUnique n , nameString n)) $ bns++btns
              top50 = fmap (\(i,s) -> RetentionRecord (IM.findWithDefault "???" i nameTable) i s) . take 50 . sortOn (negate . snd) $ IM.assocs rm
          BSL.putStr $ Csv.encodeDefaultOrderedByName top50
      putStrLn "!!! Compiling"
      case compile opts pirT of
        Left pirError -> error $ show pirError
        Right _       -> putStrLn "!!! Compilation successful"

main :: IO ()
main = loadPirAndCompile =<< execParser opts
  where
    opts =
      info (options <**> helper)
           ( fullDesc
           <> progDesc "Load a flat pir term from file and run the compiler on it"
           <> header "pir - a small tool for loading pir from flat representation and compiling it")

data RetentionRecord = RetentionRecord { name :: T.Text, unique :: Int, size :: PIR.Size}
    deriving stock (Generic, Show)
    deriving anyclass Csv.ToNamedRecord
    deriving anyclass Csv.DefaultOrdered

deriving newtype instance Csv.ToField PIR.Size


-- -- | All the identifiers/names introduced by this binding
-- -- In case of a datatype-binding it has multiple identifiers: the type, constructors, match function
-- -- adapted from PlutusIR.Core.Plated.bindingIds
-- bindingNamesAnns :: (PLC.HasUnique tyname PLC.TypeUnique, PLC.HasUnique name PLC.TermUnique)
--                  => Traversal1' (Binding tyname name uni fun a) (Name, ann)
-- bindingNamesAnns f = \case
--    TermBind x s d t -> flip (TermBind x s) t <$> (PLC.varDeclName . PLC.theUnique) f d
--    TypeBind a d ty -> flip (TypeBind a) ty <$> (PLC.tyVarDeclName . PLC.theUnique) f d
--    DatatypeBind a1 (Datatype a2 tvdecl tvdecls n vdecls) ->
--      DatatypeBind a1 <$>
--        (Datatype a2 <$> (PLC.tyVarDeclName . PLC.theUnique) f tvdecl
--                     <.*> traverse1Maybe ((PLC.tyVarDeclName . PLC.theUnique) f) tvdecls
--                     <.> PLC.theUnique f n
--                     <.*> traverse1Maybe ((PLC.varDeclName . PLC.theUnique) f) vdecls)
--   where
--     -- | Traverse using 'Apply', but getting back the result in 'MaybeApply f' instead of in 'f'.
--     traverse1Maybe :: (Apply f, Traversable t) => (a -> f b) -> t a -> MaybeApply f (t b)
--     traverse1Maybe f' = traverse (MaybeApply . Left . f')

--     -- | Apply a non-empty container of functions to a possibly-empty-with-unit container of values.
--     -- Taken from: <https://github.com/ekmett/semigroupoids/issues/66#issue-271899630>
--     (<.*>) :: (Apply f) => f (a -> b) -> MaybeApply f a -> f b
--     ff <.*> MaybeApply (Left fa) = ff <.> fa
--     ff <.*> MaybeApply (Right a) = ($ a) <$> ff
--     infixl 4 <.*>
