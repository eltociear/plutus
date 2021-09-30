{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings #-}

module Plutus.ChainIndex.PaginationSpec (tests) where

import           Data.Maybe                   (fromJust)
import qualified Data.Set                     as Set
import           Hedgehog                     (Property, forAll, property, (===))
import           Hedgehog.Gen                 as Gen
import           Hedgehog.Range               as Gen
import           Plutus.ChainIndex            (Page (nextPageQuery, pageItems, totalItems, totalPages),
                                               PageQuery (PageQuery), PageSize (PageSize))
import           Plutus.ChainIndex.Pagination (pageOf)
import           Test.Tasty
import           Test.Tasty.Hedgehog          (testProperty)

tests :: TestTree
tests = do
  testGroup "pagination"
    [ testGroup "pageOf"
        [ testProperty "requested page size equal to length of items" pageSizeEqualItemsLengthSpec
        , testProperty "total pages equal 2" totalPagesSpec
        , testProperty "total items equal to length of input" totalItemsLengthOfInputSpec
        , testProperty "last page should have no next page" lastPageShouldHaveNoNextPageQuerySpec
        , testProperty "able to request next page" canRequestNextPageSpec
        ]
    ]

pageSizeEqualItemsLengthSpec :: Property
pageSizeEqualItemsLengthSpec = property $ do
  items <- forAll $ Gen.set (Gen.linear 2 10) $ Gen.int (Gen.linear 0 100)
  let pageSize = PageSize 2
  length (pageItems $ pageOf (PageQuery pageSize Nothing) items) === 2

totalPagesSpec :: Property
totalPagesSpec = property $ do
  items <- forAll $ Gen.set (Gen.linear 2 10) $ Gen.int (Gen.linear 0 100)
  let pageSize = PageSize $ fromIntegral $ length items - 1
  totalPages (pageOf (PageQuery pageSize Nothing) items) === 2

totalItemsLengthOfInputSpec :: Property
totalItemsLengthOfInputSpec = property $ do
  items <- forAll $ Gen.set (Gen.linear 1 10) $ Gen.int (Gen.linear 0 100)
  let pageSize = PageSize 1
  totalItems (pageOf (PageQuery pageSize Nothing) items) === fromIntegral (length items)

lastPageShouldHaveNoNextPageQuerySpec :: Property
lastPageShouldHaveNoNextPageQuerySpec = property $ do
  items <- forAll $ Gen.set (Gen.linear 1 10) $ Gen.int (Gen.linear 0 100)
  let pageSize = PageSize $ fromIntegral $ length items + 1
  nextPageQuery (pageOf (PageQuery pageSize Nothing) items) === Nothing

canRequestNextPageSpec :: Property
canRequestNextPageSpec = property $ do
  let item1 = 0 :: Integer
  let item2 = 1 :: Integer
  let items = Set.fromList [item1, item2]
  let pageSize = PageSize 1
  let pageOne = pageOf (PageQuery pageSize Nothing) items
  nextPageQuery pageOne === Just (PageQuery pageSize (Just item1))
  let pageTwo = pageOf (fromJust $ nextPageQuery pageOne) items
  nextPageQuery pageTwo === Just (PageQuery pageSize (Just item2))
  let lastPage = pageOf (fromJust $ nextPageQuery pageTwo) items
  nextPageQuery lastPage === Nothing
