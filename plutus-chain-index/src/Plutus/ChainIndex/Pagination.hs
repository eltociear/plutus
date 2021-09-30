{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE UndecidableInstances  #-}
{-

Pagination allows to return a subset of results through pages. Once the first
page was requested, we can request for the next ones.

There are multiple strategies for implementation pagination, such as Offset
Pagination and Seek Pagination. Offset Pagination is the easiest to implement
and use. However, it is not performant for large offset values and it is not
consistent if new items are inserted in the database while we are querying.
For these reasons, we decided to use Seek Pagination which doesn't suffer from
those drawbacks. For a given page request, we need to provide the number of
items per page and last element we queried (can be Nothing). We suppose the
elements are ordered in ascending order.

Here's a simple illustrative example:

* Suppose we have the following items in the database [1..100].
* We first request the 50 first items.
* We obtain the first page containing [1..50].
* To request the next page, we request 50 items after the last item of the
  previous page (which is 50).
* We obtain the second page containing [51..100].
* Since we don't know this was the last page, we would request the next 50
  items after the last item (which is 100).
* We obtain a page with no elements.
-}
module Plutus.ChainIndex.Pagination
    ( PageQuery(..)
    , Page(..)
    , PageSize(..)
    , pageOf
    ) where

import           Control.Monad   (guard)
import           Data.Aeson      (FromJSON, ToJSON)
import           Data.Default    (Default (..))
import           Data.Maybe      (listToMaybe)
import qualified Data.OpenApi    as OpenApi
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           GHC.Generics    (Generic)
import           Numeric.Natural (Natural)

-- | Query parameters for pagination.
data PageQuery a = PageQuery
    { pageQuerySize     :: PageSize -- ^ Number of items per page.
    , pageQueryLastItem :: Maybe a -- ^ Last item of the queried page.
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, OpenApi.ToSchema)

instance Default (PageQuery a) where
  def = PageQuery def Nothing

newtype PageSize = PageSize { getPageSize :: Natural }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, OpenApi.ToSchema)

instance Default PageSize where
    def = PageSize 1

-- | Part of a collection
data Page a = Page
    { currentPageQuery :: PageQuery a
    -- ^ The 'PageQuery' which was used to request this 'Page'.
    , nextPageQuery    :: Maybe (PageQuery a)
    -- ^ The 'PageQuery' to use to request the next 'Page'. Nothing if we requested the last page.
    , totalItems       :: Integer
    -- ^ Total number of items (in total, not just in the current 'Page').
    , totalPages       :: Integer
    -- ^ Total number of 'Page's that can be requested.
    , pageItems        :: [a]
    -- ^ Items in the current 'Page'.
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Given a 'Set', request the 'Page' with the given 'PageQuery'.
pageOf
  :: (Eq a)
  => PageQuery a -- ^ Pagination query parameters.
  -> Set a
  -> Page a
pageOf pageQuery@PageQuery { pageQuerySize = PageSize ps, pageQueryLastItem } items =
    let totalItems = Set.size items
        ps' = fromIntegral ps
        totalPages =
            let (d, m) = totalItems `divMod` ps'
            in if m == 0 then d else d + 1
        pageItems = case pageQueryLastItem of
                      Nothing -> take ps' $ Set.toList items
                      Just i  -> take ps' $ drop 1 $ dropWhile ((/=) i) $ Set.toList items
        nextLastItem = listToMaybe $ reverse pageItems
    in Page
        { currentPageQuery = pageQuery
        , nextPageQuery = guard (length pageItems == ps')
                       >> fmap (\i -> PageQuery (PageSize ps) $ Just i) nextLastItem
        , totalPages = fromIntegral totalPages
        , totalItems = fromIntegral totalItems
        , pageItems
        }
