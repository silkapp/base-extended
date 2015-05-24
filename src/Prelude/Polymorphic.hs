-- | An extended prelude to avoid typing all the standard default imports.
-- The exports are polymorphic functions of the Prelude alternatives,
-- so the normal Prelude should be hidden.
--
module Prelude.Polymorphic
  ( module Prelude

  -- * Control modules
  , module Control.Applicative
  , module Control.Arrow       -- Partial
  , module Control.Category
  , module Control.Monad
  , module Control.Monad.IO.Class
  , module Control.Monad.Trans.Class

  -- * Data modules
  , module Data.Char           -- Partial
  , module Data.Data           -- Partial
  , module Data.Either
  , module Data.Function
  , module Data.Foldable
  , module Data.List           -- Partial
  , module Data.Maybe
  , module Data.Monoid         -- Partial
  , module Data.Ord
  , module Data.Traversable
  , module Data.Time           -- Partial
  , module Data.Tuple

  -- * Safe
  , module Safe

  -- * System modules
  , module Data.Time.Locale.Compat -- Partial

  ) where

import Prelude hiding (all, and, any, concat, concatMap, elem, foldl, foldl1, foldr, foldr1, id, mapM, mapM_, maximum, minimum, notElem, or, product, sequence,
                sequence_, sum, (.))

import Control.Applicative
import Control.Arrow (first, second, (&&&), (***), (+++), (|||))
import Control.Category (Category (..))
import Control.Monad hiding (forM, forM_, mapM, mapM_, msum, sequence, sequence_)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

import Data.Char hiding (GeneralCategory (..))
import Data.Data (Data (..), Typeable)
import Data.Either
import Data.Foldable
import Data.Function (on)
import Data.List hiding (all, and, any, concat, concatMap, delete, elem, find, foldl, foldl', foldl1, foldr, foldr1, mapAccumL, mapAccumR, maximum, maximumBy,
                  minimum, minimumBy, notElem, or, product, sum)
import Data.Maybe
import Data.Monoid (Monoid (..), (<>))
import Data.Ord
import Data.Time (FormatTime, NominalDiffTime, ParseTime, UTCTime, addUTCTime, diffUTCTime, formatTime, getCurrentTime, parseTime)
import Data.Time.Clock ()
import Data.Traversable
import Data.Tuple

import Safe hiding (abort)

import Data.Time.Locale.Compat (TimeLocale, defaultTimeLocale)
