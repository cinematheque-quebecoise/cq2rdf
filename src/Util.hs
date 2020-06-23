-- This file is part of cq2rdf.

-- cq2rdf is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- cq2rdf is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with cq2rdf.  If not, see <https://www.gnu.org/licenses/>.

{-# LANGUAGE NoImplicitPrelude #-}
-- | Silly utility module, used to demonstrate how to write a test
-- case.
module Util
  ( plus2
  , getCurrentDayText
  ) where

import RIO
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime(..))
import Data.Time.Format (formatTime, defaultTimeLocale, iso8601DateFormat)

plus2 :: Int -> Int
plus2 = (+ 2)

getCurrentDayText :: UTCTime -> Text
getCurrentDayText = Text.pack
                  . (formatTime defaultTimeLocale (iso8601DateFormat Nothing))
                  . utctDay
