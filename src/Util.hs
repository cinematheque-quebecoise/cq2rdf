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

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- | Silly utility module, used to demonstrate how to write a test
-- case.
module Util
  ( plus2
  , getCurrentDayText
  , sqlKeyToText
  , parseDateField
  , parseYearField
  , utcTimeToText
  )
where

import qualified Data.Text          as Text
import           Data.Time.Clock    (UTCTime (..))
import           Data.Time.Format   (defaultTimeLocale, formatTime,
                                     iso8601DateFormat, parseTimeM)
import           Database.Esqueleto (Key, SqlBackend, ToBackendKey, fromSqlKey)
import           RIO
import           Text.XML.XSD       (fromUTCTime)

plus2 :: Int -> Int
plus2 = (+ 2)

getCurrentDayText :: UTCTime -> Text
getCurrentDayText =
  Text.pack . formatTime defaultTimeLocale (iso8601DateFormat Nothing) . utctDay

sqlKeyToText :: (ToBackendKey SqlBackend a) => Key a -> Text
sqlKeyToText key = Text.pack $ show $ fromSqlKey key

-- | Parses a YEAR field in the database.
parseYearField
  :: (Show a)
  => a -- ^ Year field in the format YYYY
  -> Maybe UTCTime
parseYearField year =
  parseTimeM False defaultTimeLocale "%-Y" (show year) :: Maybe UTCTime

-- | Parses a DATE field in the database.
parseDateField
  :: Text -- ^ Date field in the format DD-MM-YY
  -> Maybe UTCTime
parseDateField date =
  parseTimeM False defaultTimeLocale "%d-%-m-%-y" (Text.unpack date) :: Maybe
      UTCTime

-- | Formats a UTCTime in Text format. The format is: 1988-01-01T00:00:00Z
utcTimeToText :: UTCTime -> Text
utcTimeToText = Text.pack . show . fromUTCTime
