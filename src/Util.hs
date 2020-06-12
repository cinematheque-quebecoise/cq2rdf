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
