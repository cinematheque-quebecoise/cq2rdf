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
{-# LANGUAGE OverloadedStrings #-}
module Run
  ( run
  )
where

import           Import
import           Run.CinetvToRdf  (convertCinetvToRdf)
import           Run.AdlibToRdf  (convertAdlibToRdf)
import           Run.GenerateVoid (generateVoid)
import           Util             (createOutputDirIfMissing)

import           Data.Time.Clock  (UTCTime, diffUTCTime, getCurrentTime)
import qualified RIO.Text         as T

run :: RIO App ()
run = do
  startTime <- liftIO getCurrentTime

  outputDir <- createOutputDirIfMissing

  command   <- fmap (optionsCommand . appOptions) ask
  case command of
    CinetvToRdf  -> convertCinetvToRdf outputDir
    AdlibToRdf  -> convertAdlibToRdf
    GenerateVoid -> generateVoid outputDir

  endTime <- liftIO getCurrentTime
  logInfo
    $  display
    $  "Finished in "
    <> T.pack (show $ diffUTCTimeSec endTime startTime)
    <> "s!"

 where
  diffUTCTimeSec :: UTCTime -> UTCTime -> Double
  diffUTCTimeSec endTime startTime = realToFrac $ diffUTCTime endTime startTime
