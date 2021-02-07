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

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
module Run.GenerateVoid
  ( generateVoid
  )
where

import           Data.CQLOD.RDF.Void                 (CinetvRdf (..),
                                                  createVoidGraph)
import           Import                          hiding ((^.))

import           Data.RDF                        (RDF)
import qualified Data.RDF                        as RDF
import           Data.Time.Clock                 (UTCTime, getCurrentTime)
import           Data.Time.Format                (defaultTimeLocale, parseTimeM)
import           Database.HSparql.Connection     (EndPoint)
import qualified RIO.Text                        as T
import           Text.RDF.RDF4H.TurtleSerializer
import           Text.RE.TDFA.Text

generateVoid :: Text -> RIO App ()
generateVoid outputDir = do
  -- Generate VoID RDF graph
  logInfo "Generating VoID data from RDF data..."

  -- parseFile (T.unpack $ outputDir <> "/cmtq-dataset.ttl.gz")
  -- content <- liftIO $ BS.readFile (T.unpack $ outputDir <> "/cmtq-dataset.ttl.gz") >>= (return . TL.toStrict . TL.decodeUtf8 . decompress)
  -- graph <- case (RDF.parseString (RDF.TurtleParser (Just $ RDF.BaseUrl "http://data.cinematheque.qc.ca") Nothing) content :: Either RDF.ParseFailure (RDF RDF.TList)) of
  --   Left err -> do
  --     logError $ displayShow err
  --     exitFailure
  --   Right g -> return g

  originalDate <- getSqliteDbDate
  currentTime  <- liftIO getCurrentTime

  let sparqlEndpoint = "http://localhost:9999/blazegraph/sparql" :: EndPoint
  let cinetvRdf      = CinetvRdf originalDate currentTime sparqlEndpoint
  voidGraph <- liftIO (createVoidGraph cinetvRdf :: IO (RDF RDF.AlgebraicGraph))

  -- Write VoID RDF graph
  logInfo "Writing VoID data to file..."
  let voidGraphFpath = T.unpack $ outputDir <> "/void.ttl"
  liftIO $ withFile
    voidGraphFpath
    WriteMode
    (\h -> RDF.hWriteRdf
      (TurtleSerializer Nothing (RDF.prefixMappings voidGraph))
      h
      voidGraph
    )

getSqliteDbDate :: RIO App UTCTime
getSqliteDbDate = do
  sqliteDbPath <- fmap (optionsSqlitePath . appOptions) ask
  let utcTimeM = getUtcTimeFromDbFile sqliteDbPath
  case utcTimeM of
    Just utcTime -> return utcTime
    Nothing      -> do
      logError "The SQLite file must contain a date in the format YYYY-MM-DD"
      exitFailure
 where
  getUtcTimeFromDbFile :: Text -> Maybe UTCTime
  getUtcTimeFromDbFile fname = do
    date <- matchedText $ fname ?=~ [re|[0-9]+-[0-9]+-[0-9]+|]
    parseTimeM False defaultTimeLocale "%Y-%-m-%-d" (T.unpack date)

