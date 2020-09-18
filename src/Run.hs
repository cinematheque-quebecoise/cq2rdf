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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
module Run (run) where

import Import hiding ((^.))
import Namespaces
import Util (getCurrentDayText)
import Conversion.CineTV (convertToRdf)

import qualified RIO.Text as Text
import qualified Data.Text.Lazy.IO as TextL
import Control.Monad.State (execStateT)
import Data.RDF (RDF)
import qualified Data.RDF as RDF
import Text.RDF.RDF4H.TurtleSerializer
import Text.RDF.RDF4H.NTriplesSerializer
import Database.Esqueleto hiding (get)
import Database.Persist.Sqlite (SqliteConf(..))
import Data.Time.Clock (getCurrentTime)
import Data.Aeson (ToJSON(..))
import Data.Aeson.Text (encodeToLazyText)
import Codec.Compression.GZip (compress)
import qualified Data.ByteString.Lazy as BS
import System.Directory (doesFileExist, removeFile, createDirectoryIfMissing)
import System.FilePath (joinPath)
import Text.RE.TDFA.Text
-- import System.Process (callCommand)

-- Used for converting the prefix mappings in JSON format
instance ToJSON RDF.PrefixMappings

data Metadata = Metadata { metadataOriginalDate :: Text
                         , metadataCreationDate :: Text
                         } deriving (Generic)
instance ToJSON Metadata

-- run :: RIO App ()
-- run = do
--   let emptyRdf = RDF.empty :: RDF RDF.TList
--   let mappings = RDF.PrefixMappings $ Map.fromList [ ("mo", "http://purl.org/ontology/mo/") ]
--   let baseUrl = Just $ "http://example.org/resource"
--   liftIO $ RDF.writeRdf (TurtleSerializer baseUrl mappings) emptyRdf

run :: RIO App ()
run = do
  env <- ask

  let sqliteDbPath = optionsSqlitePath $ appOptions env
  doesSqlitePathExist <- liftIO $ doesFileExist $ Text.unpack sqliteDbPath
  unless doesSqlitePathExist $ do
    logError $ display $ "Can't find SQLite database at: " <> sqliteDbPath
    exitFailure
  pool <- liftIO $ createPoolConfig (SqliteConf sqliteDbPath 1)

  logInfo "Converting CineTV in RDF..."

  originalDate <- getSqliteDbDate
  creationDate <- liftIO $ fmap getCurrentDayText getCurrentTime

  -- let emptyRdf = RDF.empty :: RDF RDF.TList
  -- let baseUri = optionsBaseUri $ appOptions env
  -- let baseUriJust = Just $ RDF.BaseUrl baseUri
  let emptyRdf = RDF.mkRdf [] Nothing prefixMappings :: RDF RDF.TList
  graph <- liftIO $ execStateT (convertToRdf pool) emptyRdf

  let outputDir = Text.pack $ joinPath [ Text.unpack $ optionsOutputDir $ appOptions env
                                       , "cmtq-dataset"
                                       ]
  liftIO $ createDirectoryIfMissing True $ Text.unpack outputDir

  -- let prefixesFpath = Text.unpack $ outputDir <> "/prefixes.json"
  -- liftIO $ TextL.writeFile prefixesFpath $ encodeToLazyText $ RDF.prefixMappings graph

  let metadataFpath = Text.unpack $ outputDir <> "/metadata.json"
  liftIO $ TextL.writeFile metadataFpath $ encodeToLazyText $ Metadata originalDate creationDate

  let cmtqTurtleFpath = Text.unpack $ outputDir <> "/cmtq-dataset.ttl"
  liftIO $ withFile cmtqTurtleFpath WriteMode (\h -> RDF.hWriteRdf (TurtleSerializer Nothing prefixMappings) h graph)

  let cmtqNtriplesFpath = Text.unpack $ outputDir <> "/cmtq-dataset.nt"
  liftIO $ withFile cmtqNtriplesFpath WriteMode (\h -> RDF.hWriteRdf NTriplesSerializer h graph)

  -- Compress output Turtle file
  let cmtqTurtleFpathCompressed = cmtqTurtleFpath <> ".gz"
  liftIO $ BS.readFile cmtqTurtleFpath >>= (return . compress) >>= BS.writeFile cmtqTurtleFpathCompressed
  liftIO $ removeFile cmtqTurtleFpath

  -- -- Compress output N-triples file
  -- let cmtqNtriplesFpathCompressed = cmtqNtriplesFpath <> ".gz"
  -- liftIO $ BS.readFile cmtqNtriplesFpath >>= (return . compress) >>= BS.writeFile cmtqNtriplesFpathCompressed
  -- liftIO $ removeFile cmtqNtriplesFpath

  -- let cmtqHdtFpath = Text.unpack $ outputDir <> "/cmtq-dataset.hdt"
  -- let rdf2hdtCmd = "rdf2hdt -B " <> Text.unpack baseUri <> "/resource -f ttl " <> cmtqNtriplesFpathCompressed <> " " <> cmtqHdtFpath
  -- liftIO $ callCommand rdf2hdtCmd

  logInfo "Finished!"

getSqliteDbDate :: RIO App Text
getSqliteDbDate = do
  sqliteDbPath <- fmap (optionsSqlitePath . appOptions) ask
  case matchedText $ sqliteDbPath ?=~ [re|[0-9]+-[0-9]+-[0-9]+|] of
    Just date -> return date
    Nothing -> do
      logError "The SQLite file must contain a date in the format YYYY-MM-DD"
      exitFailure
