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
module Run
  ( run
  )
where

import           CineTV.RDF.Conversion             (convertToRdf)
import           CineTV.RDF.Void                   (CinetvRdf(..), createVoidGraph)
import           Import                            hiding ((^.))
import           Namespaces

import           Codec.Compression.GZip            (compress)
import           Control.Monad.State               (execStateT)
-- import           Data.Aeson                        (ToJSON (..))
-- import           Data.Aeson.Text                   (encodeToLazyText)
import qualified Data.ByteString.Lazy              as BS
import           Data.RDF                          (RDF, Rdf)
import qualified Data.RDF                          as RDF
import           Data.Time.Clock                   (UTCTime, getCurrentTime, diffUTCTime)
import           Data.Time.Format                  (defaultTimeLocale,
                                                    parseTimeM)
import           Database.Esqueleto                hiding (get)
import           Database.Persist.Sqlite           (SqliteConf (..))
import qualified RIO.Text                          as T
import           System.Directory                  (createDirectoryIfMissing,
                                                    doesFileExist, removeFile)
import           System.FilePath                   (joinPath)
import           System.Process                    (callCommand)
import           Text.RDF.RDF4H.NTriplesSerializer
import           Text.RDF.RDF4H.TurtleSerializer
import           Text.RE.TDFA.Text

-- import qualified Data.Text.Lazy.Encoding as TL
-- import qualified Data.Text.Lazy as TL

run :: RIO App ()
run = do
  startTime  <- liftIO getCurrentTime

  command <- fmap (optionsCommand . appOptions) ask
  case command of
    CinetvToRdf -> convertCinetvToRdf
    GenerateVoid -> generateVoid

  endTime <- liftIO getCurrentTime
  logInfo $ display $ "Finished in " <> T.pack (show $ diffUTCTimeSec endTime startTime) <> "s!"

 where
   diffUTCTimeSec :: UTCTime -> UTCTime -> Double
   diffUTCTimeSec endTime startTime = realToFrac $ diffUTCTime endTime startTime

convertCinetvToRdf :: RIO App ()
convertCinetvToRdf = do
  verifySqliteFileExists

  outputDir <- createOutputDirIfMissing

  -- Generate RDF graph from CineTV
  graph <- convertCinetv2RdfGraph

  writeRdfGraphToRdfFormats graph outputDir

generateVoid :: RIO App ()
generateVoid = do
  outputDir <- createOutputDirIfMissing

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

  let sparqlEndpoint = "http://localhost:9999/blazegraph/sparql"
  let cinetvRdf = CinetvRdf originalDate currentTime sparqlEndpoint
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

-- |Verify if the SQLite specified in the application datatype exists.
--
-- If not, the program terminates.
verifySqliteFileExists :: RIO App ()
verifySqliteFileExists = do
  sqliteDbPath <- fmap (optionsSqlitePath . appOptions) ask
  doesSqlitePathExist <- liftIO $ doesFileExist $ T.unpack sqliteDbPath
  unless doesSqlitePathExist $ do
    logError $ display $ "Can't find SQLite database at: " <> sqliteDbPath
    exitFailure

-- |Create output directory if missing.
createOutputDirIfMissing :: RIO App Text
createOutputDirIfMissing = do
  env <- ask
  let outputDir = T.pack $ joinPath
        [T.unpack $ optionsOutputDir $ appOptions env, "cmtq-dataset"]
  liftIO $ createDirectoryIfMissing True $ T.unpack outputDir
  return outputDir

-- |Convert CineTV database in the SQLite file in a RDF graph.
convertCinetv2RdfGraph :: RIO App (RDF RDF.AdjHashMap)
convertCinetv2RdfGraph = do
  logInfo "Converting CineTV in RDF..."
  env <- ask
  let sqliteDbPath = optionsSqlitePath $ appOptions env

  pool <- liftIO $ createPoolConfig (SqliteConf sqliteDbPath 1)
  let emptyRdf = RDF.mkRdf [] Nothing prefixMappings :: RDF RDF.AdjHashMap
  liftIO $ execStateT (convertToRdf pool) emptyRdf

-- |
writeRdfGraphToRdfFormats :: (Rdf a) => RDF a -> Text -> RIO App ()
writeRdfGraphToRdfFormats graph outputDir = do
  -- Write RDF graph to compressed Turtle file
  writeRdfGraphToTurtleFile graph outputDir

  -- Write RDF graph to N-Triples file
  writeRdfGraphToNTriplesFile graph outputDir

  -- Write RDF data to HDT file"
  writeRdfGraphToHdtFile outputDir


writeRdfGraphToTurtleFile :: (Rdf a) => RDF a -> Text -> RIO App ()
writeRdfGraphToTurtleFile graph outputDir = do
  logInfo "Write RDF data to compressed Turtle file"
  let cmtqTurtleFpath = T.unpack $ outputDir <> "/cmtq-dataset.ttl"
  liftIO $ withFile
    cmtqTurtleFpath
    WriteMode
    (\h -> RDF.hWriteRdf (TurtleSerializer Nothing prefixMappings) h graph)

  let cmtqTurtleFpathCompressed = cmtqTurtleFpath <> ".gz"
  liftIO
    $   BS.readFile cmtqTurtleFpath
    >>= (return . compress)
    >>= BS.writeFile cmtqTurtleFpathCompressed

  liftIO $ removeFile cmtqTurtleFpath

writeRdfGraphToNTriplesFile :: (Rdf a) => RDF a -> Text -> RIO App ()
writeRdfGraphToNTriplesFile graph outputDir = do
  logInfo "Write RDF data to compressed N-Triples file"
  let cmtqNtriplesFpath = T.unpack $ outputDir <> "/cmtq-dataset.nt"
  liftIO $ withFile cmtqNtriplesFpath
                    WriteMode
                    (\h -> RDF.hWriteRdf NTriplesSerializer h graph)

  -- Compress output N-triples file
  let cmtqNtriplesFpathCompressed = cmtqNtriplesFpath <> ".gz"
  liftIO
    $   BS.readFile cmtqNtriplesFpath
    >>= (return . compress)
    >>= BS.writeFile cmtqNtriplesFpathCompressed

  liftIO $ removeFile cmtqNtriplesFpath

writeRdfGraphToHdtFile :: Text -> RIO App ()
writeRdfGraphToHdtFile outputDir = do
  logInfo "Write RDF data to HDT file"
  let cmtqHdtFpath = T.unpack $ outputDir <> "/cmtq-dataset.hdt"
  let cmtqNtriplesFpathCompressed = T.unpack $ outputDir <> "/cmtq-dataset.nt.gz"

  env <- ask
  let baseUri = optionsBaseUri $ appOptions env
  let rdf2hdtCmd =
        "rdf2hdt -B "
          <> T.unpack baseUri
          <> "/resource -f ttl "
          <> cmtqNtriplesFpathCompressed
          <> " "
          <> cmtqHdtFpath
  liftIO $ callCommand rdf2hdtCmd

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
