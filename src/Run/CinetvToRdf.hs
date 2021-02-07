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
module Run.CinetvToRdf
  ( convertCinetvToRdf
  )
where

-- import           CineTV.RDF.Conversion             (convertToRdf)
import           Import                            hiding ((^.))
import           Namespaces
import Data.CQLOD.Readers.CineTV (readCineTV)
import Data.CQLOD.Writers.RDF (writeRdf)

import           Codec.Compression.GZip            (compress)
import qualified Data.ByteString.Lazy              as BS
import           Data.RDF                          (RDF, Rdf)
import qualified Data.RDF                          as RDF
import           Database.Esqueleto                hiding (get)
import           Database.Persist.Sqlite           (SqliteConf (..))
import qualified RIO.Text                          as T
import           System.Directory                  (doesFileExist, removeFile)
import           System.Process                    (callCommand)
import           Text.RDF.RDF4H.NTriplesSerializer
import           Text.RDF.RDF4H.TurtleSerializer
import           Data.Pool                                   (Pool)

convertCinetvToRdf :: Text -> RIO App ()
convertCinetvToRdf outputDir = do
  verifySqliteFileExists

  -- Generate RDF graph from CineTV
  graph <- convertCinetv2RdfGraph

  writeRdfGraphToRdfFormats graph outputDir

-- |Verify if the SQLite specified in the application datatype exists.
--
-- If not, the program terminates.
verifySqliteFileExists :: RIO App ()
verifySqliteFileExists = do
  sqliteDbPath        <- fmap (optionsSqlitePath . appOptions) ask
  doesSqlitePathExist <- liftIO $ doesFileExist $ T.unpack sqliteDbPath
  unless doesSqlitePathExist $ do
    logError $ display $ "Can't find SQLite database at: " <> sqliteDbPath
    exitFailure

-- |Convert CineTV database in the SQLite file in a RDF graph.
convertCinetv2RdfGraph :: RIO App (RDF RDF.AlgebraicGraph)
convertCinetv2RdfGraph = do
  logInfo "Converting CineTV in RDF..."
  env <- ask
  let sqliteDbPath = optionsSqlitePath $ appOptions env

  pool <- liftIO $ createPoolConfig (SqliteConf sqliteDbPath 1)
  cinetvToRdf pool

cinetvToRdf :: (MonadIO m, Rdf a) => Pool SqlBackend -> m (RDF a)
cinetvToRdf = readCineTV >=> writeRdf

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
  let cmtqNtriplesFpathCompressed =
        T.unpack $ outputDir <> "/cmtq-dataset.nt.gz"

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
