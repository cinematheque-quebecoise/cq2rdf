-- This file is part of cq3rdf.

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
module CineTV.RDF.Conversion.MovieResumeSpec
  ( spec
  )
where

import           CineTV.RDF.Conversion.MovieResume (convertMoviesResume)
import qualified Data.RDF.Types.Extended           as RDF (mkTriple,
                                                           mkTripleLit)
import           Import
import           Namespaces
import qualified SW.Vocabulary                     as SW

import           Control.Monad.State               (execStateT)
import           Data.Pool                         (Pool)
import           Data.RDF                          (RDF)
import qualified Data.RDF                          as RDF
import           Database.CineTv.Public.Model
import           Database.Esqueleto                hiding (get)
import           Database.Persist.Sqlite           (SqliteConf (..))
import           Test.Hspec
import           Test.Hspec.Expectations.Extended  (shouldContainElems)

spec :: Spec
spec = do
  pool  <- runIO dbSetup
  graph <- runIO $ execStateT (convertMoviesResume pool) emptyGraph

  describe "converting cinetv movie resume to RDF"
    $ it "should create movie resume for each row in table Filmo_Resume"
    $ do
        let workUri            = "/resource/Work1"
        let synopsisFrenchUri  = "/resource/Synopsis50Language38"
        let synopsisEnglishUri = "/resource/Synopsis100Language8"
        let synopsisTypeUri    = "/resource/Synopsis"

        RDF.triplesOf graph `shouldContainElems` catMaybes
          [ RDF.mkTriple synopsisTypeUri SW.rdfType SW.crmE55
          , RDF.mkTriple synopsisFrenchUri SW.rdfType SW.crmE33
          , RDF.mkTriple synopsisFrenchUri SW.crmP67 workUri
          , RDF.mkTriple synopsisFrenchUri SW.crmP2 synopsisTypeUri
          , RDF.mkTripleLit synopsisFrenchUri
                            SW.crmP190
                            (RDF.PlainL "Résumé en français")
          , RDF.mkTriple synopsisFrenchUri SW.crmP72 "/resource/Language38"
          , RDF.mkTriple synopsisFrenchUri SW.crmP73 synopsisEnglishUri
          , RDF.mkTriple workUri SW.crmP67 synopsisEnglishUri
          , RDF.mkTriple synopsisEnglishUri SW.rdfType SW.crmE33
          , RDF.mkTriple synopsisEnglishUri SW.crmP2 synopsisTypeUri
          , RDF.mkTripleLit synopsisEnglishUri
                            SW.crmP190
                            (RDF.PlainL "Resume in english")
          , RDF.mkTriple synopsisEnglishUri SW.crmP72 "/resource/Language8"
          , RDF.mkTriple synopsisEnglishUri SW.crmP73 synopsisFrenchUri
          ]

emptyGraph :: RDF RDF.TList
emptyGraph = RDF.mkRdf [] Nothing prefixMappings

dbSetup :: IO (Pool SqlBackend)
dbSetup = do
  pool <- createPoolConfig (SqliteConf ":memory:" 1)
  _    <- liftIO $ flip liftSqlPersistMPool pool $ do
    runMigration migrateAll
    insertKey (toSqlKey 10) $ Pays "Québec"
    insertKey (toSqlKey 1) $ Filmo Nothing
                                   Nothing
                                   Nothing
                                   Nothing
                                   Nothing
                                   Nothing
                                   Nothing
                                   Nothing
                                   Nothing
                                   Nothing
                                   Nothing
                                   Nothing
                                   Nothing
    insertKey (toSqlKey 50)
      $ FilmoResumes (toSqlKey 1) (Just "Résumé en français")
    insertKey (toSqlKey 100)
      $ FilmoResumesAnglais (toSqlKey 1) (Just "Resume in english")
  return pool
