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
module CineTV.RDF.Conversion.PlaceSpec
  ( spec
  )
where

import           CineTV.RDF.Conversion.Place      (convertPlaces)
import           Control.Monad.State              (execStateT)
import           Import
import           Namespaces
import qualified SW.Vocabulary                    as SW

import           Data.Pool                        (Pool)
import           Data.RDF                         (RDF)
import qualified Data.RDF                         as RDF
import qualified Data.RDF.Namespace               as RDF
import qualified Data.RDF.Types.Extended          as RDF
import           Database.CineTv.Public.Model
import           Database.Esqueleto               hiding (get)
import           Database.Persist.Sqlite          (SqliteConf (..))
import           Test.Hspec
import           Test.Hspec.Expectations.Extended (shouldContainElems)

spec :: Spec
spec = do
  pool  <- runIO dbSetup
  graph <- runIO $ execStateT (convertPlaces pool) emptyGraph

  describe "converting cinetv places to RDF"
    $ it "should create a place for each row in table Pays"
    $ do
        let placeUri = "/resource/Place100"

        RDF.triplesOf graph `shouldContainElems` catMaybes
          [ RDF.mkTriple placeUri SW.rdfType SW.crmE53
          , RDF.mkTripleLit placeUri SW.rdfsLabel (RDF.PlainLL "Québec" "fr")
          , RDF.mkTriple placeUri SW.crmP1 "/resource/AppellationPlace100"
          , RDF.mkTriple placeUri SW.crmP48 "/resource/IdentifierPlace100"
          , RDF.mkTriple placeUri SW.owlSameAs (RDF.mkUri wd "Q176")
          , RDF.mkTriple "/resource/AppellationPlace100" SW.rdfType SW.crmE41
          , RDF.mkTripleLit "/resource/AppellationPlace100"
                            SW.crmP190
                            (RDF.PlainL "Québec")
          , RDF.mkTriple "/resource/IdentifierPlace100" SW.rdfType SW.crmE42
          , RDF.mkTripleLit "/resource/IdentifierPlace100"
                            SW.crmP190
                            (RDF.PlainL "100")
          ]

emptyGraph :: RDF RDF.TList
emptyGraph = RDF.mkRdf [] Nothing prefixMappings

dbSetup :: IO (Pool SqlBackend)
dbSetup = do
  pool <- createPoolConfig (SqliteConf ":memory:" 1)
  _    <- liftIO $ flip liftSqlPersistMPool pool $ do
    runMigration migrateAll
    insertKey (toSqlKey 100) $ Pays "Québec"
    insert $ Pays_LienWikidata (toSqlKey 100)
                               (Just "http://www.wikidata.org/entity/Q176")

  return pool


