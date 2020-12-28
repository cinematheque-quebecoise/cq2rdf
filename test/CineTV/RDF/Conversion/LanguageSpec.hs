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
module CineTV.RDF.Conversion.LanguageSpec
  ( spec
  )
where

import           Control.Monad.State          (execStateT)
import           CineTV.RDF.Conversion.Language   (convertLanguages)
import           Import
import           Namespaces
import qualified SW.Vocabulary                as SW

import           Data.Pool                    (Pool)
import           Data.RDF                     (RDF)
import qualified Data.RDF                     as RDF
import qualified Data.RDF.Extended            as RDF (mkNode, mkTriple,
                                                      mkTripleLit)
import           Test.Hspec.Expectations.Extended (shouldContainElems)
import qualified Data.RDF.Namespace           as RDF
import           Database.CineTv.Public.Model
import           Database.Esqueleto           hiding (get)
import           Database.Persist.Sqlite      (SqliteConf (..))
import           Test.Hspec

spec :: Spec
spec = do
  pool <- runIO dbSetup
  graph <- runIO $ execStateT (convertLanguages pool) emptyGraph

  describe "converting cinetv languages to RDF"
    $ it "should create a language for each row in table Langue"
    $ do
        let languageUri    = "/resource/Language100"
        let appellationUri = "/resource/AppellationLanguage100"
        let identifierUri  = "/resource/IdentifierLanguage100"

        RDF.triplesOf graph `shouldContainElems` catMaybes
          [ RDF.mkTriple languageUri SW.rdfType SW.crmE56
          , RDF.mkTripleLit languageUri SW.rdfsLabel (RDF.PlainLL "français" "fr")
          , RDF.mkTriple languageUri SW.crmP1 appellationUri
          , RDF.mkTriple languageUri SW.crmP48 identifierUri
          , RDF.mkTriple languageUri SW.owlSameAs (RDF.mkUri wd "Q150")

          , RDF.mkTriple identifierUri SW.rdfType SW.crmE42
          , RDF.mkTripleLit identifierUri SW.crmP190 (RDF.PlainL "100")

          , RDF.mkTriple appellationUri SW.rdfType SW.crmE41
          , RDF.mkTripleLit appellationUri SW.crmP190 (RDF.PlainL "français")
          ]

        length (RDF.query graph (RDF.mkNode languageUri) Nothing Nothing)
          `shouldBe` 5
        length (RDF.query graph (RDF.mkNode appellationUri) Nothing Nothing)
          `shouldBe` 2
        length (RDF.query graph (RDF.mkNode identifierUri) Nothing Nothing)
          `shouldBe` 2

emptyGraph :: RDF RDF.TList
emptyGraph = RDF.mkRdf [] Nothing prefixMappings

dbSetup :: IO (Pool SqlBackend)
dbSetup = do
  pool <- createPoolConfig (SqliteConf ":memory:" 1)
  _    <- liftIO $ flip liftSqlPersistMPool pool $ do
    runMigration migrateAll
    insertKey (toSqlKey 100) $ Langue "français"
    insert $ Langue_LienWikidata (toSqlKey 100)
                                 (Just "http://www.wikidata.org/entity/Q150")

  return pool
