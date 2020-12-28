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
module CineTV.RDF.Conversion.Language
  ( convertLanguages
  )
where

import           Data.RDF.Types.Extended      (mkTriple, mkTripleLit)
import           Database.CineTv.Public.Model (Langue (..),
                                               Langue_LienWikidata (..))
import           Import                       hiding ((^.))
import qualified SW.Vocabulary                as SW
import           Util                         (sqlKeyToText)

import           Data.Pool                    (Pool)
import qualified Data.RDF                     as RDF
import           Data.RDF.State
import           Database.Esqueleto           hiding (get)

baseUriPath :: Text
baseUriPath = "/resource"

{-|
Create all triples for representing the Language concept for all rows in table Langue.

Generated triples:

@
for each row in table Langue
    cmtq:Language{Langue.LangueID} rdf:type crm:E56_Language
    cmtq:Language{Langue.LangueID} rdfs:label {Langue.Terme}@fr
    cmtq:Language{Langue.LangueID} crm:P48_has_preferred_identifier cmtq:IdentifierLanguage{Langue.LangueID}
    cmtq:Language{Langue.LangueID} crm:P1_is_identified_by cmtq:AppellationLanguage{Langue.LangueID}

    cmtq:AppellationLanguage{Langue.LangueID} rdf:type crm:E41_Appellation
    cmtq:AppellationLanguage{Langue.LangueID} crm:P190_has_symbolic_content {Langue.Terme}

    cmtq:IdentifierLanguage{Langue.LangueID} rdf:type crm:E42_Identifier
    cmtq:IdentifierLanguage{Langue.LangueID} crm:P190_has_symbolic_content {Langue.LangueID}
@
-}
convertLanguages
  :: (RDF.Rdf rdfImpl, MonadIO m) => Pool SqlBackend -> RdfState rdfImpl m ()
convertLanguages pool = do
  createTriplesFromLangues pool
  createTriplesFromAllLangueLienWikidata pool

getLangueEntities :: (MonadIO m) => Pool SqlBackend -> m [Entity Langue]
getLangueEntities pool =
  liftIO $ flip liftSqlPersistMPool pool $ select $ distinct $ from return

createTriplesFromLangues
  :: (RDF.Rdf rdfImpl, MonadIO m) => Pool SqlBackend -> RdfState rdfImpl m ()
createTriplesFromLangues pool = do
  langueEntities <- getLangueEntities pool
  mapM_ createTriplesFromLangue langueEntities

createTriplesFromLangue
  :: (RDF.Rdf rdfImpl, Monad m) => Entity Langue -> RdfState rdfImpl m ()
createTriplesFromLangue langueEntity = do
  let langueTextId = (sqlKeyToText . entityKey) langueEntity
  let langueLabel  = (langueTerme . entityVal) langueEntity
  let langueUri    = baseUriPath <> "/Language" <> langueTextId
  let langueIdentifierUri =
        baseUriPath <> "/IdentifierLanguage" <> langueTextId
  let langueAppellationUri =
        baseUriPath <> "/AppellationLanguage" <> langueTextId

  mapM_ addTriple $ mkTriple langueUri SW.rdfType SW.crmE56
  mapM_ addTriple
    $ mkTripleLit langueUri SW.rdfsLabel (RDF.PlainLL langueLabel "fr")
  mapM_ addTriple $ mkTriple langueUri SW.crmP1 langueAppellationUri
  mapM_ addTriple $ mkTriple langueUri SW.crmP48 langueIdentifierUri

  mapM_ addTriple $ mkTriple langueAppellationUri SW.rdfType SW.crmE41
  mapM_ addTriple
    $ mkTripleLit langueAppellationUri SW.crmP190 (RDF.PlainL langueLabel)

  mapM_ addTriple $ mkTriple langueIdentifierUri SW.rdfType SW.crmE42
  mapM_ addTriple
    $ mkTripleLit langueIdentifierUri SW.crmP190 (RDF.PlainL langueTextId)

{-|
Create all triples for representing the link between the Language concept and a Wikidata entity from all rows in table Langue_LienWikidata.

Generated triples:

@
for each row in table Langue_LienWikidata
  cmtq:Person{Langue_LienWikidata.LangueID} owl:sameAs {Langue_LienWikidata.LienWikidata}
@
-}
createTriplesFromAllLangueLienWikidata
  :: (RDF.Rdf rdfImpl, MonadIO m) => Pool SqlBackend -> RdfState rdfImpl m ()
createTriplesFromAllLangueLienWikidata pool = do
  langueLienWikidataEntities <- getAllLangueLienWikidataEntities pool
  mapM_ createTriplesFromLangueLienWikidata langueLienWikidataEntities

getAllLangueLienWikidataEntities
  :: (MonadIO m) => Pool SqlBackend -> m [Entity Langue_LienWikidata]
getAllLangueLienWikidataEntities pool =
  liftIO $ flip liftSqlPersistMPool pool $ select $ distinct $ from return

createTriplesFromLangueLienWikidata
  :: (RDF.Rdf rdfImpl, MonadIO m)
  => Entity Langue_LienWikidata
  -> RdfState rdfImpl m ()
createTriplesFromLangueLienWikidata langueLienWdEntity = do
  let wikidataUriMaybe =
        langue_LienWikidataLienWikidata $ entityVal langueLienWdEntity

  forM_ wikidataUriMaybe $ \wikidataUri -> do
    let langueId = sqlKeyToText $ langue_LienWikidataLangueId $ entityVal
          langueLienWdEntity
    let langueUri = baseUriPath <> "/Language" <> langueId
    mapM_ addTriple $ mkTriple langueUri SW.owlSameAs wikidataUri
