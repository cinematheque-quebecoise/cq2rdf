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
module Conversion.CineTV.Place
  ( convertPlaces
  )
where

import           Data.RDF.Types.Extended      (mkTriple, mkTripleLit)
import           Database.CineTv.Public.Model
import           Import                       hiding ((^.))
import qualified SW.Vocabulary                as SW
import           Util                         (sqlKeyToText)

import           Data.Pool                    (Pool)
import qualified Data.RDF                     as RDF
import           Data.RDF.State
import           Database.Esqueleto           hiding (get)

baseUriPath :: Text
baseUriPath = "/resource"

convertPlaces
  :: (RDF.Rdf rdfImpl, MonadIO m) => Pool SqlBackend -> RdfState rdfImpl m ()
convertPlaces pool = do
  createTriplesFromPlaces pool
  createTriplesFromAllPaysLienWikidata pool

{-|
Create all triples for representing the Place concept from all rows in
table Pays. Even though the table is called 'Pays', the instances it
contains represent the more general concept 'Place'.

Generated triples:

@
for each row in table Pays
    cmtq:Place{Pays.PaysID} rdf:type crm:E53_Place
    cmtq:Place{Pays.PaysID} rdfs:label {Pays.Terme}
@
-}
createTriplesFromPlaces
  :: (RDF.Rdf rdfImpl, MonadIO m) => Pool SqlBackend -> RdfState rdfImpl m ()
createTriplesFromPlaces pool = do
  paysEntities <- getPaysEntities pool
  mapM_ createTriplesFromPlace paysEntities

getPaysEntities :: (MonadIO m) => Pool SqlBackend -> m [Entity Pays]
getPaysEntities pool =
  liftIO $ flip liftSqlPersistMPool pool $ select $ distinct $ from return

createTriplesFromPlace
  :: (RDF.Rdf rdfImpl, Monad m) => Entity Pays -> RdfState rdfImpl m ()
createTriplesFromPlace paysEntity = do
  let placeId        = sqlKeyToText $ entityKey paysEntity
  let placeLabel     = paysTerme $ entityVal paysEntity

  let placeUri       = baseUriPath <> "/Place" <> placeId
  let appellationUri = baseUriPath <> "/AppellationPlace" <> placeId
  let identifierUri  = baseUriPath <> "/IdentifierPlace" <> placeId

  mapM_ addTriple $ mkTriple placeUri SW.rdfType SW.crmE53
  mapM_ addTriple $ mkTripleLit placeUri SW.rdfsLabel (placeLabel <> "@fr")
  mapM_ addTriple $ mkTriple placeUri SW.crmP48 identifierUri
  mapM_ addTriple $ mkTriple placeUri SW.crmP1 appellationUri
  mapM_ addTriple $ mkTripleLit identifierUri SW.crmP190 placeId
  mapM_ addTriple $ mkTripleLit appellationUri SW.crmP190 placeLabel

{-|
Create all triples for representing the link between the Place concept from all rows in
table Pays_LienWikidata and Wikidata.

Generated triples:

@
for each row in table Pays_LienWikidata
    cmtq:Place{Pays_LienWikidata.PaysID} owl:sameAs {Pays_LienWikidata.LienWikidata}
@
-}
createTriplesFromAllPaysLienWikidata
  :: (RDF.Rdf rdfImpl, MonadIO m) => Pool SqlBackend -> RdfState rdfImpl m ()
createTriplesFromAllPaysLienWikidata pool = do
  paysLienWikidataEntities <- getAllPaysLienWikidataEntities pool
  mapM_ createTriplesFromPaysLienWikidata paysLienWikidataEntities

getAllPaysLienWikidataEntities
  :: (MonadIO m) => Pool SqlBackend -> m [Entity Pays_LienWikidata]
getAllPaysLienWikidataEntities pool =
  liftIO $ flip liftSqlPersistMPool pool $ select $ distinct $ from return

createTriplesFromPaysLienWikidata
  :: (RDF.Rdf rdfImpl, MonadIO m)
  => Entity Pays_LienWikidata
  -> RdfState rdfImpl m ()
createTriplesFromPaysLienWikidata paysLienWdEntity = do
  let placeId          = sqlKeyToText $ pays_LienWikidataPaysId paysLienWd
  let placeUri         = baseUriPath <> "/Place" <> placeId
  let wikidataUriMaybe = pays_LienWikidataLienWikidata paysLienWd

  forM_ wikidataUriMaybe $ \wikidataUri ->
    mapM_ addTriple $ mkTriple placeUri SW.owlSameAs wikidataUri
  where paysLienWd = entityVal paysLienWdEntity
