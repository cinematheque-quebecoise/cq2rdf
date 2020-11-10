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
module Conversion.CineTV.Person
  ( convertPeople
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

{-|
Create all triples for representing the Person concept for all row in table Nom.

Generated triples:

@
for each row in table Nom where Nom.NomId = Filmo_Generique.NomID
  cmtq:Person{Nom.NomID} rdf:type crm:E21_Person
  cmtq:Person{Nom.NomID} rdfs:label {Nom.Prenom + Nom.Nom}
  cmtq:Person{Nom.NomID} foaf:name {Nom.Prenom + Nom.Nom}
  cmtq:Person{Nom.NomID} foaf:familyName {Nom.Nom}
  cmtq:Person{Nom.NomID} foaf:givenName {Nom.Prenom}
  cmtq:Person{Nom.NomID} crm:P1_is_identified_by cmtq:AppellationPerson{Nom.NomID}
  cmtq:Person{Nom.NomID} crm:P48_has_preferred_identifier cmtq:IdentifierPerson{Nom.NomID}

  cmtq:AppellationPerson{Nom.NomID} rdf:type crm:E41_Appellation
  cmtq:AppellationPerson{Nom.NomID} crm:P190_has_symbolic_content "{Nom.Prenom + Nom.Nom}"

  cmtq:IdentifierPerson{Nom.NomID} rdf:type crm:E42_Identifier
  cmtq:IdentifierPerson{Nom.NomID} crm:P190_has_symbolic_content "{Nom.NomID}"

for each row in table Nom_LienWikidata
  cmtq:Person{Nom_LienWikidata.NomID} owl:sameAs {Nom_LienWikidata.LienWikidata}
@
-}
convertPeople
  :: (RDF.Rdf rdfImpl, MonadIO m) => Pool SqlBackend -> RdfState rdfImpl m ()
convertPeople pool = do
  createTriplesFromPeople pool
  createTriplesFromAllNomLienWikidata pool
  createTriplesFromOrganismes pool

createTriplesFromPeople
  :: (RDF.Rdf rdfImpl, MonadIO m) => Pool SqlBackend -> RdfState rdfImpl m ()
createTriplesFromPeople pool = do
  nomEntities <- getNomEntities pool
  mapM_ createTriplesFromPerson nomEntities

getNomEntities :: (MonadIO m) => Pool SqlBackend -> m [Entity Nom]
getNomEntities pool =
  liftIO $ flip liftSqlPersistMPool pool $ select $ distinct $ from return

createTriplesFromPerson
  :: (RDF.Rdf rdfImpl, Monad m) => Entity Nom -> RdfState rdfImpl m ()
createTriplesFromPerson nomEntity = do
  let personUri      = baseUriPath <> "/Person" <> personTextId
  let identifierUri  = baseUriPath <> "/IdentifierPerson" <> personTextId
  let appellationUri = baseUriPath <> "/AppellationPerson" <> personTextId

  mapM_ addTriple $ mkTriple personUri SW.rdfType SW.crmE21
  mapM_ addTriple $ mkTriple personUri SW.crmP48 identifierUri

  mapM_ addTriple $ mkTriple identifierUri SW.rdfType SW.crmE42
  mapM_ addTriple $ mkTripleLit identifierUri SW.crmP190 (RDF.PlainL personTextId)

  forM_ nameOpt $ \name -> do
    mapM_ addTriple $ mkTripleLit personUri SW.rdfsLabel (RDF.PlainL name)
    mapM_ addTriple $ mkTripleLit personUri SW.foafName (RDF.PlainL name)
    mapM_ addTriple $ mkTriple personUri SW.crmP1 appellationUri

    mapM_ addTriple $ mkTriple appellationUri SW.rdfType SW.crmE41
    mapM_ addTriple $ mkTripleLit appellationUri SW.crmP190 (RDF.PlainL name)

  forM_ lastnameOpt $ \lastname ->
    mapM_ addTriple $ mkTripleLit personUri SW.foafFamilyName (RDF.PlainL lastname)

  forM_ firstnameOpt $ \firstname ->
    mapM_ addTriple $ mkTripleLit personUri SW.foafGivenName (RDF.PlainL firstname)

 where
  personTextId = sqlKeyToText $ entityKey nomEntity
  firstnameOpt    = nomPrenom $ entityVal nomEntity
  lastnameOpt     = nomNom $ entityVal nomEntity
  nameOpt      = case (firstnameOpt, lastnameOpt) of
    (f@(Just _), l@(Just _)) -> f <> Just " " <> l
    (f@(Just _), Nothing   ) -> f
    (Nothing   , l@(Just _)) -> l
    (Nothing   , Nothing   ) -> Nothing

createTriplesFromAllNomLienWikidata
  :: (RDF.Rdf rdfImpl, MonadIO m) => Pool SqlBackend -> RdfState rdfImpl m ()
createTriplesFromAllNomLienWikidata pool = do
  nomLienWikidataEntities <- getAllNomLienWikidataEntities pool
  mapM_ createTriplesFromNomLienWikidata nomLienWikidataEntities

getAllNomLienWikidataEntities
  :: (MonadIO m) => Pool SqlBackend -> m [Entity Nom_LienWikidata]
getAllNomLienWikidataEntities pool =
  liftIO $ flip liftSqlPersistMPool pool $ select $ distinct $ from return

createTriplesFromNomLienWikidata
  :: (RDF.Rdf rdfImpl, MonadIO m)
  => Entity Nom_LienWikidata
  -> RdfState rdfImpl m ()
createTriplesFromNomLienWikidata nomLienWdEntity = do
  let wikidataUriMaybe =
        nom_LienWikidataLienWikidata $ entityVal nomLienWdEntity

  case wikidataUriMaybe of
    Just wikidataUri -> do
      let nomId =
            sqlKeyToText $ nom_LienWikidataNomId $ entityVal nomLienWdEntity
      let nomUri = baseUriPath <> "/Person" <> nomId
      mapM_ addTriple $ mkTriple nomUri SW.owlSameAs wikidataUri
    Nothing -> return ()

createTriplesFromOrganismes
  :: (RDF.Rdf rdfImpl, MonadIO m) => Pool SqlBackend -> RdfState rdfImpl m ()
createTriplesFromOrganismes pool = do
  sujetEntity <- getOrganismeSujetEntities pool
  mapM_ createTriplesFromOrganisme sujetEntity

getOrganismeSujetEntities :: (MonadIO m) => Pool SqlBackend -> m [Entity Sujet]
getOrganismeSujetEntities pool = do
  results <-
    liftIO
    $ flip liftSqlPersistMPool pool
    $ select
    $ distinct
    $ from
    $ \(filmoGenerique, sujet) -> do
        where_
          (sujet ?. SujetId ==. filmoGenerique ^. Filmo_GeneriqueOrganismeId)
        return sujet

  return $ catMaybes results

createTriplesFromOrganisme
  :: (RDF.Rdf rdfImpl, Monad m) => Entity Sujet -> RdfState rdfImpl m ()
createTriplesFromOrganisme subjectEntity = do
  let legalBodyUri   = baseUriPath <> "/LegalBody" <> legalBodyId
  let identifierUri  = baseUriPath <> "/IdentifierLegalBody" <> legalBodyId
  let appellationUri = baseUriPath <> "/AppellationLegalBody" <> legalBodyId

  mapM_ addTriple $ mkTriple legalBodyUri SW.rdfType SW.crmE40
  mapM_ addTriple $ mkTripleLit legalBodyUri SW.rdfsLabel (RDF.PlainL legalBodyTerm)

  mapM_ addTriple $ mkTriple legalBodyUri SW.crmP48 identifierUri
  mapM_ addTriple $ mkTripleLit identifierUri SW.crmP190 (RDF.PlainL legalBodyId)

  mapM_ addTriple $ mkTriple legalBodyUri SW.crmP1 appellationUri
  mapM_ addTriple $ mkTripleLit appellationUri SW.crmP190 (RDF.PlainL legalBodyTerm)

 where
  legalBodyId   = sqlKeyToText $ entityKey subjectEntity
  legalBodyTerm = sujetTerme $ entityVal subjectEntity
