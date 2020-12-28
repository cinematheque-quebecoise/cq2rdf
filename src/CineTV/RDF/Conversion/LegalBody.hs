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
module CineTV.RDF.Conversion.LegalBody
  ( convertLegalBodies
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
Create all triples for representing the LegalBody concept for all row between Sujet and Filmo_Generique.

Generated triples:

@
for each row in table Sujet where Sujet.SujetId = Filmo_Generique.OrganismeID
  cmtq:LegalBody{Sujet.SujetID} rdf:type crm:E40_Legal_Body
  cmtq:LegalBody{Sujet.SujetID} rdfs:label {Sujet.Terme}
  cmtq:LegalBody{Sujet.SujetID} crm:P1_is_identified_by cmtq:AppellationLegalBody{Sujet.SujetID}
  cmtq:LegalBody{Sujet.SujetID} crm:P48_has_preferred_identifier cmtq:IdentifierPerson{Sujet.SujetID}

  cmtq:appellationLegalBody{Sujet.SujetID} rdf:type crm:E41_Appellation
  cmtq:appellationLegalBody{Sujet.SujetID} crm:P190_has_symbolic_content "{Sujet.Terme}"

  cmtq:identifierLegalBody{Sujet.SujetID} rdf:type crm:E42_Identifier
  cmtq:identifierLegalBody{Sujet.SujetID} crm:P190_has_symbolic_content "{Sujet.SujetID}"
@
-}
convertLegalBodies
  :: (RDF.Rdf rdfImpl, MonadIO m) => Pool SqlBackend -> RdfState rdfImpl m ()
convertLegalBodies = createTriplesFromOrganismes

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
  mapM_ addTriple
    $ mkTripleLit legalBodyUri SW.rdfsLabel (RDF.PlainL legalBodyTerm)
  mapM_ addTriple $ mkTriple legalBodyUri SW.crmP48 identifierUri
  mapM_ addTriple $ mkTriple legalBodyUri SW.crmP1 appellationUri

  mapM_ addTriple $ mkTriple appellationUri SW.rdfType SW.crmE41
  mapM_ addTriple
    $ mkTripleLit appellationUri SW.crmP190 (RDF.PlainL legalBodyTerm)

  mapM_ addTriple $ mkTriple identifierUri SW.rdfType SW.crmE42
  mapM_ addTriple
    $ mkTripleLit identifierUri SW.crmP190 (RDF.PlainL legalBodyId)

 where
  legalBodyId   = sqlKeyToText $ entityKey subjectEntity
  legalBodyTerm = sujetTerme $ entityVal subjectEntity

