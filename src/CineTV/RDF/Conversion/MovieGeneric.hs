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
module CineTV.RDF.Conversion.MovieGeneric
  ( convertMoviesGeneric
  )
where

import qualified Data.RDF.Types.Extended      as RDF (mkTriple)
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

convertMoviesGeneric
  :: (RDF.Rdf rdfImpl, MonadIO m) => Pool SqlBackend -> RdfState rdfImpl m ()
convertMoviesGeneric = createTriplesFromMovieGenerics

{-|
Create all triples for representing the recording event concept for all rows in table Filmo_Generique.

Some roles will not be used to create the RecordingRoleActivity triples
such as Générique Additionnel (id=13).

Generated triples:

@
for each row in table Filmo_Generique
 if {Filmo_Generique.FonctionID = 33 (Source Originale)}
   cmtq:RecordingWork{Filmo_Generique.FilmoID} crm:R2_is_derivative_of cmtq:Work{Filmo_Generique.FilmoID}-{Filmo_Generique.NomID}
   cmtq:Work{Filmo_Generique.FilmoID}d frbroo:R16i_was_initiated_by cmtq:WorkConception{Filmo_Generique.FilmoID}-{Filmo_Generique.NomID ou Filmo_Generique.OrganismeId}
   cmtq:WorkConception{Filmo_Generique.FilmoID}-{Filmo_Generique.NomID ou Filmo_Generique.OrganismeId} crm:P14_carried_out_by cmtq:Person{Filmo_Generique.NomID} ou cmtq:LegalBody{Filmo_Generique.OrganismeId}
 else
   recordingActivityUri = cmtq:RecordingActivity{Filmo_Generique.FilmoID}-{Filmo_Generique.FonctionID}-{Filmo_Generique.NomID ou Filmo_Generique.OrganismeID}
   cmtq:RecordingEvent{Filmo_Generique.FilmoID} crm:P9_consists_of cmtq:RecordingActivity{Filmo_Generique.FilmoID}-{Filmo_Generique.FonctionID}-{Filmo_Generique.NomID ou Filmo_Generique.OrganismeID}
   cmtq:RecordingActivityCarriedOutBy{Filmo_Generique.FilmoID}-{Filmo_Generique.FonctionID}-{Filmo_Generique.NomID ou Filmo_Generique.OrganismeID} cmtqo:P01_has_domain cmtq:RecordingActivity{Filmo_Generique.FilmoID}-{Filmo_Generique.FonctionID}-{Filmo_Generique.NomID ou Filmo_Generique.OrganismeID}
   cmtq:RecordingActivityCarriedOutBy{Filmo_Generique.FilmoID}-{Filmo_Generique.FonctionID}-{Filmo_Generique.NomID ou Filmo_Generique.OrganismeID} cmtqo:P02_has_range (cmtqo:Person{Filmo_Generique.NomID} ou cmtqo:LegalBody{Filmo_Generique.OrganismeId)
   cmtq:RecordingActivityCarriedOutBy{Filmo_Generique.FilmoID}-{Filmo_Generique.FonctionID}-{Filmo_Generique.NomID ou Filmo_Generique.OrganismeID} cmtqo:P14.1_in_the_role_of cmtqo:Role{Filmo_Generique.FonctionID}
@
-}
createTriplesFromMovieGenerics
  :: (RDF.Rdf rdfImpl, MonadIO m) => Pool SqlBackend -> RdfState rdfImpl m ()
createTriplesFromMovieGenerics pool = do
  filmoGeneriqueEntities <- getFilmoGeneriqueEntities pool
  mapM_ createTriplesFromFilmoGenerique filmoGeneriqueEntities

getFilmoGeneriqueEntities
  :: (MonadIO m) => Pool SqlBackend -> m [Entity Filmo_Generique]
getFilmoGeneriqueEntities pool =
  liftIO
    $ flip liftSqlPersistMPool pool
    $ select
    $ distinct
    $ from
    $ \filmoGenerique -> return filmoGenerique

createTriplesFromFilmoGenerique
  :: (RDF.Rdf rdfImpl, Monad m)
  => Entity Filmo_Generique
  -> RdfState rdfImpl m ()
createTriplesFromFilmoGenerique filmoGeneriqueEntity = do
  let recordingActivityRoleTextId =
        (sqlKeyToText . filmo_GeneriqueFonctionId . entityVal)
          filmoGeneriqueEntity
  if recordingActivityRoleTextId == "33"
    then createDerivingWorkTriples filmoGeneriqueEntity
    else createRecordingActivityTriples filmoGeneriqueEntity

createRecordingActivityTriples
  :: (RDF.Rdf rdfImpl, Monad m)
  => Entity Filmo_Generique
  -> RdfState rdfImpl m ()
createRecordingActivityTriples filmoGeneriqueEntity = do
  let generiqueId = sqlKeyToText $ entityKey filmoGeneriqueEntity
  let filmoId =
        (sqlKeyToText . filmo_GeneriqueFilmoId . entityVal) filmoGeneriqueEntity
  let fonctionId = (sqlKeyToText . filmo_GeneriqueFonctionId . entityVal)
        filmoGeneriqueEntity

  let recordingEventUri = baseUriPath <> "/RecordingEvent" <> filmoId
  let roleActivityUri   = baseUriPath <> "/RecordingActivity" <> generiqueId
  let roleActivityCarriedOutByUri =
        baseUriPath <> "/RecordingActivityCarriedOutBy" <> generiqueId
  let roleUri = baseUriPath <> "/Role" <> fonctionId

  let personUriMaybe =
        ( fmap (((baseUriPath <> "/Person") <>) . sqlKeyToText)
          . filmo_GeneriqueNomId
          . entityVal
          )
          filmoGeneriqueEntity

  let legalBodyUriMaybe =
        ( fmap (((baseUriPath <> "/LegalBody") <>) . sqlKeyToText)
          . filmo_GeneriqueOrganismeId
          . entityVal
          )
          filmoGeneriqueEntity

  mapM_ addTriple $ RDF.mkTriple recordingEventUri SW.crmP9 roleActivityUri
  mapM_ addTriple
    $ RDF.mkTriple roleActivityCarriedOutByUri SW.crmP01 roleActivityUri
  mapM_ addTriple $ RDF.mkTriple roleActivityCarriedOutByUri SW.crmP14_1 roleUri

  forM_ personUriMaybe $ \personUri -> mapM_ addTriple
    $ RDF.mkTriple roleActivityCarriedOutByUri SW.crmP02 personUri

  forM_ legalBodyUriMaybe $ \legalBodyUri -> mapM_ addTriple
    $ RDF.mkTriple roleActivityCarriedOutByUri SW.crmP02 legalBodyUri

createDerivingWorkTriples
  :: (RDF.Rdf rdfImpl, Monad m)
  => Entity Filmo_Generique
  -> RdfState rdfImpl m ()
createDerivingWorkTriples filmoGeneriqueEntity = do
  let generiqueId = sqlKeyToText $ entityKey filmoGeneriqueEntity
  let filmoId =
        (sqlKeyToText . filmo_GeneriqueFilmoId . entityVal) filmoGeneriqueEntity

  let workUri        = baseUriPath <> "/Work" <> filmoId
  let workDerivedUri = workUri <> "-" <> generiqueId
  let workDerivedConceptionUri =
        baseUriPath <> "/WorkConception" <> filmoId <> "-" <> generiqueId

  let personUriMaybe =
        ( fmap (((baseUriPath <> "/Person") <>) . sqlKeyToText)
          . filmo_GeneriqueNomId
          . entityVal
          )
          filmoGeneriqueEntity

  let legalBodyUriMaybe =
        ( fmap (((baseUriPath <> "/LegalBody") <>) . sqlKeyToText)
          . filmo_GeneriqueOrganismeId
          . entityVal
          )
          filmoGeneriqueEntity

  forM_ (personUriMaybe <|> legalBodyUriMaybe) $ \agentUri -> do
    mapM_ addTriple $ RDF.mkTriple workUri SW.frbrooR2 workDerivedUri
    mapM_ addTriple
      $ RDF.mkTriple workDerivedUri SW.frbrooR16i workDerivedConceptionUri
    mapM_ addTriple $ RDF.mkTriple workDerivedConceptionUri SW.crmP14 agentUri
