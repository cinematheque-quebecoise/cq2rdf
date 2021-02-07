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
module CineTV.RDF.Conversion.MovieDirector
  ( convertMoviesDirector
  )
where

import qualified Data.RDF.Types.Extended      as RDF (mkTriple)
import           Database.CineTv.Public.Model
import           Import                       hiding ((^.))
import Data.RDF.Vocabulary
import           Util                         (sqlKeyToText)

import           Data.Pool                    (Pool)
import qualified Data.RDF                     as RDF
import           Data.RDF.State
import qualified Data.Text                    as Text
import           Database.Esqueleto           hiding (get)

baseUriPath :: Text
baseUriPath = "/resource"

-- | The director role does not exist in CineTV database.
-- Has to be manually created.
customDirectorRoleId :: Int64
customDirectorRoleId = 1

{-|
Create all triples for representing the director of the movie.

Generated triples:

@
for each row in table Filmo_Realisation
   cmtq:RecordingEvent{Filmo_Realisation.FilmoId} crm:P9_consists_of cmtq:RecordingActivityDirector{Filmo_Realisation.FilmoId}-{Filmo_Realisation.NomId}
   cmtq:RecordingActivityCarriedOutByDirector{Filmo_Realisation.FilmoId}-{Filmo_Realisation.NomId} crm:P01_has_domain cmtq:RecordingActivityDirector{Filmo_Realisation.FilmoId}-{Filmo_Realisation.NomId}
  cmtq:RecordingActivityCarriedOutByDirector{Filmo_Realisation.FilmoId}-{Filmo_Realisation.NomId} crm:P02_has_range cmtq:Person{Filmo_Realisation.NomId}
  cmtq:RecordingActivityCarriedOutByDirector{Filmo_Realisation.FilmoId}-{Filmo_Realisation.NomId} crm:P14.1_in_the_role_of cmtq:Role1
@
-}
convertMoviesDirector
  :: (RDF.Rdf rdfImpl, MonadIO m) => Pool SqlBackend -> RdfState rdfImpl m ()
convertMoviesDirector = createTriplesFromMoviesDirector

createTriplesFromMoviesDirector
  :: (RDF.Rdf rdfImpl, MonadIO m) => Pool SqlBackend -> RdfState rdfImpl m ()
createTriplesFromMoviesDirector pool = do
  filmoRealisationEntities <- getFilmoRealisationEntities pool
  mapM_ createTriplesFromFilmoRealisation filmoRealisationEntities

getFilmoRealisationEntities
  :: (MonadIO m) => Pool SqlBackend -> m [Entity Filmo_Realisation]
getFilmoRealisationEntities pool =
  liftIO
    $ flip liftSqlPersistMPool pool
    $ select
    $ distinct
    $ from
    $ \(filmo, filmoRealisation, nom) -> do
        where_
          (   (nom ^. NomId ==. filmoRealisation ^. Filmo_RealisationNomId)
          &&. (   filmo
              ^.  FilmoId
              ==. filmoRealisation
              ^.  Filmo_RealisationFilmoId
              )
          )
        return filmoRealisation

createTriplesFromFilmoRealisation
  :: (RDF.Rdf rdfImpl, Monad m)
  => Entity Filmo_Realisation
  -> RdfState rdfImpl m ()
createTriplesFromFilmoRealisation filmoRealisationEntity = do
  let filmoId = (sqlKeyToText . filmo_RealisationFilmoId . entityVal)
        filmoRealisationEntity
  let nomId = (sqlKeyToText . filmo_RealisationNomId . entityVal)
        filmoRealisationEntity

  let recordingEventUri = baseUriPath <> "/RecordingEvent" <> filmoId
  let roleActivityUri =
        baseUriPath <> "/RecordingActivityDirector" <> filmoId <> "-" <> nomId
  let roleActivityCarriedOutByUri =
        baseUriPath
          <> "/RecordingActivityCarriedOutByDirector"
          <> filmoId
          <> "-"
          <> nomId
  let roleUri = baseUriPath <> "/Role" <> Text.pack (show customDirectorRoleId)
  let personUri = baseUriPath <> "/Person" <> nomId

  mapM_ addTriple $ RDF.mkTriple recordingEventUri crmP9 roleActivityUri
  mapM_ addTriple
    $ RDF.mkTriple roleActivityCarriedOutByUri crmP01 roleActivityUri
  mapM_ addTriple $ RDF.mkTriple roleActivityCarriedOutByUri crmP14_1 roleUri
  mapM_ addTriple $ RDF.mkTriple roleActivityCarriedOutByUri crmP02 personUri
