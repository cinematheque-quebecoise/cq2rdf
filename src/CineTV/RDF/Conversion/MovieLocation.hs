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
module CineTV.RDF.Conversion.MovieLocation
  ( convertMoviesLocation
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

{-|
Create all triples for representing the location of the recording event.

Generated triples:

@
for each row in table Filmo_Pays where Pays.PaysId = Filmo_Pays.PaysId
   cmtq:RecordingEvent{Filmo_Pays.FilmoId} crm:P7_took_place_at cmtq:Place{Filmo_Pays.PaysId}
@
-}
convertMoviesLocation
  :: (RDF.Rdf rdfImpl, MonadIO m) => Pool SqlBackend -> RdfState rdfImpl m ()
convertMoviesLocation = createTriplesFromMoviesLocation

createTriplesFromMoviesLocation
  :: (RDF.Rdf rdfImpl, MonadIO m) => Pool SqlBackend -> RdfState rdfImpl m ()
createTriplesFromMoviesLocation pool = do
  filmoPaysEntities <- getFilmoPaysEntities pool
  mapM_ createTriplesFromFilmoPays filmoPaysEntities

getFilmoPaysEntities :: (MonadIO m) => Pool SqlBackend -> m [Entity Filmo_Pays]
getFilmoPaysEntities pool =
  liftIO
    $ flip liftSqlPersistMPool pool
    $ select
    $ distinct
    $ from
    $ \(filmo, filmoPays, pays) -> do
        where_
          (   (pays ^. PaysId ==. filmoPays ^. Filmo_PaysPaysId)
          &&. (filmo ^. FilmoId ==. filmoPays ^. Filmo_PaysFilmoId)
          )
        return filmoPays

createTriplesFromFilmoPays
  :: (RDF.Rdf rdfImpl, Monad m) => Entity Filmo_Pays -> RdfState rdfImpl m ()
createTriplesFromFilmoPays filmoPaysEntity = do
  let filmoId = sqlKeyToText $ filmo_PaysFilmoId $ entityVal filmoPaysEntity
  let placeId = sqlKeyToText $ filmo_PaysPaysId $ entityVal filmoPaysEntity
  let recordingEventUri = baseUriPath <> "/RecordingEvent" <> filmoId
  let placeUri = baseUriPath <> "/Place" <> placeId

  mapM_ addTriple $ RDF.mkTriple recordingEventUri SW.crmP7 placeUri
