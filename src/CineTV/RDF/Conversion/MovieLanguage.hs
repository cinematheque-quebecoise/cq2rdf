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
module CineTV.RDF.Conversion.MovieLanguage
  ( convertMoviesLanguage
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
import           Database.Esqueleto           hiding (get)

baseUriPath :: Text
baseUriPath = "/resource"

{-|
Create all triples for representing the language of the work.

Generated triples:

@
for each row in Filmo_Langue
  cmtq:Recording{Filmo_Langue.FilmoId} crm:P72_has_language cmtq:Language{Filmo_Langue.LangueId}
@
-}
convertMoviesLanguage
  :: (RDF.Rdf rdfImpl, MonadIO m) => Pool SqlBackend -> RdfState rdfImpl m ()
convertMoviesLanguage = createTriplesFromMoviesLanguage

createTriplesFromMoviesLanguage
  :: (RDF.Rdf rdfImpl, MonadIO m) => Pool SqlBackend -> RdfState rdfImpl m ()
createTriplesFromMoviesLanguage pool = do
  filmoLanguageEntities <- getFilmoLangueEntities pool
  mapM_ createTriplesFromFilmoLangue filmoLanguageEntities

getFilmoLangueEntities
  :: (MonadIO m) => Pool SqlBackend -> m [Entity Filmo_Langue]
getFilmoLangueEntities pool =
  liftIO
    $ flip liftSqlPersistMPool pool
    $ select
    $ distinct
    $ from
    $ \(filmo, filmoLangue, langue) -> do
        where_
          (   (langue ^. LangueId ==. filmoLangue ^. Filmo_LangueLangueId)
          &&. (filmo ^. FilmoId ==. filmoLangue ^. Filmo_LangueFilmoId)
          )
        return filmoLangue

createTriplesFromFilmoLangue
  :: (RDF.Rdf rdfImpl, Monad m) => Entity Filmo_Langue -> RdfState rdfImpl m ()
createTriplesFromFilmoLangue filmoLangueEntity = do
  let filmoId =
        sqlKeyToText $ filmo_LangueFilmoId $ entityVal filmoLangueEntity
  let langueId =
        sqlKeyToText $ filmo_LangueLangueId $ entityVal filmoLangueEntity
  let recordingUri = baseUriPath <> "/Recording" <> filmoId
  let langueUri    = baseUriPath <> "/Language" <> langueId

  mapM_ addTriple $ RDF.mkTriple recordingUri crmP72 langueUri
