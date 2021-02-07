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
module Data.CQLOD.Readers.CineTV.PaysLienWikidata
  ( readPaysLienWikidata
  )
where

import           Data.CQLOD                   (CQLODStatement (..),
                                               CQLODStatements, PlaceId (..),
                                               WikidataUri (..), addStatement)
import           Database.CineTv.Public.Model
import           Import                       hiding ((^.))
import           Util                         (sqlKeyToText)

import           Data.Pool                    (Pool)
import           Database.Esqueleto           hiding (get)

readPaysLienWikidata :: (MonadIO m) => Pool SqlBackend -> CQLODStatements m ()
readPaysLienWikidata pool = do
  paysLienWikidataEntities <-
    liftIO
    $ flip liftSqlPersistMPool pool
    $ select
    $ distinct
    $ from
    $ \pays -> return pays
  mapM_ createStatements paysLienWikidataEntities

createStatements
  :: (Monad m) => Entity Pays_LienWikidata -> CQLODStatements m ()
createStatements paysLienWikidataEntity = do
  let wikidataUriMaybe = WikidataUri
        <$> pays_LienWikidataLienWikidata (entityVal paysLienWikidataEntity)
  forM_ wikidataUriMaybe $ \wikidataUri -> do
    let paysId = PlaceId $ sqlKeyToText $ pays_LienWikidataPaysId $ entityVal
          paysLienWikidataEntity
    addStatement $ PlaceWikidataLink paysId wikidataUri

