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
module Data.CQLOD.Readers.CineTV.LangueLienWikidata
  ( readLangueLienWikidata
  )
where

import           Data.CQLOD                   (CQLODStatement (..),
                                               CQLODStatements, LanguageId (..),
                                               WikidataUri (..), addStatement)
import           Database.CineTv.Public.Model
import           Import                       hiding ((^.))
import           Util                         (sqlKeyToText)

import           Data.Pool                    (Pool)
import           Database.Esqueleto           hiding (get)

readLangueLienWikidata :: (MonadIO m) => Pool SqlBackend -> CQLODStatements m ()
readLangueLienWikidata pool = do
  langueLienWikidataEntities <-
    liftIO
    $ flip liftSqlPersistMPool pool
    $ select
    $ distinct
    $ from
    $ \langue -> return langue
  mapM_ createStatements langueLienWikidataEntities

createStatements
  :: (Monad m) => Entity Langue_LienWikidata -> CQLODStatements m ()
createStatements langueLienWikidataEntity = do
  let wikidataUriMaybe = WikidataUri <$> langue_LienWikidataLienWikidata
        (entityVal langueLienWikidataEntity)
  forM_ wikidataUriMaybe $ \wikidataUri -> do
    let langueId =
          LanguageId $ sqlKeyToText $ langue_LienWikidataLangueId $ entityVal
            langueLienWikidataEntity
    addStatement $ LanguageWikidataLink langueId wikidataUri
