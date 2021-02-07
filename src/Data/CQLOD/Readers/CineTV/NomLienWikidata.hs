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
module Data.CQLOD.Readers.CineTV.NomLienWikidata
  ( readNomLienWikidata
  )
where

import           Data.CQLOD                   (CQLODStatement (..),
                                               CQLODStatements, PersonId (..),
                                               WikidataUri (..), addStatement)
import           Database.CineTv.Public.Model
import           Import                       hiding ((^.))
import           Util                         (sqlKeyToText)

import           Data.Pool                    (Pool)
import           Database.Esqueleto           hiding (get)

readNomLienWikidata :: (MonadIO m) => Pool SqlBackend -> CQLODStatements m ()
readNomLienWikidata pool = do
  nomLienWikidataEntities <-
    liftIO
    $ flip liftSqlPersistMPool pool
    $ select
    $ distinct
    $ from
    $ \nom -> return nom
  mapM_ createStatements nomLienWikidataEntities

createStatements
  :: (Monad m) => Entity Nom_LienWikidata -> CQLODStatements m ()
createStatements nomLienWikidataEntity = do
  let wikidataUriMaybe = WikidataUri <$> nom_LienWikidataLienWikidata
        (entityVal nomLienWikidataEntity)
  forM_ wikidataUriMaybe $ \wikidataUri -> do
    let nomId =
          PersonId $ sqlKeyToText $ nom_LienWikidataNomId $ entityVal
            nomLienWikidataEntity
    addStatement $ PersonWikidataLink nomId wikidataUri
