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
{-# LANGUAGE OverloadedStrings #-}
module Data.CQLOD.Readers.CineTV.FilmoTitres
  ( readFilmoTitres
  )
where

import           Data.CQLOD                   (CQLODStatement (..),
                                               CQLODStatements, WorkId (..),
                                               WorkTitleId (..), addStatement)
import           Database.CineTv.Public.Model
import           Import                       hiding ((^.))
import           Util                         (sqlKeyToText)

import           Data.Pool                    (Pool)
import qualified Data.Text                    as T
import           Database.Esqueleto           hiding (get)

readFilmoTitres :: (MonadIO m) => Pool SqlBackend -> CQLODStatements m ()
readFilmoTitres pool = do
  filmoTitresEntities <-
    liftIO $ flip liftSqlPersistMPool pool $ select $ distinct $ from return
  mapM_ createStatements filmoTitresEntities

createStatements :: (Monad m) => Entity FilmoTitres -> CQLODStatements m ()
createStatements filmoTitresEntity = do
  let filmoTitreId = sqlKeyToText $ entityKey filmoTitresEntity
  let filmoTitres  = entityVal filmoTitresEntity
  let filmoId      = sqlKeyToText $ filmoTitresFilmoId filmoTitres
  let workTitle    = mkTitleWork filmoTitres
  addStatement
    $ WorkOtherTitle (WorkId filmoId) (WorkTitleId filmoTitreId) workTitle

mkTitleWork :: FilmoTitres -> Text
mkTitleWork filmoTitres = do
  let restTitle = filmoTitresTitre filmoTitres
  case filmoTitresPrefixe filmoTitres of
    Just prefixTitle -> if T.isSuffixOf "'" prefixTitle
      then prefixTitle <> "" <> restTitle
      else prefixTitle <> " " <> restTitle
    Nothing -> restTitle
