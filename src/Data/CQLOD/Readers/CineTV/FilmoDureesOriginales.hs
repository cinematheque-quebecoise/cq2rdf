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
module Data.CQLOD.Readers.CineTV.FilmoDureesOriginales
  ( readFilmoDureesOriginales
  )
where

import           Data.CQLOD                   (CQLODStatement (..),
                                               CQLODStatements,
                                               ManifestationProductTypeId (..),
                                               PublicationExpressionId (..),
                                               addStatement)
import           Database.CineTv.Public.Model
import           Import                       hiding ((^.))
import           Util                         (sqlKeyToText)

import           Data.Pool                    (Pool)
import           Database.Esqueleto           hiding (get)

readFilmoDureesOriginales
  :: (MonadIO m) => Pool SqlBackend -> CQLODStatements m ()
readFilmoDureesOriginales pool = do
  filmoDureesOriginalesEntities <-
    liftIO $ flip liftSqlPersistMPool pool $ select $ distinct $ from return
  mapM_ createStatements filmoDureesOriginalesEntities

createStatements
  :: (Monad m) => Entity FilmoDureesOriginales -> CQLODStatements m ()
createStatements filmoDureesOriginalesEntity = do
  let filmoDureesOriginales = entityVal filmoDureesOriginalesEntity

  let filmoId =
        sqlKeyToText $ filmoDureesOriginalesFilmoId filmoDureesOriginales

  let manifProductTypeId = ManifestationProductTypeId filmoId

  addStatement $ ManifestationProductTypeDeclaration manifProductTypeId

  let pubExprId = PublicationExpressionId filmoId
  addStatement $ ManifestationProductTypeCarriesPublicationExpression
    manifProductTypeId
    pubExprId

  let minutes      = filmoDureesOriginalesMinutes filmoDureesOriginales
  let seconds      = filmoDureesOriginalesSecondes filmoDureesOriginales
  let totalSeconds = minutes * 60 + seconds
  addStatement
    $ ManifestationProductTypeDuration manifProductTypeId totalSeconds
