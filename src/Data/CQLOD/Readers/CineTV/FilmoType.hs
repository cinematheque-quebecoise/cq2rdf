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
module Data.CQLOD.Readers.CineTV.FilmoType
  ( readFilmoType
  )
where

import           Data.CQLOD                   (CQLODStatement (..),
                                               CQLODStatements, WorkId (..),
                                               WorkTitleId (..), WorkType (..),
                                               addStatement)
import           Database.CineTv.Public.Model
import           Import                       hiding ((^.))
import           Util                         (sqlKeyToText)

import           Control.Monad.Trans.Maybe
import           Data.Pool                    (Pool)
import qualified Data.Text                    as T
import           Database.Esqueleto           hiding (get)

data FilmoType = FilmoType { filmoTypeFilmoId  :: FilmoId
                           , filmoTypeWorkType :: WorkType
                           }

readFilmoType :: (MonadIO m) => Pool SqlBackend -> CQLODStatements m ()
readFilmoType pool = do
  filmoEntities <-
    liftIO $ flip liftSqlPersistMPool pool $ select $ distinct $ from return
  -- mapM_ ((runMaybeT . createFilmoType pool) >=> createStatements) filmoEntities
  forM_ filmoEntities $ \filmoEntity -> do
    filmoTypeMaybe <- runMaybeT $ createFilmoType pool filmoEntity
    forM_ filmoTypeMaybe createStatements

createFilmoType
  :: (MonadIO m) => Pool SqlBackend -> Entity Filmo -> MaybeT m FilmoType
createFilmoType pool filmoEntity = do
  MaybeT (return $ createFilmoUniqueWork filmoEntity)
    <|> createFilmoSeries pool filmoEntity
    <|> createFilmoSeason filmoEntity
    <|> createFilmoSeriesByNumEpisodes pool filmoEntity
    <|> createFilmoEpisode pool filmoEntity

createFilmoUniqueWork :: Entity Filmo -> Maybe FilmoType
createFilmoUniqueWork filmoEntity = do
  natureDeLaProductionId <- filmoNatureDeLaProductionId $ entityVal filmoEntity
  case sqlKeyToText natureDeLaProductionId of
    "1" -> Just $ FilmoType (entityKey filmoEntity) UniqueWork
    _   -> Nothing

createFilmoSeries
  :: (MonadIO m) => Pool SqlBackend -> Entity Filmo -> MaybeT m FilmoType
createFilmoSeries pool filmoEntity = MaybeT $ do
  let tvSeriesIds =
        [ 4420
        , 7456
        , 10623
        , 10625
        , 10626
        , 13120
        , 16257
        , 16258
        , 20053
        , 20054
        , 20056
        , 20058
        ]

  filmoSeries <-
    liftIO
    $ flip liftSqlPersistMPool pool
    $ select
    $ distinct
    $ from
    $ \(filmo, filmoGenresCategories, sujet) -> do
        where_
          (   (   filmo
              ^.  FilmoId
              ==. filmoGenresCategories
              ^.  Filmo_GenresCategoriesFilmoId
              )
          &&. (   filmoGenresCategories
              ^.  Filmo_GenresCategoriesSujetId
              ==. sujet
              ^.  SujetId
              )
          &&. (filmo ^. FilmoId ==. val (entityKey filmoEntity))
          &&. (sujet ^. SujetId `in_` valList (toSqlKey <$> tvSeriesIds))
          )
        return filmo

  if not (null filmoSeries)
    then return (Just $ FilmoType (entityKey filmoEntity) TelevisionSeries)
    else return Nothing

createFilmoSeason :: (MonadIO m) => Entity Filmo -> MaybeT m FilmoType
createFilmoSeason filmoEntity = MaybeT $ do
  let titleMaybe = filmoTitreOriginal $ entityVal filmoEntity

  return $ do
    title <- titleMaybe
    if T.isInfixOf "[" title && T.isInfixOf "]" title
      then Just $ FilmoType (entityKey filmoEntity) TelevisionSeriesSeason
      else Nothing

createFilmoSeriesByNumEpisodes :: (MonadIO m) => Pool SqlBackend -> Entity Filmo -> MaybeT m FilmoType
createFilmoSeriesByNumEpisodes pool filmoEntity = MaybeT $ do
  filmoSeason <-
    liftIO
    $ flip liftSqlPersistMPool pool
    $ select
    $ distinct
    $ from
    $ \(filmo, filmoNombresEpisodes) -> do
        where_
          (   (   filmo
              ^.  FilmoId
              ==. filmoNombresEpisodes
              ^.  FilmoNombresEpisodesFilmoId
              )
          &&. (filmo ^. FilmoId ==. val (entityKey filmoEntity))
          )
        return filmo

  if not (null filmoSeason)
    then return (Just $ FilmoType (entityKey filmoEntity) TelevisionSeries)
    else return Nothing

createFilmoEpisode :: (MonadIO m) => Pool SqlBackend -> Entity Filmo -> MaybeT m FilmoType
createFilmoEpisode pool filmoEntity = MaybeT $ do
  filmoEpisode <-
    liftIO
    $ flip liftSqlPersistMPool pool
    $ select
    $ distinct
    $ from
    $ \(filmo, filmoNombresEpisodes) -> do
        where_
          (   (   filmo
              ^.  FilmoId
              ==. filmoNombresEpisodes
              ^.  FilmoDureesEpisodesFilmoId
              )
          &&. (filmo ^. FilmoId ==. val (entityKey filmoEntity))
          )
        return filmo

  if not (null filmoEpisode)
    then return (Just $ FilmoType (entityKey filmoEntity) TelevisionSeriesEpisode)
    else return Nothing

createStatements :: (Monad m) => FilmoType -> CQLODStatements m ()
createStatements filmoType = addStatement $ WorkTypeDeclaration
  (WorkId $ sqlKeyToText $ filmoTypeFilmoId filmoType)
  (filmoTypeWorkType filmoType)
