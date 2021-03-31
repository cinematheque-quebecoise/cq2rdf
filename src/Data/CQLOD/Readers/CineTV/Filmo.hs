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
module Data.CQLOD.Readers.CineTV.Filmo
  ( readFilmo
  )
where

import           Data.CQLOD
import           Database.CineTv.Public.Model
import           Import                       hiding ((^.))
import           Util                         (parseDateField, parseYearField,
                                               sqlKeyToText)

import           Data.Pool                    (Pool)
import qualified Data.Text                    as T
import           Database.Esqueleto           hiding (get)

readFilmo :: (MonadIO m) => Pool SqlBackend -> CQLODStatements m ()
readFilmo pool = do
  filmo <-
    liftIO
    $ flip liftSqlPersistMPool pool
    $ select
    $ distinct
    $ from
    $ \filmo -> return filmo
  mapM_ addStatements filmo

addStatements :: (Monad m) => Entity Filmo -> CQLODStatements m ()
addStatements filmoEntity = do
  let filmoId = sqlKeyToText $ entityKey filmoEntity
  let workId     = WorkId filmoId

  addStatement $ WorkDeclaration workId
  let recordingWorkId = RecordingWorkId filmoId
  addStatement $ RecordingWorkDeclaration recordingWorkId
  addStatement $ RecordingWorkWorkDerivative recordingWorkId workId

  let recordingEventId = RecordingEventId filmoId
  addStatement $ RecordingEventDeclaration recordingEventId
  addStatement $ RecordingEventRealisation recordingEventId recordingWorkId

  let recordingId = RecordingId filmoId
  addStatement $ RecordingDeclaration recordingId
  addStatement $ RecordingEventCreatedRecording recordingEventId recordingId

  let publicationExpressionId = PublicationExpressionId filmoId
  addStatement $ PublicationExpressionDeclaration publicationExpressionId

  addStatement
    $ PublicationExpressionIncorporatesExpression publicationExpressionId recordingId

  let peventid = PublicationEventId filmoId
  addStatement $ PublicationEventDeclaration peventid
  addStatement
    $ PublicationEventCreatedPublicationExpression peventid publicationExpressionId

  let filmo = entityVal filmoEntity

  addWorkOriginalTitleStatement workId filmo

  addWorkProductionCostStatement workId filmo

  addPublicProjectionEventStatements filmoId filmo

  addRecordingEventTimeSpanStatement recordingEventId filmo

addWorkOriginalTitleStatement
  :: (Monad m) => WorkId -> Filmo -> CQLODStatements m ()
addWorkOriginalTitleStatement workId filmo = do
  let titleMaybe = mkOriginalTitleWork filmo
  forM_ titleMaybe $ \title -> do
    let workOriginalTitleStatement = WorkOriginalTitle workId title
    addStatement workOriginalTitleStatement

addWorkProductionCostStatement
  :: (Monad m) => WorkId -> Filmo -> CQLODStatements m ()
addWorkProductionCostStatement workId filmo = do
  forM_ (filmoCout filmo) $ \movieCost -> do
    addStatement $ WorkProductionCost
      workId
      (fromIntegral movieCost)
      CAD

mkOriginalTitleWork :: Filmo -> Maybe Text
mkOriginalTitleWork filmo = do
  restTitle <- filmoTitreOriginal filmo
  case filmoPrefixeTitreOriginal filmo of
    Just prefixTitle -> if T.isSuffixOf "'" prefixTitle
      then Just $ prefixTitle <> "" <> restTitle
      else Just $ prefixTitle <> " " <> restTitle
    Nothing -> Just restTitle

addPublicProjectionEventStatements
  :: (Monad m) => Text -> Filmo -> CQLODStatements m ()
addPublicProjectionEventStatements filmoId filmo = do
  let releaseYear = filmoAnneeSortie filmo >>= parseYearField
  let ppeventid   = PublicProjectionEventId filmoId
  addStatement $ PublicProjectionEventDeclaration ppeventid
  addStatement
    $ PublicProjectionEventTimeSpan ppeventid (TimeSpan releaseYear Nothing)

  let pubExprId   = PublicationExpressionId filmoId
  addStatement $ PublicProjectionEventUsedPublicationExpression ppeventid pubExprId

addRecordingEventTimeSpanStatement
  :: (Monad m) => RecordingEventId -> Filmo -> CQLODStatements m ()
addRecordingEventTimeSpanStatement peventid filmo = do
  let dateBeginMaybe     = filmoDateDebProd filmo >>= parseDateField
  let dateEndMaybe       = filmoDateFinProd filmo >>= parseDateField
  let yearBeginMaybe     = filmoAnneeDebProd filmo >>= parseYearField
  let yearEndMaybe       = filmoAnneeFinProd filmo >>= parseYearField
  let beginDateTimeMaybe = dateBeginMaybe <|> yearBeginMaybe
  let endDateTimeMaybe   = dateEndMaybe <|> yearEndMaybe
  addStatement $ RecordingEventTimeSpan
    peventid
    (TimeSpan beginDateTimeMaybe endDateTimeMaybe)
