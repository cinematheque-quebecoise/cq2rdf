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
module Data.CQLOD where

import           Import
import Data.RDF.Vocabulary

import           Control.Monad.State (StateT, get, put)
import           Data.Time.Clock              (UTCTime (..))

type CQLODStatements m a = StateT [CQLODStatement] m a

addStatement :: (Monad m) => CQLODStatement -> CQLODStatements m ()
addStatement statement = do
  s <- get
  put (statement : s)

newtype CQLOD = CQLOD { cqlodStatements :: [CQLODStatement] } deriving (Show)

data CQLODStatement = RoleDeclaration Role
                    | RoleWikidataLink RoleId WikidataUri

                    | GenreCategoryDeclaration GenreCategory
                    | GenreCategoryWikidataLink GenreCategoryId WikidataUri

                    | PersonDeclaration Person
                    | PersonWikidataLink PersonId WikidataUri

                    | LanguageDeclaration Language
                    | LanguageWikidataLink LanguageId WikidataUri

                    | LegalBodyDeclaration LegalBody

                    | PlaceDeclaration Place
                    | PlaceWikidataLink PlaceId WikidataUri

                    | SynopsisDeclaration Synopsis

                    | SynopsisTranslation SynopsisId SynopsisId

                    | WorkDeclaration WorkId
                    | WorkWikidataLink WorkId WikidataUri
                    | WorkSourcePerson WorkId WorkId PersonId
                    | WorkSourceLegalBody WorkId WorkId LegalBodyId
                    | WorkOriginalTitle WorkId Text
                    | WorkOtherTitle WorkId WorkTitleId Text
                    | WorkProductionCost WorkId Double Currency
                    | WorkGenreCategory WorkId GenreCategoryId
                    | WorkSynopsis WorkId SynopsisId

                    | RecordingWorkDeclaration RecordingWorkId
                    | RecordingWorkWorkDerivative RecordingWorkId WorkId

                    | RecordingEventDeclaration RecordingEventId
                    | RecordingEventLocation RecordingEventId PlaceId
                    | RecordingEventTimeSpan RecordingEventId TimeSpan
                    | RecordingEventRealisation RecordingEventId RecordingWorkId
                    | RecordingEventCreatedRecording RecordingEventId RecordingId
                    | RecordingEventActivity RecordingEventId RecordingActivityId

                    | RecordingActivityPersonDeclaration RecordingActivityId PersonId RoleId
                    | RecordingActivityLegalBodyDeclaration RecordingActivityId LegalBodyId RoleId

                    | RecordingDeclaration RecordingId
                    | RecordingLanguage RecordingId LanguageId

                    | PublicationExpressionDeclaration PublicationExpressionId
                    | PublicationExpressionIncorporatesExpression PublicationExpressionId RecordingId

                    | PublicationEventDeclaration PublicationEventId
                    | PublicationEventCreatedPublicationExpression PublicationEventId PublicationExpressionId

                    | PublicProjectionEventDeclaration PublicProjectionEventId
                    | PublicProjectionEventUsedPublicationExpression PublicProjectionEventId PublicationExpressionId
                    | PublicProjectionEventTimeSpan PublicProjectionEventId TimeSpan

                    | ManifestationProductTypeDeclaration ManifestationProductTypeId
                    | ManifestationProductTypeCarriesPublicationExpression ManifestationProductTypeId PublicationExpressionId
                    | ManifestationProductTypeDuration ManifestationProductTypeId Int -- ^ Duration in seconds

                    deriving (Eq, Show)

newtype PublicProjectionEventId = PublicProjectionEventId { unPublicProjectionEventId :: Text }
  deriving (Eq, Show)

newtype WorkId = WorkId { unWorkId :: Text } deriving (Eq, Show)

data Currency = CAD deriving (Eq, Show)

currencyToUri :: Currency -> Text
currencyToUri currency = case currency of
  CAD -> unitCAD

newtype RecordingWorkId = RecordingWorkId { unRecordingWorkId :: Text } deriving (Eq, Show)

data RecordingWorkDerivative = RecordingWorkDerivative
  { recordingWorkDerivativeRecordingWorkId :: RecordingWorkId
  , recordingWorkDerivativeWorkId          :: WorkId
  } deriving (Eq, Show)

newtype RecordingEventId = RecordingEventId { unRecordingEventId :: Text } deriving (Eq, Show)

newtype RecordingId = RecordingId { unRecordingId :: Text } deriving (Eq, Show)

newtype PublicationExpressionId = PublicationExpressionId { unPublicationExpressionId :: Text } deriving (Eq, Show)

newtype PublicationEventId = PublicationEventId { unPublicationEventId :: Text } deriving (Eq, Show)

newtype RoleId = RoleId { unRoleId :: Text } deriving (Eq, Show)

data Role = Role
  { roleId   :: RoleId
  , roleName :: Text
  } deriving (Eq, Show)

newtype PlaceId = PlaceId { unPlaceId :: Text } deriving (Eq, Show)

data Place = Place
  { placeId   :: PlaceId
  , placeName :: Text
  } deriving (Eq, Show)

data TimeSpan = TimeSpan
  { timeSpanBeginTime   :: Maybe UTCTime
  , timeSpanEndTime     :: Maybe UTCTime
  } deriving (Eq, Show)

newtype GenreCategoryId = GenreCategoryId { unGenreCategoryId :: Text } deriving (Eq, Show)

data GenreCategory = GenreCategory
  { genreCategoryId   :: GenreCategoryId
  , genreCategoryName :: Text
  } deriving (Eq, Show)

newtype PersonId = PersonId { unPersonId :: Text } deriving (Eq, Show)

data Person = Person
  { personId :: PersonId
  , personFirstName :: Maybe Text
  , personLastName :: Maybe Text
  } deriving (Eq, Show)

newtype LanguageId = LanguageId { unLanguageId :: Text } deriving (Eq, Show)

data Language = Language
  { languageId   :: LanguageId
  , languageName :: Text
  } deriving (Eq, Show)

newtype LegalBodyId = LegalBodyId { unLegalBodyId :: Text } deriving (Eq, Show)

data LegalBody = LegalBody
  { legalBodyId   :: LegalBodyId
  , legalBodyName :: Text
  } deriving (Eq, Show)

newtype SynopsisId = SynopsisId { unSynopsisId :: Text } deriving (Eq, Show)

data Synopsis = Synopsis
  { synopsisId   :: SynopsisId
  , synopsisText :: Text
  , synopsisLanguageId :: LanguageId
  } deriving (Eq, Show)

newtype RecordingActivityId = RecordingActivityId { unRecordingActivityId :: Text } deriving (Eq, Show)

newtype WorkTitleId = WorkTitleId { unWorkTitleId :: Text } deriving (Eq, Show)

newtype ManifestationProductTypeId = ManifestationProductTypeId { unManifestationProductTypeId :: Text } deriving (Eq, Show)

newtype WikidataUri = WikidataUri { unWikidataUri :: Text } deriving (Eq, Show)
