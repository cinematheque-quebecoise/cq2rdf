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
module Data.CQLOD.Writers.RDFSpec
  ( spec
  )
where

import           Data.CQLOD
import           Data.CQLOD.Writers.RDF
import           Data.RDF.Types.Extended          (mkTriple, mkTripleLit)
import           Import
import           Util                    (utcTimeToText)

import           Data.RDF
import           Data.RDF.Vocabulary
import qualified Data.Text                        as T
import           Test.Hspec
import           Test.Hspec.Expectations.Extended (shouldContainElems)
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Instances.Text
import Data.Time

newtype EntityId = EntityId { unEntityId :: Text } deriving (Show)

instance Arbitrary EntityId where
  arbitrary = EntityId <$> (T.pack <$> uriPart)

newtype RoleWrapper = RoleWrapper { unRoleWrapper :: Role } deriving (Show)

instance Arbitrary RoleWrapper where
  arbitrary =
    RoleWrapper
      <$> (Role <$> (RoleId . T.pack <$> uriPart) <*> (T.pack <$> arbitrary))

newtype WikidataUriWrapper = WikidataUriWrapper { unWikidataUriWrapper :: WikidataUri } deriving (Show)

instance Arbitrary WikidataUriWrapper where
  arbitrary = WikidataUriWrapper <$> (WikidataUri <$> (T.pack <$> uriPart))

newtype UTCTimeWrapper = UTCTimeWrapper { unUTCTimeWrapper :: UTCTime } deriving (Show)

instance Arbitrary UTCTimeWrapper where
  arbitrary = do
    randomDay <- choose (1, 29) :: Gen Int
    randomMonth <- choose (1, 12) :: Gen Int
    randomYear <- choose (2001, 2002) :: Gen Integer
    randomTime <- choose (0, 86401) :: Gen Int
    return $ UTCTimeWrapper $ UTCTime (fromGregorian randomYear randomMonth randomDay) (fromIntegral randomTime)

newtype WorkTypeWrapper = WorkTypeWrapper { unWorkTypeWrapper :: WorkType } deriving (Show)

instance Arbitrary WorkTypeWrapper where
  arbitrary = WorkTypeWrapper <$> elements [UniqueWork, TelevisionSeries, TelevisionSeriesSeason, TelevisionSeriesEpisode]

alpha :: Gen Char
alpha = elements $ ['a' .. 'z'] ++ ['A' .. 'Z']

digit :: Gen Char
digit = elements ['0' .. '9']

uriPart :: Gen String
uriPart = listOf $ oneof [alpha, digit, elements ['+', '-', '.']]

spec :: Spec
spec = do
  describe "writeRoleDeclaration" $ do
    prop "basic check" $ \(roleEntityId, rlabel) -> do
      let rid               = unEntityId roleEntityId
      let role              = Role (RoleId rid) rlabel
      let triples           = toTriples $ RoleDeclaration role

      let roleUri           = mkRoleUri rid
      let roleIdentifierUri = mkRoleIdentifierUri rid

      triples `shouldContainElems` catMaybes
        [ mkTriple roleUri rdfType crmE55
        , mkTripleLit roleUri rdfsLabel (PlainLL (roleName role) "fr")
        , mkTriple roleUri crmP2  roleTypeUri
        , mkTriple roleUri crmP48 roleIdentifierUri
        , mkTripleLit roleIdentifierUri crmP190 (PlainL rid)
        ]

  describe "writeRoleWikidataLink" $ do
    prop "basic check" $ \(roleEntityId, uri) -> do
      let rId         = RoleId (unEntityId roleEntityId)
      let wikidataUri = WikidataUri (unEntityId uri)
      let triples     = toTriples $ RoleWikidataLink rId wikidataUri

      let roleUri     = mkRoleUri $ unRoleId rId
      triples `shouldContainElems` catMaybes
        [mkTriple roleUri owlSameAs (unWikidataUri wikidataUri)]
      length triples `shouldBe` 1

  describe "writePersonDeclaration" $ do
    prop "basic check" $ \(personEntityId, firstname, lastname) -> do
      let person = Person (PersonId $ unEntityId personEntityId)
                          (Just firstname)
                          (Just lastname)
      let triples   = toTriples $ PersonDeclaration person

      let personUri = mkPersonUri $ unPersonId $ personId person
      let personIdentifierUri =
            mkPersonIdentifierUri $ unPersonId $ personId person
      let personAppellationUri =
            mkPersonAppellationUri $ unPersonId $ personId person
      triples `shouldContainElems` catMaybes
        [ mkTriple personUri rdfType crmE21
        , mkTriple personUri crmP48  personIdentifierUri
        , mkTriple personUri crmP1   personAppellationUri
        , mkTripleLit personUri foafGivenName (PlainL firstname)
        , mkTripleLit personUri foafFamilyName (PlainL lastname)
        , mkTripleLit personUri foafName (PlainL $ firstname <> " " <> lastname)
        , mkTripleLit personIdentifierUri
                      crmP190
                      (PlainL $ unPersonId $ personId person)
        , mkTripleLit personAppellationUri
                      crmP190
                      (PlainL $ firstname <> " " <> lastname)
        ]

    prop "empty firstname should not create foaf:givenName triple"
      $ \(personEntityId, lastname) -> do
          let
            person = Person (PersonId $ unEntityId personEntityId)
                            Nothing
                            (Just lastname)
          let triples   = toTriples $ PersonDeclaration person

          let personUri = mkPersonUri $ unPersonId $ personId person
          triples `shouldContainElems` catMaybes
            [mkTripleLit personUri foafName (PlainL lastname)]
          length (filter (hasPredicate foafGivenName) triples) `shouldBe` 0

    prop "empty lastname should not create foaf:familyName triple"
      $ \(personEntityId, firstname) -> do
          let
            person = Person (PersonId $ unEntityId personEntityId)
                            (Just firstname)
                            Nothing
          let triples   = toTriples $ PersonDeclaration person

          let personUri = mkPersonUri $ unPersonId $ personId person
          triples `shouldContainElems` catMaybes
            [mkTripleLit personUri foafName (PlainL firstname)]
          length (filter (hasPredicate foafFamilyName) triples) `shouldBe` 0

    prop
        "empty lastname and firstname should not create any appellation triples"
      $ \personEntityId -> do
          let person =
                Person (PersonId $ unEntityId personEntityId) Nothing Nothing
          let triples = toTriples $ PersonDeclaration person

          length (filter (hasPredicate foafFamilyName) triples) `shouldBe` 0
          length (filter (hasPredicate foafGivenName) triples) `shouldBe` 0
          length (filter (hasPredicate foafName) triples) `shouldBe` 0
          length (filter (hasPredicate crmP1) triples) `shouldBe` 0

  describe "writePersonWikidataLink" $ do
    prop "basic check" $ \(entityId, uri) -> do
      let pId         = PersonId (unEntityId entityId)
      let wikidataUri = WikidataUri (unEntityId uri)
      let triples     = toTriples $ PersonWikidataLink pId wikidataUri

      let personUri     = mkPersonUri $ unPersonId pId
      triples `shouldContainElems` catMaybes
        [mkTriple personUri owlSameAs (unWikidataUri wikidataUri)]
      length triples `shouldBe` 1

  describe "writeGenreCategoryDeclaration" $ do
    prop "basic check" $ \(entityId, entityName) -> do
      let eid               = unEntityId entityId
      let genreCategory              = GenreCategory (GenreCategoryId eid) entityName
      let triples           = toTriples $ GenreCategoryDeclaration genreCategory

      let genreUri           = mkGenreCategoryUri eid
      let genreAppellationUri = mkGenreCategoryAppellationUri eid
      let genreIdentifierUri = mkGenreCategoryIdentifierUri eid

      triples `shouldContainElems` catMaybes
        [ mkTriple genreUri rdfType crmE55
        , mkTripleLit genreUri rdfsLabel (PlainLL entityName "fr")
        , mkTriple genreUri crmP2 genreCategoryTypeUri
        , mkTriple genreUri crmP48 genreIdentifierUri
        , mkTriple genreUri crmP1 genreAppellationUri
        , mkTripleLit genreIdentifierUri crmP190 (PlainL eid)
        , mkTripleLit genreAppellationUri crmP190 (PlainL entityName)
        ]

  describe "writeGenreCategoryWikidataLink" $ do
    prop "basic check" $ \(entityId, uri) -> do
      let eId         = GenreCategoryId (unEntityId entityId)
      let wikidataUri = WikidataUri (unEntityId uri)
      let triples     = toTriples $ GenreCategoryWikidataLink eId wikidataUri

      let genreUri     = mkGenreCategoryUri $ unGenreCategoryId eId
      triples `shouldContainElems` catMaybes
        [mkTriple genreUri owlSameAs (unWikidataUri wikidataUri)]
      length triples `shouldBe` 1

  describe "writeLanguageDeclaration" $ do
    prop "basic check" $ \(entityId, entityName) -> do
      let eid               = unEntityId entityId
      let language              = Language (LanguageId eid) entityName
      let triples           = toTriples $ LanguageDeclaration language

      let languageUri           = mkLanguageUri eid
      let languageAppellationUri = mkLanguageAppellationUri eid
      let languageIdentifierUri = mkLanguageIdentifierUri eid

      triples `shouldContainElems` catMaybes
        [ mkTriple languageUri rdfType crmE56
        , mkTripleLit languageUri rdfsLabel (PlainLL entityName "fr")
        , mkTriple languageUri crmP48 languageIdentifierUri
        , mkTriple languageUri crmP1 languageAppellationUri
        , mkTripleLit languageIdentifierUri crmP190 (PlainL eid)
        , mkTripleLit languageAppellationUri crmP190 (PlainL entityName)
        ]

  describe "writeLanguageWikidataLink" $ do
    prop "basic check" $ \(entityId, uri) -> do
      let eId         = LanguageId (unEntityId entityId)
      let wikidataUri = WikidataUri (unEntityId uri)
      let triples     = toTriples $ LanguageWikidataLink eId wikidataUri

      let languageUri     = mkLanguageUri $ unLanguageId eId
      triples `shouldContainElems` catMaybes
        [mkTriple languageUri owlSameAs (unWikidataUri wikidataUri)]
      length triples `shouldBe` 1

  describe "writeLegalBodyDeclaration" $ do
    prop "basic check" $ \(entityId, entityName) -> do
      let eid               = unEntityId entityId
      let language              = LegalBody (LegalBodyId eid) entityName
      let triples           = toTriples $ LegalBodyDeclaration language

      let legalBodyUri           = mkLegalBodyUri eid
      let legalBodyAppellationUri = mkLegalBodyAppellationUri eid
      let legalBodyIdentifierUri = mkLegalBodyIdentifierUri eid

      triples `shouldContainElems` catMaybes
        [ mkTriple legalBodyUri rdfType crmE40
        , mkTripleLit legalBodyUri rdfsLabel (PlainL entityName)
        , mkTriple legalBodyUri crmP48 legalBodyIdentifierUri
        , mkTriple legalBodyUri crmP1 legalBodyAppellationUri
        , mkTripleLit legalBodyIdentifierUri crmP190 (PlainL eid)
        , mkTripleLit legalBodyAppellationUri crmP190 (PlainL entityName)
        ]

  describe "writePlaceDeclaration" $ do
    prop "basic check" $ \(entityId, entityName) -> do
      let eid               = unEntityId entityId
      let place              = Place (PlaceId eid) entityName
      let triples           = toTriples $ PlaceDeclaration place

      let placeUri           = mkPlaceUri eid
      let placeAppellationUri = mkPlaceAppellationUri eid
      let placeIdentifierUri = mkPlaceIdentifierUri eid

      triples `shouldContainElems` catMaybes
        [ mkTriple placeUri rdfType crmE53
        , mkTripleLit placeUri rdfsLabel (PlainLL entityName "fr")
        , mkTriple placeUri crmP48 placeIdentifierUri
        , mkTriple placeUri crmP1 placeAppellationUri
        , mkTripleLit placeIdentifierUri crmP190 (PlainL eid)
        , mkTripleLit placeAppellationUri crmP190 (PlainL entityName)
        ]

  describe "writePlaceWikidataLink" $ do
    prop "basic check" $ \(entityId, uri) -> do
      let eId         = PlaceId (unEntityId entityId)
      let wikidataUri = WikidataUri (unEntityId uri)
      let triples     = toTriples $ PlaceWikidataLink eId wikidataUri

      let placeUri     = mkPlaceUri $ unPlaceId eId
      triples `shouldContainElems` catMaybes
        [mkTriple placeUri owlSameAs (unWikidataUri wikidataUri)]
      length triples `shouldBe` 1

  describe "writeWorkDeclaration" $ do
    prop "basic check" $ \entityId -> do
      let eid               = unEntityId entityId
      let workId              = WorkId eid
      let triples           = toTriples $ WorkDeclaration workId

      triples `shouldContainElems` catMaybes
        [ mkTriple (mkWorkUri eid) rdfType frbrooF1
        ]

  describe "writeWorkWikidataLink" $ do
    prop "basic check" $ \(entityId, uri) -> do
      let eId         = WorkId (unEntityId entityId)
      let wikidataUri = WikidataUri (unEntityId uri)
      let triples     = toTriples $ WorkWikidataLink eId wikidataUri

      let workUri     = mkWorkUri $ unWorkId eId
      triples `shouldContainElems` catMaybes
        [mkTriple workUri owlSameAs (unWikidataUri wikidataUri)]
      length triples `shouldBe` 1

  describe "writeWorkOtherTitle" $ do
    prop "basic check" $ \(entityId, entityId2, title) -> do
      let workId              = WorkId $ unEntityId entityId
      let workTitleId              = WorkTitleId $ unEntityId entityId2
      let triples           = toTriples $ WorkOtherTitle workId workTitleId title

      let workUri = "/resource/Work" <> unEntityId entityId
      let workTitleUri = "/resource/WorkTitle" <> unEntityId entityId2

      triples `shouldContainElems` catMaybes
        [ mkTriple workUri crmP102 workTitleUri
        , mkTriple workTitleUri rdfType crmE35
        , mkTripleLit workTitleUri crmP190 (PlainL title)
        ]

  describe "writeWorkType" $ do
    prop "basic check" $ \(entityId, workTypeWrapper) -> do
      let workId              = WorkId $ unEntityId entityId
      let workType              = unWorkTypeWrapper workTypeWrapper
      let triples           = toTriples $ WorkTypeDeclaration workId workType

      let workUri = "/resource/Work" <> unEntityId entityId
      let workTypeUri = "/resource/" <> T.pack (show workType)

      triples `shouldContainElems` catMaybes
        [ mkTriple workUri crmP2 workTypeUri
        ]

  describe "writeWorkSourcePerson" $ do
    prop "basic check" $ \(entityId, entityId2, entityId3) -> do
      let workId              = unEntityId entityId
      let derivedWorkId       = unEntityId entityId2
      let pId = unEntityId entityId3
      let triples           = toTriples $ WorkSourcePerson (WorkId workId) (WorkId derivedWorkId) (PersonId pId)

      let derivedWorkUri = "/resource/Work" <> derivedWorkId
      let derivedWorkConceptionUri = "/resource/WorkConception" <> derivedWorkId
      let personUri = "/resource/Person" <> pId

      triples `shouldContainElems` catMaybes
        [ mkTriple (mkWorkUri workId) frbrooR2 derivedWorkUri
        , mkTriple derivedWorkUri frbrooR16i derivedWorkConceptionUri
        , mkTriple derivedWorkConceptionUri crmP14 personUri
        ]

  describe "writeWorkSourceLegalBody" $ do
    prop "basic check" $ \(entityId, entityId2, entityId3) -> do
      let workId              = unEntityId entityId
      let derivedWorkId       = unEntityId entityId2
      let pId = unEntityId entityId3
      let triples           = toTriples $ WorkSourceLegalBody (WorkId workId) (WorkId derivedWorkId) (LegalBodyId pId)

      let derivedWorkUri = "/resource/Work" <> derivedWorkId
      let derivedWorkConceptionUri = "/resource/WorkConception" <> derivedWorkId
      let legalBodyUri = "/resource/LegalBody" <> pId

      triples `shouldContainElems` catMaybes
        [ mkTriple (mkWorkUri workId) frbrooR2 derivedWorkUri
        , mkTriple derivedWorkUri frbrooR16i derivedWorkConceptionUri
        , mkTriple derivedWorkConceptionUri crmP14 legalBodyUri
        ]

  describe "writeWorkProductionCost" $ do
    prop "basic check" $ \(entityId, amount) -> do
      let workId              = unEntityId entityId
      let triples           = toTriples $ WorkProductionCost (WorkId workId) amount CAD

      let workUri = "/resource/Work" <> workId
      let dimensionLabel = T.pack (show amount) <> "CAD"
      let dimensionUri = "/resource/" <> "Dimension" <> dimensionLabel
      triples `shouldContainElems` catMaybes
        [ mkTriple workUri crmP43 dimensionUri
        , mkTriple dimensionUri rdfType crmE54
        , mkTriple dimensionUri crmP2 budgetTypeUri
        , mkTripleLit dimensionUri rdfsLabel (PlainL dimensionLabel)
        , mkTripleLit dimensionUri crmP181   (TypedL (T.pack $ show amount) xsdDouble)
        , mkTriple dimensionUri crmP180 unitCAD
        ]

  describe "writeWorkGenreCategory" $ do
    prop "basic check" $ \(entityId, entityId2) -> do
      let workId              = unEntityId entityId
      let genreId              = unEntityId entityId2
      let triples           = toTriples $ WorkGenreCategory (WorkId workId) (GenreCategoryId genreId)

      let workUri = "/resource/Work" <> workId
      let genreCategoryUri = "/resource/GenreCategory" <> genreId
      triples `shouldContainElems` catMaybes
        [ mkTriple workUri crmP2 genreCategoryUri
        ]

  describe "writeRecordingDeclaration" $ do
    prop "basic check" $ \entityId -> do
      let rworkId              = unEntityId entityId
      let triples           = toTriples $ RecordingWorkDeclaration (RecordingWorkId rworkId)

      let rworkUri = "/resource/RecordingWork" <> rworkId
      triples `shouldContainElems` catMaybes
        [ mkTriple rworkUri rdfType frbrooF21
        ]

  describe "writeRecordingWorkWorkDerivative" $ do
    prop "basic check" $ \(entityId, entityId2) -> do
      let rworkId              = unEntityId entityId
      let workId              = unEntityId entityId2
      let triples           = toTriples $ RecordingWorkWorkDerivative (RecordingWorkId rworkId) (WorkId workId)

      let rworkUri = "/resource/RecordingWork" <> rworkId
      let workUri = "/resource/Work" <> workId
      triples `shouldContainElems` catMaybes
        [ mkTriple rworkUri frbrooR2 workUri
        ]

  describe "writeRecordingEventDeclaration" $ do
    prop "basic check" $ \entityId -> do
      let reworkId              = unEntityId entityId
      let triples           = toTriples $ RecordingEventDeclaration (RecordingEventId reworkId)

      let reworkUri = "/resource/RecordingEvent" <> reworkId
      triples `shouldContainElems` catMaybes
        [ mkTriple reworkUri rdfType frbrooF29
        ]

  describe "writeRecordingEventRealisation" $ do
    prop "basic check" $ \(entityId, entityId2) -> do
      let reworkId              = unEntityId entityId
      let rworkId              = unEntityId entityId2
      let triples           = toTriples $ RecordingEventRealisation (RecordingEventId reworkId) (RecordingWorkId rworkId)

      let reworkUri = "/resource/RecordingEvent" <> reworkId
      let rworkUri = "/resource/RecordingWork" <> rworkId
      triples `shouldContainElems` catMaybes
        [ mkTriple reworkUri frbrooR22 rworkUri
        ]

  describe "writeRecordingEventLocation" $ do
    prop "basic check" $ \(entityId, entityId2) -> do
      let reworkId              = unEntityId entityId
      let pId              = unEntityId entityId2
      let triples           = toTriples $ RecordingEventLocation (RecordingEventId reworkId) (PlaceId pId)

      let reworkUri = "/resource/RecordingEvent" <> reworkId
      let placeUri = "/resource/Place" <> pId
      triples `shouldContainElems` catMaybes
        [ mkTriple reworkUri crmP7 placeUri
        ]

  describe "writeRecordingEventCreatedRecording" $ do
    prop "basic check" $ \(entityId, entityId2) -> do
      let reworkId              = unEntityId entityId
      let rId              = unEntityId entityId2
      let triples           = toTriples $ RecordingEventCreatedRecording (RecordingEventId reworkId) (RecordingId rId)

      let reworkUri = "/resource/RecordingEvent" <> reworkId
      let recordingUri = "/resource/Recording" <> rId
      triples `shouldContainElems` catMaybes
        [ mkTriple reworkUri frbrooR21 recordingUri
        ]

  describe "writeRecordingEventTimeSpan" $ do
    prop "basic check" $ \(entityId, beginTimeW, endTimeW) -> do
      let reworkId              = unEntityId entityId
      let beginTime = unUTCTimeWrapper beginTimeW
      let endTime = unUTCTimeWrapper endTimeW
      let triples           = toTriples $ RecordingEventTimeSpan (RecordingEventId reworkId) (TimeSpan (Just beginTime) (Just endTime))

      let reworkUri = "/resource/RecordingEvent" <> reworkId
      let timeSpanUri = "/resource/Time-Span" <> utcTimeToText beginTime <> "-" <> utcTimeToText endTime
      triples `shouldContainElems` catMaybes
        [ mkTriple reworkUri crmP4 timeSpanUri
        , mkTripleLit timeSpanUri crmP82a (TypedL (utcTimeToText beginTime) xsdDateTime)
        , mkTripleLit timeSpanUri crmP82b (TypedL (utcTimeToText endTime) xsdDateTime)
        ]

    prop "empty begin time should not create crm:P82a triple"
      $ \(entityId, endTimeW) -> do
          let reworkId              = unEntityId entityId
          let endTime = unUTCTimeWrapper endTimeW
          let triples           = toTriples $ RecordingEventTimeSpan (RecordingEventId reworkId) (TimeSpan Nothing (Just endTime))

          let reworkUri = "/resource/RecordingEvent" <> reworkId
          let timeSpanUri = "/resource/Time-Span" <> utcTimeToText endTime

          triples `shouldContainElems` catMaybes
            [ mkTriple reworkUri crmP4 timeSpanUri
            , mkTripleLit timeSpanUri crmP82b (TypedL (utcTimeToText endTime) xsdDateTime)
            ]
          length (filter (hasPredicate crmP82a) triples) `shouldBe` 0

    prop "empty end time should not create crm:P82b triple"
      $ \(entityId, beginTimeW) -> do
          let reworkId              = unEntityId entityId
          let beginTime = unUTCTimeWrapper beginTimeW
          let triples           = toTriples $ RecordingEventTimeSpan (RecordingEventId reworkId) (TimeSpan (Just beginTime) Nothing)

          let reworkUri = "/resource/RecordingEvent" <> reworkId
          let timeSpanUri = "/resource/Time-Span" <> utcTimeToText beginTime

          triples `shouldContainElems` catMaybes
            [ mkTriple reworkUri crmP4 timeSpanUri
            , mkTripleLit timeSpanUri crmP82a (TypedL (utcTimeToText beginTime) xsdDateTime)
            ]
          length (filter (hasPredicate crmP82b) triples) `shouldBe` 0

    prop
        "empty begin and end time should not create any time-span"
      $ \entityId -> do
          let reworkId              = unEntityId entityId
          let triples           = toTriples $ RecordingEventTimeSpan (RecordingEventId reworkId) (TimeSpan Nothing Nothing)

          length (filter (hasPredicate crmP4) triples) `shouldBe` 0
          length (filter (hasPredicate crmP82a) triples) `shouldBe` 0
          length (filter (hasPredicate crmP82b) triples) `shouldBe` 0

  describe "writeRecordingActivity" $ do
    prop "basic check" $ \(entityId, entityId2) -> do
      let reworkId              = unEntityId entityId
      let raId              = unEntityId entityId2
      let triples           = toTriples $ RecordingEventActivity (RecordingEventId reworkId) (RecordingActivityId raId)

      let reworkUri = "/resource/RecordingEvent" <> reworkId
      let recordingActivityUri = "/resource/RecordingActivity" <> raId
      triples `shouldContainElems` catMaybes
        [ mkTriple reworkUri crmP9 recordingActivityUri
        ]

  describe "writeRecordingActivityPersonDeclaration" $ do
    prop "basic check" $ \(entityId, entityId2, entityId3) -> do
      let raId              = unEntityId entityId
      let pId              = unEntityId entityId2
      let rId              = unEntityId entityId3
      let triples           = toTriples $ RecordingActivityPersonDeclaration (RecordingActivityId raId) (PersonId pId) (RoleId rId)

      let recordingActivityCarriedOutByUri = "/resource/RecordingActivityCarriedOutBy" <> raId
      let recordingActivityUri = "/resource/RecordingActivity" <> raId
      let personUri = "/resource/Person" <> pId
      let roleUri = "/resource/Role" <> rId
      triples `shouldContainElems` catMaybes
        [ mkTriple recordingActivityCarriedOutByUri crmP01 recordingActivityUri
        , mkTriple recordingActivityCarriedOutByUri crmP02 personUri
        , mkTriple recordingActivityCarriedOutByUri crmP14_1 roleUri
        ]

  describe "writeRecordingActivityLegalBodyDeclaration" $ do
    prop "basic check" $ \(entityId, entityId2, entityId3) -> do
      let raId              = unEntityId entityId
      let lbId              = unEntityId entityId2
      let rId              = unEntityId entityId3
      let triples           = toTriples $ RecordingActivityLegalBodyDeclaration (RecordingActivityId raId) (LegalBodyId lbId) (RoleId rId)

      let recordingActivityCarriedOutByUri = "/resource/RecordingActivityCarriedOutBy" <> raId
      let recordingActivityUri = "/resource/RecordingActivity" <> raId
      let legalBodyUri = "/resource/LegalBody" <> lbId
      let roleUri = "/resource/Role" <> rId
      triples `shouldContainElems` catMaybes
        [ mkTriple recordingActivityCarriedOutByUri crmP01 recordingActivityUri
        , mkTriple recordingActivityCarriedOutByUri crmP02 legalBodyUri
        , mkTriple recordingActivityCarriedOutByUri crmP14_1 roleUri
        ]

  describe "writeRecordingDeclaration" $ do
    prop "basic check" $ \entityId -> do
      let rId              = unEntityId entityId
      let triples           = toTriples $ RecordingDeclaration (RecordingId rId)

      let recordingUri = "/resource/Recording" <> rId
      triples `shouldContainElems` catMaybes
        [ mkTriple recordingUri rdfType frbrooF26
        ]

  describe "writeRecordingLanguage" $ do
    prop "basic check" $ \(entityId, entityId2) -> do
      let rId              = unEntityId entityId
      let langId              = unEntityId entityId2
      let triples           = toTriples $ RecordingLanguage (RecordingId rId) (LanguageId langId)

      let recordingUri = "/resource/Recording" <> rId
      let languageUri = "/resource/Language" <> langId
      triples `shouldContainElems` catMaybes
        [ mkTriple recordingUri crmP72 languageUri
        ]

  describe "writePublicationExpressionDeclaration" $ do
    prop "basic check" $ \entityId -> do
      let peId              = unEntityId entityId
      let triples           = toTriples $ PublicationExpressionDeclaration (PublicationExpressionId peId)

      let publiExprUri = "/resource/PublicationExpression" <> peId
      triples `shouldContainElems` catMaybes
        [ mkTriple publiExprUri rdfType frbrooF24
        ]

  describe "writePublicationExpressionIncorporatesExpression" $ do
    prop "basic check" $ \(entityId, entityId2) -> do
      let peId              = unEntityId entityId
      let rId              = unEntityId entityId2
      let triples           = toTriples $ PublicationExpressionIncorporatesExpression (PublicationExpressionId peId) (RecordingId rId)

      let publiExprUri = "/resource/PublicationExpression" <> peId
      let recordingUri = "/resource/Recording" <> rId
      triples `shouldContainElems` catMaybes
        [ mkTriple publiExprUri crmP165 recordingUri
        ]

  describe "writePublicationEventDeclaration" $ do
    prop "basic check" $ \entityId -> do
      let peId              = unEntityId entityId
      let triples           = toTriples $ PublicationEventDeclaration (PublicationEventId peId)

      let peventUri = "/resource/PublicationEvent" <> peId
      triples `shouldContainElems` catMaybes
        [ mkTriple peventUri rdfType frbrooF30
        ]

  describe "writePublicationEventCreatedPublicationExpression" $ do
    prop "basic check" $ \(entityId, entityId2) -> do
      let peventId              = unEntityId entityId
      let pexprId              = unEntityId entityId2
      let triples           = toTriples $ PublicationEventCreatedPublicationExpression (PublicationEventId peventId) (PublicationExpressionId pexprId)

      let publiEventUri = "/resource/PublicationEvent" <> peventId
      let publiExprUri = "/resource/PublicationExpression" <> pexprId
      triples `shouldContainElems` catMaybes
        [ mkTriple publiEventUri frbrooR24 publiExprUri
        ]

  describe "writePublicProjectionEventDeclaration" $ do
    prop "basic check" $ \entityId -> do
      let peId              = unEntityId entityId
      let triples           = toTriples $ PublicProjectionEventDeclaration (PublicProjectionEventId peId)

      let peventUri = "/resource/PublicProjectionEvent" <> peId
      triples `shouldContainElems` catMaybes
        [ mkTriple peventUri rdfType crmE7
        , mkTriple peventUri crmP2 publicProjectionEventTypeUri
        ]

  describe "writePublicProjectionEventDeclaration" $ do
    prop "basic check" $ \entityId -> do
      let peId              = unEntityId entityId
      let triples           = toTriples $ PublicProjectionEventDeclaration (PublicProjectionEventId peId)

      let peventUri = "/resource/PublicProjectionEvent" <> peId
      triples `shouldContainElems` catMaybes
        [ mkTriple peventUri rdfType crmE7
        , mkTriple peventUri crmP2 publicProjectionEventTypeUri
        ]

  describe "writePublicProjectionEventTimeSpan" $ do
    prop "basic check" $ \(entityId, beginTimeW, endTimeW) -> do
      let ppeventId              = unEntityId entityId
      let beginTime = unUTCTimeWrapper beginTimeW
      let endTime = unUTCTimeWrapper endTimeW
      let triples           = toTriples $ PublicProjectionEventTimeSpan (PublicProjectionEventId ppeventId) (TimeSpan (Just beginTime) (Just endTime))

      let ppeventUri = "/resource/PublicProjectionEvent" <> ppeventId
      let timeSpanUri = "/resource/Time-Span" <> utcTimeToText beginTime <> "-" <> utcTimeToText endTime
      triples `shouldContainElems` catMaybes
        [ mkTriple ppeventUri crmP4 timeSpanUri
        , mkTripleLit timeSpanUri crmP82a (TypedL (utcTimeToText beginTime) xsdDateTime)
        , mkTripleLit timeSpanUri crmP82b (TypedL (utcTimeToText endTime) xsdDateTime)
        ]

    prop "empty begin time should not create crm:P82a triple"
      $ \(entityId, endTimeW) -> do
          let ppeventId              = unEntityId entityId
          let endTime = unUTCTimeWrapper endTimeW
          let triples           = toTriples $ PublicProjectionEventTimeSpan (PublicProjectionEventId ppeventId) (TimeSpan Nothing (Just endTime))

          let ppeventUri = "/resource/PublicProjectionEvent" <> ppeventId
          let timeSpanUri = "/resource/Time-Span" <> utcTimeToText endTime

          triples `shouldContainElems` catMaybes
            [ mkTriple ppeventUri crmP4 timeSpanUri
            , mkTripleLit timeSpanUri crmP82b (TypedL (utcTimeToText endTime) xsdDateTime)
            ]
          length (filter (hasPredicate crmP82a) triples) `shouldBe` 0

    prop "empty end time should not create crm:P82b triple"
      $ \(entityId, beginTimeW) -> do
          let ppeventId              = unEntityId entityId
          let beginTime = unUTCTimeWrapper beginTimeW
          let triples           = toTriples $ PublicProjectionEventTimeSpan (PublicProjectionEventId ppeventId) (TimeSpan (Just beginTime) Nothing)

          let ppeventUri = "/resource/PublicProjectionEvent" <> ppeventId
          let timeSpanUri = "/resource/Time-Span" <> utcTimeToText beginTime

          triples `shouldContainElems` catMaybes
            [ mkTriple ppeventUri crmP4 timeSpanUri
            , mkTripleLit timeSpanUri crmP82a (TypedL (utcTimeToText beginTime) xsdDateTime)
            ]
          length (filter (hasPredicate crmP82b) triples) `shouldBe` 0

    prop
        "empty begin and end time should not create any time-span"
      $ \entityId -> do
          let ppeventId              = unEntityId entityId
          let triples           = toTriples $ PublicProjectionEventTimeSpan (PublicProjectionEventId ppeventId) (TimeSpan Nothing Nothing)

          length (filter (hasPredicate crmP4) triples) `shouldBe` 0
          length (filter (hasPredicate crmP82a) triples) `shouldBe` 0
          length (filter (hasPredicate crmP82b) triples) `shouldBe` 0

  describe "writePublicProjectionEventUsedPublicationExpression" $ do
    prop "basic check" $ \(entityId, entityId2) -> do
      let ppeventId              = unEntityId entityId
      let pexprId              = unEntityId entityId2
      let triples           = toTriples $ PublicProjectionEventUsedPublicationExpression (PublicProjectionEventId ppeventId) (PublicationExpressionId pexprId)

      let ppeventUri = "/resource/PublicProjectionEvent" <> ppeventId
      let pexprUri = "/resource/PublicationExpression" <> pexprId
      triples `shouldContainElems` catMaybes
        [ mkTriple ppeventUri crmP16 pexprUri
        ]

  describe "writePublicProjectionEventDeclaration" $ do
    prop "basic check" $ \entityId -> do
      let manifId              = unEntityId entityId
      let triples           = toTriples $ ManifestationProductTypeDeclaration (ManifestationProductTypeId manifId)

      let manifProdTypeUri = "/resource/ManifestationProductType" <> manifId
      triples `shouldContainElems` catMaybes
        [ mkTriple manifProdTypeUri rdfType frbrooF3
        ]

  describe "writeManifestationProductTypeCarriesPublicationExpression" $ do
    prop "basic check" $ \(entityId, entityId2) -> do
      let manifId              = unEntityId entityId
      let pexprId              = unEntityId entityId2
      let triples           = toTriples $ ManifestationProductTypeCarriesPublicationExpression (ManifestationProductTypeId manifId) (PublicationExpressionId pexprId)

      let manifProdTypeUri = "/resource/ManifestationProductType" <> manifId
      let pexprUri = "/resource/PublicationExpression" <> pexprId
      triples `shouldContainElems` catMaybes
        [ mkTriple manifProdTypeUri frbrooCLR6 pexprUri
        ]

  describe "writeManifestationProductTypeDuration" $ do
    prop "basic check" $ \(entityId, duration) -> do
      let manifId              = unEntityId entityId
      let triples           = toTriples $ ManifestationProductTypeDuration (ManifestationProductTypeId manifId) duration

      let manifProdTypeUri = "/resource/ManifestationProductType" <> manifId
      let durationUri = "/resource/Dimension" <> T.pack (show duration) <> "Seconds"
      triples `shouldContainElems` catMaybes
        [ mkTriple manifProdTypeUri crmP43 durationUri
        , mkTriple durationUri rdfType crmE54
        , mkTriple durationUri crmP2 durationTypeUri
        , mkTripleLit durationUri crmP90 (TypedL (T.pack $ show duration) xsdInteger)
        , mkTriple durationUri crmP91 unitSEC
        ]

hasPredicate :: Text -> Triple -> Bool
hasPredicate pred (Triple _ (UNode p) _) = p == pred
hasPredicate _    _                      = False
