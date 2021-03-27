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
module Data.CQLOD.Writers.RDF where

import           Data.CQLOD
import           Data.RDF.Types.Extended (mkTriple, mkTripleLit)
import           Data.RDF.Vocabulary
import           Import
import           Namespaces
import           Util                    (utcTimeToText)

import           Control.Monad.State
import           Data.RDF                (RDF, Rdf, Triples)
import qualified Data.RDF                as RDF
import qualified Data.Text               as T

append :: (Foldable m) => m a -> State [a] ()
append a = do
  s <- get
  forM_ a $ \b -> put $ s ++ [b]

class ToTriples a where
  toTriples :: a -> Triples

instance ToTriples CQLODStatement where
  toTriples (RoleDeclaration role) = writeRoleDeclaration role
  toTriples (RoleWikidataLink rId wikidataUri) = writeRoleWikidataLink rId wikidataUri

  toTriples (PersonDeclaration person) = writePersonDeclaration person
  toTriples (PersonWikidataLink pId wikidataUri) = writePersonWikidataLink pId wikidataUri

  toTriples (GenreCategoryDeclaration genreCategory) =
    writeGenreCategoryDeclaration genreCategory
  toTriples (GenreCategoryWikidataLink gcId wikidataUri) = writeGenreCategoryWikidataLink gcId wikidataUri

  toTriples (LanguageDeclaration language) = writeLanguageDeclaration language
  toTriples (LanguageWikidataLink lId wikidataUri) = writeLanguageWikidataLink lId wikidataUri

  toTriples (LegalBodyDeclaration legalBody) =
    writeLegalBodyDeclaration legalBody

  toTriples (PlaceDeclaration place) =
    writePlaceDeclaration place
  toTriples (PlaceWikidataLink pId wikidataUri) = writePlaceWikidataLink pId wikidataUri

  toTriples (SynopsisDeclaration synopsis) =
    writeSynopsisDeclaration synopsis
  toTriples (SynopsisTranslation sId osId) =
    writeSynopsisTranslation sId osId

  toTriples (WorkDeclaration   wid      ) = writeWorkDeclaration wid
  toTriples (WorkTypeDeclaration  wid wtype) = writeWorkTypeDeclaration wid wtype
  toTriples (WorkWikidataLink wid wikidataUri) = writeWorkWikidataLink wid wikidataUri
  toTriples (WorkOriginalTitle wid title) = writeWorkOriginalTitle wid title
  toTriples (WorkOtherTitle wid wtId title) = writeWorkOtherTitle wid wtId title
  toTriples (WorkSourcePerson wid swid pId) = writeWorkSourcePerson wid swid pId
  toTriples (WorkSourceLegalBody wid swid lbId) = writeWorkSourceLegalBody wid swid lbId
  toTriples (WorkProductionCost wid amount currency) =
    writeProductionCost wid amount currency
  toTriples (WorkGenreCategory wid gid) = writeWorkGenreCategory wid gid
  toTriples (WorkSynopsis wid sid) = writeWorkSynopsis wid sid

  toTriples (RecordingWorkDeclaration rwid) =
    writeRecordingWorkDeclaration rwid
  toTriples (RecordingWorkWorkDerivative rid wid) =
    writeRecordingWorkWorkDerivative rid wid

  toTriples (RecordingEventDeclaration reid) =
    writeRecordingEventDeclaration reid
  toTriples (RecordingEventRealisation reid rwid) =
    writeRecordingEventRealisation reid rwid
  toTriples (RecordingEventLocation reid pId) =
    writeRecordingEventLocation reid pId
  toTriples (RecordingEventCreatedRecording reid rid) =
    writeRecordingEventCreatedRecording reid rid
  toTriples (RecordingEventTimeSpan reid timespan) =
    writeRecordingEventTimeSpan reid timespan
  toTriples (RecordingEventActivity reid raid) =
    writeRecordingEventActivity reid raid

  toTriples (RecordingActivityPersonDeclaration raId pId rId) =
    writeRecordingActivityPersonDeclaration raId pId rId

  toTriples (RecordingActivityLegalBodyDeclaration raId leId rId) =
    writeRecordingActivityLegalBodyDeclaration raId leId rId

  toTriples (RecordingDeclaration rid) = writeRecordingDeclaration rid
  toTriples (RecordingLanguage rid lid) = writeRecordingLanguage rid lid

  toTriples (PublicationExpressionDeclaration pexprid) =
    writePublicationExpressionDeclaration pexprid
  toTriples (PublicationExpressionIncorporatesExpression pexprid rid) =
    writePublicationExpressionIncorporatesExpression pexprid rid

  toTriples (PublicationEventDeclaration peventid) =
    writePublicationEventDeclaration peventid
  toTriples (PublicationEventCreatedPublicationExpression peventid pexprid) =
    writePublicationEventCreatedPublicationExpression peventid pexprid

  toTriples (PublicProjectionEventDeclaration ppeventid) =
    writePublicProjectionEventDeclaration ppeventid
  toTriples (PublicProjectionEventTimeSpan ppeventid timespan) =
    writePublicProjectionEventTimeSpan ppeventid timespan
  toTriples (PublicProjectionEventUsedPublicationExpression ppeventid pexprId) =
    writePublicProjectionEventUsedPublicationExpression ppeventid pexprId

  toTriples (ManifestationProductTypeDeclaration mptId) =
    writeManifestationProductTypeDeclaration mptId
  toTriples (ManifestationProductTypeCarriesPublicationExpression mptId pubExprId) =
    writeManifestationProductTypeCarriesPublicationExpression mptId pubExprId
  toTriples (ManifestationProductTypeDuration mptId duration) =
    writeManifestationProductTypeDuration mptId duration

  -- toTriples _ = []

baseUriPath :: Text
baseUriPath = "/resource"

roleTypeUri :: Text
roleTypeUri = baseUriPath <> "/Role"

directorRoleUri :: Text
directorRoleUri = baseUriPath <> "/Role1"

genreCategoryTypeUri :: Text
genreCategoryTypeUri = baseUriPath <> "/GenreCategory"

publicProjectionEventTypeUri :: Text
publicProjectionEventTypeUri = baseUriPath <> "/PublicProjectionEvent"

budgetTypeUri :: Text
budgetTypeUri = baseUriPath <> "/Budget"

synopsisTypeUri :: Text
synopsisTypeUri = baseUriPath <> "/Synopsis"

durationTypeUri :: Text
durationTypeUri = baseUriPath <> "/Duration"

secondsTypeUri :: Text
secondsTypeUri = baseUriPath <> "/Second"

originalTitleTypeUri :: Text
originalTitleTypeUri = baseUriPath <> "/OriginalTitle"

mkSynopsisUri :: Text -> Text
mkSynopsisUri sid = baseUriPath <> "/Synopsis" <> sid

mkPersonUri :: Text -> Text
mkPersonUri pId = baseUriPath <> "/Person" <> pId

mkPersonIdentifierUri :: Text -> Text
mkPersonIdentifierUri pId = baseUriPath <> "/IdentifierPerson" <> pId

mkPersonAppellationUri :: Text -> Text
mkPersonAppellationUri pId =
  baseUriPath <> "/AppellationPerson" <> pId

mkWorkUri :: Text -> Text
mkWorkUri wid = baseUriPath <> "/Work" <> wid

mkWorkTypeUri :: WorkType -> Text
mkWorkTypeUri wtype = baseUriPath <> "/" <> T.pack (show wtype)

mkWorkConceptionUri :: Text -> Text
mkWorkConceptionUri wcId = baseUriPath <> "/WorkConception" <> wcId

mkWorkOriginalTitleUri :: Text -> Text
mkWorkOriginalTitleUri wid = baseUriPath <> "/OriginalTitleWork" <> wid

mkWorkOtherTitleUri :: Text -> Text
mkWorkOtherTitleUri wid = baseUriPath <> "/WorkTitle" <> wid

mkWorkCostUri :: Text -> Text
mkWorkCostUri dimensionLabel = baseUriPath <> "/" <> dimensionLabel

mkRecordingWorkUri :: Text -> Text
mkRecordingWorkUri rwid = baseUriPath <> "/RecordingWork" <> rwid

mkRecordingEventUri :: Text -> Text
mkRecordingEventUri reid = baseUriPath <> "/RecordingEvent" <> reid

mkRecordingActivityUri :: Text -> Text
mkRecordingActivityUri raId = baseUriPath <> "/RecordingActivity" <> raId

mkRecordingActivityCarriedOutByUri :: Text -> Text
mkRecordingActivityCarriedOutByUri raId = baseUriPath <> "/RecordingActivityCarriedOutBy" <> raId

mkRecordingUri :: Text -> Text
mkRecordingUri rid = baseUriPath <> "/Recording" <> rid

mkPublicationExpressionUri :: Text -> Text
mkPublicationExpressionUri pexprid =
  baseUriPath <> "/PublicationExpression" <> pexprid

mkPublicationEventUri :: Text -> Text
mkPublicationEventUri peventid = baseUriPath <> "/PublicationEvent" <> peventid

mkPublicProjectionEventUri :: Text -> Text
mkPublicProjectionEventUri projEventId =
  baseUriPath <> "/PublicProjectionEvent" <> projEventId

mkRoleUri :: Text -> Text
mkRoleUri rid = baseUriPath <> "/Role" <> rid

mkRoleIdentifierUri :: Text -> Text
mkRoleIdentifierUri rid = baseUriPath <> "/IdentifierRole" <> rid

mkPlaceUri :: Text -> Text
mkPlaceUri pid = baseUriPath <> "/Place" <> pid

mkPlaceIdentifierUri :: Text -> Text
mkPlaceIdentifierUri pid = baseUriPath <> "/IdentifierPlace" <> pid

mkPlaceAppellationUri :: Text -> Text
mkPlaceAppellationUri pid = baseUriPath <> "/AppellationPlace" <> pid

mkTimeSpanUri :: Text -> Text
mkTimeSpanUri timeSpanLabel = baseUriPath <> "/Time-Span" <> timeSpanLabel

mkGenreCategoryUri :: Text -> Text
mkGenreCategoryUri gid = baseUriPath <> "/GenreCategory" <> gid

mkGenreCategoryIdentifierUri :: Text -> Text
mkGenreCategoryIdentifierUri gid =
  baseUriPath <> "/IdentifierGenreCategory" <> gid

mkGenreCategoryAppellationUri :: Text -> Text
mkGenreCategoryAppellationUri gid =
  baseUriPath <> "/AppellationGenreCategory" <> gid

mkLanguageUri :: Text -> Text
mkLanguageUri lid = baseUriPath <> "/Language" <> lid

mkLanguageIdentifierUri :: Text -> Text
mkLanguageIdentifierUri lid = baseUriPath <> "/IdentifierLanguage" <> lid

mkLanguageAppellationUri :: Text -> Text
mkLanguageAppellationUri lid = baseUriPath <> "/AppellationLanguage" <> lid

mkLegalBodyUri :: Text -> Text
mkLegalBodyUri lbid = baseUriPath <> "/LegalBody" <> lbid

mkLegalBodyIdentifierUri :: Text -> Text
mkLegalBodyIdentifierUri lbid = baseUriPath <> "/IdentifierLegalBody" <> lbid

mkLegalBodyAppellationUri :: Text -> Text
mkLegalBodyAppellationUri lbid = baseUriPath <> "/AppellationLegalBody" <> lbid

mkManifestationProductTypeUri :: Text -> Text
mkManifestationProductTypeUri mptId = baseUriPath <> "/ManifestationProductType" <> mptId

mkDurationUri :: Text -> Text
mkDurationUri durationId = baseUriPath <> "/" <> durationId

writeRdf :: (Monad m, Rdf a) => CQLOD -> m (RDF a)
writeRdf (CQLOD statements) = do
  let entityTypeTriples = mkEntityTypesTriples
  let triples = concatMap toTriples statements
  return $ RDF.mkRdf (entityTypeTriples ++ triples) Nothing prefixMappings

mkEntityTypesTriples :: Triples
mkEntityTypesTriples = execState writeTriples []
 where
  writeTriples :: State Triples ()
  writeTriples = do
    append $ mkTriple roleTypeUri rdfType crmE55
    append $ mkTripleLit roleTypeUri rdfsLabel (RDF.PlainLL "Role" "fr")
    append $ mkTripleLit roleTypeUri rdfsLabel (RDF.PlainLL "Role" "en")
    append $ mkTripleLit
      roleTypeUri
      rdfsComment
      (RDF.PlainLL "Role occupé par un agent dans la production d'une oeuvre" "fr"
      )

    append $ mkTriple directorRoleUri rdfType crmE55
    append $ mkTripleLit directorRoleUri rdfsLabel (RDF.PlainLL "Réalisation" "fr")
    append $ mkTriple directorRoleUri crmP2 roleTypeUri
    let directorRoleIdentifierUri = baseUriPath <> "/IdentifierRole1"
    append $ mkTriple directorRoleUri crmP48 directorRoleIdentifierUri
    append $ mkTriple directorRoleIdentifierUri rdfType crmE42
    append $ mkTripleLit directorRoleIdentifierUri rdfsLabel (RDF.PlainLL "Identifiant de réalisation" "fr")
    append $ mkTripleLit directorRoleIdentifierUri crmP190 (RDF.PlainL "1")

    append $ mkTriple durationTypeUri rdfType crmE55
    append $ mkTripleLit durationTypeUri rdfsLabel (RDF.PlainLL "Duration" "en")
    append $ mkTripleLit durationTypeUri rdfsLabel (RDF.PlainLL "Durée" "fr")
    append $ mkTripleLit durationTypeUri rdfsComment (RDF.PlainLL "Notion de durée" "fr")

    append $ mkTriple genreCategoryTypeUri rdfType crmE55
    append $ mkTripleLit genreCategoryTypeUri rdfsLabel (RDF.PlainLL "Genre ou category" "fr")
    append $ mkTripleLit genreCategoryTypeUri rdfsLabel (RDF.PlainLL "Genre or category" "en")
    append $ mkTripleLit genreCategoryTypeUri rdfsComment (RDF.PlainLL "Genre cinématographique ou catégorie d'une oeuvre" "fr")

    append $ mkTriple synopsisTypeUri rdfType crmE55
    append $ mkTripleLit synopsisTypeUri rdfsLabel (RDF.PlainLL "Synopsis" "fr")
    append $ mkTripleLit synopsisTypeUri rdfsLabel (RDF.PlainLL "Synopsis" "en")

    append $ mkTriple budgetTypeUri rdfType crmE55
    append $ mkTripleLit budgetTypeUri rdfsLabel (RDF.PlainLL "Budget" "fr")
    append $ mkTripleLit budgetTypeUri rdfsLabel (RDF.PlainLL "Budget" "en")
    append $ mkTripleLit budgetTypeUri rdfsComment (RDF.PlainLL "Budget d'une oeuvre" "fr")

    append $ mkTriple originalTitleTypeUri rdfType crmE55
    append $ mkTripleLit originalTitleTypeUri rdfsLabel (RDF.PlainLL "Titre originale" "fr")
    append $ mkTripleLit originalTitleTypeUri rdfsLabel (RDF.PlainLL "Original title" "en")
    append $ mkTripleLit originalTitleTypeUri rdfsComment (RDF.PlainLL "Titre originale d'une oeuvre" "fr")

    append $ mkTriple (mkWorkTypeUri UniqueWork) rdfType crmE55
    append $ mkTripleLit (mkWorkTypeUri UniqueWork) rdfsLabel (RDF.PlainLL "Oeuvre unique" "fr")
    append $ mkTripleLit (mkWorkTypeUri UniqueWork) rdfsLabel (RDF.PlainLL "Unique work" "fr")
    append $ mkTriple (mkWorkTypeUri TelevisionSeries) rdfType crmE55
    append $ mkTripleLit (mkWorkTypeUri TelevisionSeries) rdfsLabel (RDF.PlainLL "Série télévisée" "fr")
    append $ mkTripleLit (mkWorkTypeUri TelevisionSeries) rdfsLabel (RDF.PlainLL "Television Series" "en")
    append $ mkTriple (mkWorkTypeUri TelevisionSeriesSeason) rdfType crmE55
    append $ mkTripleLit (mkWorkTypeUri TelevisionSeriesSeason) rdfsLabel (RDF.PlainLL "Saison d'une série télévisée" "fr")
    append $ mkTripleLit (mkWorkTypeUri TelevisionSeriesSeason) rdfsLabel (RDF.PlainLL "Television series season" "en")
    append $ mkTriple (mkWorkTypeUri TelevisionSeriesEpisode) rdfType crmE55
    append $ mkTripleLit (mkWorkTypeUri TelevisionSeriesEpisode) rdfsLabel (RDF.PlainLL "Épisode d'une série télévisée" "fr")
    append $ mkTripleLit (mkWorkTypeUri TelevisionSeriesEpisode) rdfsLabel (RDF.PlainLL "Television series episode" "en")

writePersonDeclaration :: Person -> Triples
writePersonDeclaration person = execState writeTriples []

 where
  writeTriples :: State Triples ()
  writeTriples = do
    let firstNameMaybe       = personFirstName person
        lastNameMaybe        = personLastName person
        fullNameMaybe        = mkFullName firstNameMaybe lastNameMaybe
        pid                  = unPersonId $ personId person
        personUri            = mkPersonUri pid
        personIdentifierUri  = mkPersonIdentifierUri pid
        personAppellationUri = mkPersonAppellationUri pid

    append $ mkTriple personUri rdfType crmE21
    append $ mkTriple personUri crmP48 personIdentifierUri
    append $ mkTriple personIdentifierUri rdfType crmE42
    append $ mkTripleLit personIdentifierUri crmP190 (RDF.PlainL pid)

    forM_ fullNameMaybe $ \fullname -> do
      append $ mkTripleLit personUri rdfsLabel (RDF.PlainL fullname)
      append $ mkTripleLit personUri foafName (RDF.PlainL fullname)
      append $ mkTriple personUri crmP1 personAppellationUri
      append $ mkTriple personAppellationUri rdfType crmE41
      append $ mkTripleLit personAppellationUri rdfsLabel (RDF.PlainLL ("Appellation de " <> fullname) "fr")
      append $ mkTripleLit personAppellationUri crmP190 (RDF.PlainL fullname)
      append $ mkTripleLit personIdentifierUri rdfsLabel (RDF.PlainLL ("Identifiant de " <> fullname) "fr")

    forM_ lastNameMaybe $ \lastName ->
      append $ mkTripleLit personUri foafFamilyName (RDF.PlainL lastName)

    forM_ firstNameMaybe $ \firstName ->
      append $ mkTripleLit personUri foafGivenName (RDF.PlainL firstName)

  mkFullName :: Maybe Text -> Maybe Text -> Maybe Text
  mkFullName firstNameMaybe lastNameMaybe =
    case (firstNameMaybe, lastNameMaybe) of
      (f@(Just _), l@(Just _)) -> f <> Just " " <> l
      (f@(Just _), Nothing   ) -> f
      (Nothing   , l@(Just _)) -> l
      (Nothing   , Nothing   ) -> Nothing

writePersonWikidataLink :: PersonId -> WikidataUri -> Triples
writePersonWikidataLink (PersonId pId) (WikidataUri wikidataUri) =
  execState writeTriples []

 where
  writeTriples :: State Triples ()
  writeTriples = do
    let personUri = mkPersonUri pId
    append $ mkTriple personUri owlSameAs wikidataUri

writeRoleDeclaration :: Role -> Triples
writeRoleDeclaration (Role (RoleId rid) rname) = do
  execState writeTriples []

 where
  writeTriples :: State Triples ()
  writeTriples = do
    let roleUri           = mkRoleUri rid
    let roleIdentifierUri = mkRoleIdentifierUri rid

    append $ mkTriple roleUri rdfType crmE55
    append $ mkTriple roleUri crmP2 roleTypeUri
    append $ mkTripleLit roleUri rdfsLabel (RDF.PlainLL rname "fr")
    append $ mkTriple roleUri crmP48 roleIdentifierUri
    append $ mkTripleLit roleIdentifierUri crmP190 (RDF.PlainL rid)

writeRoleWikidataLink :: RoleId -> WikidataUri -> Triples
writeRoleWikidataLink (RoleId rId) (WikidataUri wikidataUri) =
  execState writeTriples []

 where
  writeTriples :: State Triples ()
  writeTriples = do
    let roleUri = mkRoleUri rId
    append $ mkTriple roleUri owlSameAs wikidataUri

writeWorkDeclaration :: WorkId -> Triples
writeWorkDeclaration (WorkId wid) =
  catMaybes [ mkTriple (mkWorkUri wid) rdfType frbrooF1
            , mkTripleLit (mkWorkUri wid) rdfsComment (RDF.PlainLL ("Oeuvre audiovisuelle avec l'identifiant " <> wid) "fr")
            ]

writeWorkTypeDeclaration :: WorkId -> WorkType -> Triples
writeWorkTypeDeclaration (WorkId wId) workType =
  execState writeTriples []

 where
  writeTriples :: State Triples ()
  writeTriples = do
    let workUri = mkWorkUri wId
    let workTypeUri = mkWorkTypeUri workType
    append $ mkTriple workUri crmP2 workTypeUri

writeWorkWikidataLink :: WorkId -> WikidataUri -> Triples
writeWorkWikidataLink (WorkId wId) (WikidataUri wikidataUri) =
  execState writeTriples []

 where
  writeTriples :: State Triples ()
  writeTriples = do
    let workUri = mkWorkUri wId
    append $ mkTriple workUri owlSameAs wikidataUri

writeWorkOriginalTitle :: WorkId -> Text -> Triples
writeWorkOriginalTitle (WorkId wid) title = execState writeTriples []
 where
  writeTriples :: State Triples ()
  writeTriples = do
    let workUri = mkWorkUri wid
    let originalTitleUri = mkWorkOriginalTitleUri wid

    append $ mkTripleLit workUri rdfsLabel (RDF.PlainL title)
    append $ mkTriple workUri crmP102 originalTitleUri
    append $ mkTriple originalTitleUri rdfType crmE35
    append $ mkTriple originalTitleUri crmP2 originalTitleTypeUri
    append $ mkTripleLit originalTitleUri crmP190 (RDF.PlainL title)

writeWorkOtherTitle :: WorkId -> WorkTitleId -> Text -> Triples
writeWorkOtherTitle (WorkId wId) (WorkTitleId wtId) title = do
  execState writeTriples []

 where
  writeTriples :: State Triples ()
  writeTriples = do
    let workUri           = mkWorkUri wId
    let workTitleUri = mkWorkOtherTitleUri wtId

    append $ mkTriple workUri crmP102 workTitleUri
    append $ mkTriple workTitleUri rdfType crmE35
    append $ mkTripleLit workTitleUri crmP190 (RDF.PlainL title)

writeWorkSourcePerson :: WorkId -> WorkId -> PersonId -> Triples
writeWorkSourcePerson (WorkId wId) (WorkId swId) (PersonId pId) = do
  execState writeTriples []

 where
  writeTriples :: State Triples ()
  writeTriples = do
    let workUri        = mkWorkUri wId
    let workDerivedUri = mkWorkUri swId
    let workDerivedConceptionUri = mkWorkConceptionUri swId
    let personUri = mkPersonUri pId

    append $ mkTriple workUri frbrooR2 workDerivedUri
    append $ mkTriple workDerivedUri frbrooR16i workDerivedConceptionUri
    append $ mkTriple workDerivedConceptionUri crmP14 personUri

writeWorkSourceLegalBody :: WorkId -> WorkId -> LegalBodyId -> Triples
writeWorkSourceLegalBody (WorkId wId) (WorkId swId) (LegalBodyId lbId) = do
  execState writeTriples []

 where
  writeTriples :: State Triples ()
  writeTriples = do
    let workUri        = mkWorkUri wId
    let workDerivedUri = mkWorkUri swId
    let workDerivedConceptionUri = mkWorkConceptionUri swId
    let legalBodyUri = mkLegalBodyUri lbId

    append $ mkTriple workUri frbrooR2 workDerivedUri
    append $ mkTriple workDerivedUri frbrooR16i workDerivedConceptionUri
    append $ mkTriple workDerivedConceptionUri crmP14 legalBodyUri

writeProductionCost :: WorkId -> Double -> Currency -> Triples
writeProductionCost (WorkId workId) amount currency =
  execState writeTriples []

 where
  writeTriples :: State Triples ()
  writeTriples = do
    let workUri        = mkWorkUri workId
        budgetText     = T.pack (show amount)
        dimensionLabel = "Dimension" <> budgetText <> T.pack (show currency)
        dimensionUri   = mkWorkCostUri dimensionLabel
    append $ mkTriple workUri crmP43 dimensionUri
    append $ mkTriple dimensionUri rdfType crmE54
    append $ mkTriple dimensionUri crmP2   budgetTypeUri
    append $ mkTripleLit dimensionUri rdfsLabel (RDF.PlainL $ budgetText <> T.pack (show currency))
    append $ mkTripleLit dimensionUri crmP181   (RDF.TypedL budgetText xsdDouble)
    append $ mkTriple dimensionUri crmP180 (currencyToUri currency)

writeWorkGenreCategory :: WorkId -> GenreCategoryId -> Triples
writeWorkGenreCategory (WorkId wid) (GenreCategoryId gid) =
  execState writeTriples []

 where
  writeTriples :: State Triples ()
  writeTriples = do
    let workUri            = mkWorkUri wid
        genreCategoryUri  = mkGenreCategoryUri gid
    append $ mkTriple workUri crmP2 genreCategoryUri

writeWorkSynopsis :: WorkId -> SynopsisId -> Triples
writeWorkSynopsis (WorkId wid) (SynopsisId sid) =
  execState writeTriples []

 where
  writeTriples :: State Triples ()
  writeTriples = do
    let workUri            = mkWorkUri wid
        synopsisUri  = mkSynopsisUri sid
    append $ mkTriple synopsisUri crmP67 workUri

writeRecordingWorkDeclaration :: RecordingWorkId -> Triples
writeRecordingWorkDeclaration (RecordingWorkId recordingWorkId) =
  catMaybes [mkTriple (mkRecordingWorkUri recordingWorkId) rdfType frbrooF21]

writeRecordingWorkWorkDerivative :: RecordingWorkId -> WorkId -> Triples
writeRecordingWorkWorkDerivative (RecordingWorkId rwId) (WorkId wId) =
  execState writeTriples []

 where
  writeTriples :: State Triples ()
  writeTriples = do
    let recordingWorkUri = mkRecordingWorkUri rwId
    let workUri = mkWorkUri wId
    append $ mkTriple recordingWorkUri frbrooR2 workUri

writeRecordingEventDeclaration :: RecordingEventId -> Triples
writeRecordingEventDeclaration (RecordingEventId recordingEventId) =
  catMaybes [mkTriple (mkRecordingEventUri recordingEventId) rdfType frbrooF29]

writeRecordingEventRealisation :: RecordingEventId -> RecordingWorkId -> Triples
writeRecordingEventRealisation (RecordingEventId recordingEventId) (RecordingWorkId recordingWorkId)
  = catMaybes
    [ mkTriple (mkRecordingEventUri recordingEventId)
               frbrooR22
               (mkRecordingWorkUri recordingWorkId)
    ]

writeRecordingEventLocation :: RecordingEventId -> PlaceId -> Triples
writeRecordingEventLocation (RecordingEventId reid) (PlaceId pid) =
  execState writeTriples []

 where
  writeTriples :: State Triples ()
  writeTriples = do
    let recordingEventUri            = mkRecordingEventUri reid
        placeUri  = mkPlaceUri pid
    append $ mkTriple recordingEventUri crmP7 placeUri

writeRecordingDeclaration :: RecordingId -> Triples
writeRecordingDeclaration (RecordingId recordingId) =
  catMaybes [mkTriple (mkRecordingUri recordingId) rdfType frbrooF26]

writeRecordingLanguage :: RecordingId -> LanguageId -> Triples
writeRecordingLanguage (RecordingId rid) (LanguageId lid) =
  execState writeTriples []

 where
  writeTriples :: State Triples ()
  writeTriples = do
    let recordingUri            = mkRecordingUri rid
        languageUri  = mkLanguageUri lid
    append $ mkTriple recordingUri crmP72 languageUri

writeRecordingEventCreatedRecording
  :: RecordingEventId -> RecordingId -> Triples
writeRecordingEventCreatedRecording (RecordingEventId recordingEventId) (RecordingId recordingId)
  = catMaybes
    [ mkTriple (mkRecordingEventUri recordingEventId)
               frbrooR21
               (mkRecordingUri recordingId)
    ]

writePublicationExpressionDeclaration :: PublicationExpressionId -> Triples
writePublicationExpressionDeclaration (PublicationExpressionId publicationExpressionId)
  = catMaybes
    [ mkTriple (mkPublicationExpressionUri publicationExpressionId)
               rdfType
               frbrooF24
    ]

writePublicationExpressionIncorporatesExpression
  :: PublicationExpressionId -> RecordingId -> Triples
writePublicationExpressionIncorporatesExpression (PublicationExpressionId publicationExpressionId) (RecordingId recordingId)
  = catMaybes
    [ mkTriple (mkPublicationExpressionUri publicationExpressionId)
               crmP165
               (mkRecordingUri recordingId)
    ]

writePublicationEventDeclaration :: PublicationEventId -> Triples
writePublicationEventDeclaration (PublicationEventId publicationEventId) =
  catMaybes
    [mkTriple (mkPublicationEventUri publicationEventId) rdfType frbrooF30]

writePublicationEventCreatedPublicationExpression
  :: PublicationEventId -> PublicationExpressionId -> Triples
writePublicationEventCreatedPublicationExpression (PublicationEventId publicationEventId) (PublicationExpressionId publicationExpressionId)
  = catMaybes
    [ mkTriple (mkPublicationEventUri publicationEventId)
               frbrooR24
               (mkPublicationExpressionUri publicationExpressionId)
    ]

writePublicProjectionEventDeclaration :: PublicProjectionEventId -> Triples
writePublicProjectionEventDeclaration (PublicProjectionEventId publicProjectionEventId)
  = catMaybes
    [ mkTriple (mkPublicProjectionEventUri publicProjectionEventId)
               rdfType
               crmE7
    , mkTriple (mkPublicProjectionEventUri publicProjectionEventId)
               crmP2
               publicProjectionEventTypeUri
    ]

writePublicProjectionEventTimeSpan
  :: PublicProjectionEventId -> TimeSpan -> Triples
writePublicProjectionEventTimeSpan (PublicProjectionEventId publicProjectionEventId) (TimeSpan beginTimeM endTimeM) =
  execState writeTriples []

 where
  writeTriples :: State Triples ()
  writeTriples = do
    let publicProjectionEventUri = mkPublicProjectionEventUri publicProjectionEventId
        timeSpanLit = T.intercalate "-" $ utcTimeToText <$> catMaybes [beginTimeM, endTimeM]
        timeSpanUri = mkTimeSpanUri timeSpanLit

    forM_ (beginTimeM <|> endTimeM) $ \_ ->
      append $ mkTriple publicProjectionEventUri crmP4 timeSpanUri
    forM_ beginTimeM $ \beginTime ->
      append $ mkTripleLit timeSpanUri crmP82a (RDF.TypedL (utcTimeToText beginTime) xsdDateTime)
    forM_ endTimeM $ \endTime ->
      append $ mkTripleLit timeSpanUri crmP82b (RDF.TypedL (utcTimeToText endTime) xsdDateTime)

-- writePublicProjectionEventUsedPublicationExpression ppeventid pexprId
writePublicProjectionEventUsedPublicationExpression
  :: PublicProjectionEventId -> PublicationExpressionId -> Triples
writePublicProjectionEventUsedPublicationExpression (PublicProjectionEventId publicProjectionEventId) (PublicationExpressionId pubExprId) =
  execState writeTriples []

 where
  writeTriples :: State Triples ()
  writeTriples = do
    let publicProjectionEventUri = mkPublicProjectionEventUri publicProjectionEventId
        publicationExprUri = mkPublicationExpressionUri pubExprId
    append $ mkTriple publicProjectionEventUri crmP16 publicationExprUri

writeRecordingEventTimeSpan :: RecordingEventId -> TimeSpan -> Triples
writeRecordingEventTimeSpan (RecordingEventId recordingEventId) (TimeSpan beginTimeM endTimeM) =
  execState writeTriples []

 where
  writeTriples :: State Triples ()
  writeTriples = do
    let recordingEventUri = mkRecordingEventUri recordingEventId
        timeSpanLit = T.intercalate "-" $ utcTimeToText <$> catMaybes [beginTimeM, endTimeM]
        timeSpanUri = mkTimeSpanUri timeSpanLit

    forM_ (beginTimeM <|> endTimeM) $ \_ ->
      append $ mkTriple recordingEventUri crmP4 timeSpanUri
    forM_ beginTimeM $ \beginTime ->
      append $ mkTripleLit timeSpanUri crmP82a (RDF.TypedL (utcTimeToText beginTime) xsdDateTime)
    forM_ endTimeM $ \endTime ->
      append $ mkTripleLit timeSpanUri crmP82b (RDF.TypedL (utcTimeToText endTime) xsdDateTime)

writeRecordingEventActivity :: RecordingEventId -> RecordingActivityId -> Triples
writeRecordingEventActivity (RecordingEventId reId) (RecordingActivityId raId) = execState writeTriples []
 where
  writeTriples :: State Triples ()
  writeTriples = do
    let recordingEventUri = mkRecordingEventUri reId
        recordingActivityUri = mkRecordingActivityUri raId
    append $ mkTriple recordingEventUri crmP9 recordingActivityUri

writeRecordingActivityPersonDeclaration :: RecordingActivityId -> PersonId -> RoleId -> Triples
writeRecordingActivityPersonDeclaration (RecordingActivityId raId) (PersonId pId) (RoleId rId) =
  execState writeTriples []

 where
  writeTriples :: State Triples ()
  writeTriples = do
    let recordingActivityUri = mkRecordingActivityUri raId
        recordingActivityCarriedOutByUri = mkRecordingActivityCarriedOutByUri raId
        personUri = mkPersonUri pId
        roleUri = mkRoleUri rId
    append $ mkTriple recordingActivityCarriedOutByUri crmP01 recordingActivityUri
    append $ mkTriple recordingActivityCarriedOutByUri crmP02 personUri
    append $ mkTriple recordingActivityCarriedOutByUri crmP14_1 roleUri

writeRecordingActivityLegalBodyDeclaration :: RecordingActivityId -> LegalBodyId -> RoleId -> Triples
writeRecordingActivityLegalBodyDeclaration (RecordingActivityId raId) (LegalBodyId lbId) (RoleId rId) =
  execState writeTriples []

 where
  writeTriples :: State Triples ()
  writeTriples = do
    let recordingActivityUri = mkRecordingActivityUri raId
        recordingActivityCarriedOutByUri = mkRecordingActivityCarriedOutByUri raId
        legalBodyUri = mkLegalBodyUri lbId
        roleUri = mkRoleUri rId
    append $ mkTriple recordingActivityCarriedOutByUri crmP01 recordingActivityUri
    append $ mkTriple recordingActivityCarriedOutByUri crmP02 legalBodyUri
    append $ mkTriple recordingActivityCarriedOutByUri crmP14_1 roleUri

writeGenreCategoryDeclaration :: GenreCategory -> Triples
writeGenreCategoryDeclaration (GenreCategory (GenreCategoryId gcId) gcName)
  = let genreUri            = mkGenreCategoryUri gcId
        identifierGenreUri  = mkGenreCategoryIdentifierUri gcId
        appellationGenreUri = mkGenreCategoryAppellationUri gcId
    in  catMaybes
          [ mkTriple genreUri rdfType crmE55
          , mkTriple genreUri crmP2   genreCategoryTypeUri
          , mkTripleLit genreUri rdfsLabel (RDF.PlainLL gcName "fr")
          , mkTriple genreUri           crmP48  identifierGenreUri
          , mkTriple genreUri           crmP1   appellationGenreUri
          , mkTriple identifierGenreUri rdfType crmE42
          , mkTripleLit identifierGenreUri crmP190 (RDF.PlainL gcId)
          , mkTriple appellationGenreUri rdfType crmE41
          , mkTripleLit appellationGenreUri
                        crmP190
                        (RDF.PlainL gcName)
          ]

writeGenreCategoryWikidataLink :: GenreCategoryId -> WikidataUri -> Triples
writeGenreCategoryWikidataLink (GenreCategoryId gcId) (WikidataUri wikidataUri) =
  execState writeTriples []

 where
  writeTriples :: State Triples ()
  writeTriples = do
    let genreCategoryUri = mkGenreCategoryUri gcId
    append $ mkTriple genreCategoryUri owlSameAs wikidataUri

writeLanguageDeclaration :: Language -> Triples
writeLanguageDeclaration language = execState writeTriples []

 where
  writeTriples :: State Triples ()
  writeTriples = do
    let lid                    = unLanguageId $ languageId language
        lname                  = languageName language
        languageUri            = mkLanguageUri lid
        identifierLanguageUri  = mkLanguageIdentifierUri lid
        appellationLanguageUri = mkLanguageAppellationUri lid

    append $ mkTriple languageUri rdfType crmE56
    append $ mkTripleLit languageUri rdfsLabel (RDF.PlainLL lname "fr")
    append $ mkTriple languageUri crmP48 identifierLanguageUri
    append $ mkTriple languageUri crmP1 appellationLanguageUri

    append $ mkTriple identifierLanguageUri rdfType crmE42
    append $ mkTripleLit identifierLanguageUri crmP190 (RDF.PlainL lid)

    append $ mkTriple appellationLanguageUri rdfType crmE41
    append $ mkTripleLit appellationLanguageUri crmP190 (RDF.PlainL lname)

writeLanguageWikidataLink :: LanguageId -> WikidataUri -> Triples
writeLanguageWikidataLink (LanguageId langId) (WikidataUri wikidataUri) =
  execState writeTriples []

 where
  writeTriples :: State Triples ()
  writeTriples = do
    let languageUri = mkLanguageUri langId
    append $ mkTriple languageUri owlSameAs wikidataUri

writeLegalBodyDeclaration :: LegalBody -> Triples
writeLegalBodyDeclaration legalBody = execState writeTriples []

 where
  writeTriples :: State Triples ()
  writeTriples = do
    let lbid                    = unLegalBodyId $ legalBodyId legalBody
        lbname                  = legalBodyName legalBody
        legalBodyUri            = mkLegalBodyUri lbid
        identifierLegalBodyUri  = mkLegalBodyIdentifierUri lbid
        appellationLegalBodyUri = mkLegalBodyAppellationUri lbid

    append $ mkTriple legalBodyUri rdfType crmE40
    append $ mkTripleLit legalBodyUri rdfsLabel (RDF.PlainL lbname)
    append $ mkTriple legalBodyUri crmP48 identifierLegalBodyUri
    append $ mkTriple legalBodyUri crmP1 appellationLegalBodyUri

    append $ mkTriple identifierLegalBodyUri rdfType crmE42
    append $ mkTripleLit identifierLegalBodyUri crmP190 (RDF.PlainL lbid)

    append $ mkTriple appellationLegalBodyUri rdfType crmE41
    append $ mkTripleLit appellationLegalBodyUri crmP190 (RDF.PlainL lbname)

writePlaceDeclaration :: Place -> Triples
writePlaceDeclaration (Place (PlaceId pid) pName) =
  execState writeTriples []

 where
  writeTriples :: State Triples ()
  writeTriples = do
    let placeUri       = mkPlaceUri pid
        appellationUri = mkPlaceAppellationUri pid
        identifierUri  = mkPlaceIdentifierUri pid

    append $ mkTriple placeUri rdfType crmE53
    append $ mkTripleLit placeUri rdfsLabel (RDF.PlainLL pName "fr")
    append $ mkTriple placeUri crmP1 appellationUri
    append $ mkTriple placeUri crmP48 identifierUri

    append $ mkTriple appellationUri rdfType crmE41
    append $ mkTripleLit appellationUri crmP190 (RDF.PlainL pName)

    append $ mkTriple identifierUri rdfType crmE42
    append $ mkTripleLit identifierUri crmP190 (RDF.PlainL pid)

writePlaceWikidataLink :: PlaceId -> WikidataUri -> Triples
writePlaceWikidataLink (PlaceId pId) (WikidataUri wikidataUri) =
  execState writeTriples []

 where
  writeTriples :: State Triples ()
  writeTriples = do
    let placeUri = mkPlaceUri pId
    append $ mkTriple placeUri owlSameAs wikidataUri

writeSynopsisDeclaration :: Synopsis -> Triples
writeSynopsisDeclaration (Synopsis (SynopsisId sid) content (LanguageId langId)) =
  execState writeTriples []

 where
  writeTriples :: State Triples ()
  writeTriples = do
    let synopsisUri = mkSynopsisUri sid
        languageUri = mkLanguageUri langId

    append $ mkTriple synopsisUri rdfType crmE33
    append $ mkTriple synopsisUri crmP2 synopsisTypeUri
    append $ mkTriple synopsisUri crmP72 languageUri
    append $ mkTripleLit synopsisUri crmP190 (RDF.PlainL content)

writeSynopsisTranslation :: SynopsisId -> SynopsisId -> Triples
writeSynopsisTranslation (SynopsisId sid) (SynopsisId osid) =
  execState writeTriples []

 where
  writeTriples :: State Triples ()
  writeTriples = do
    let synopsisUri = mkSynopsisUri sid
        otherSynopsisUri = mkSynopsisUri osid

    append $ mkTriple synopsisUri crmP73 otherSynopsisUri

writeManifestationProductTypeDeclaration :: ManifestationProductTypeId -> Triples
writeManifestationProductTypeDeclaration (ManifestationProductTypeId manifProdTypeId) =
  execState writeTriples []

 where
  writeTriples :: State Triples ()
  writeTriples = do
    let manifProductTypeUri = mkManifestationProductTypeUri manifProdTypeId
    append $ mkTriple manifProductTypeUri rdfType frbrooF3

writeManifestationProductTypeCarriesPublicationExpression :: ManifestationProductTypeId -> PublicationExpressionId -> Triples
writeManifestationProductTypeCarriesPublicationExpression (ManifestationProductTypeId manifProdTypeId) (PublicationExpressionId pubExprId) =
  execState writeTriples []

 where
  writeTriples :: State Triples ()
  writeTriples = do
    let manifProductTypeUri = mkManifestationProductTypeUri manifProdTypeId
        pubExprUri = mkPublicationExpressionUri pubExprId
    append $ mkTriple manifProductTypeUri frbrooCLR6 pubExprUri

writeManifestationProductTypeDuration :: ManifestationProductTypeId -> Int -> Triples
writeManifestationProductTypeDuration (ManifestationProductTypeId manifProdTypeId) duration =
  execState writeTriples []

 where
  writeTriples :: State Triples ()
  writeTriples = do
    let manifProductTypeUri = mkManifestationProductTypeUri manifProdTypeId
        totalSecondsText = T.pack $ show duration
        durationUri = mkDurationUri $ "Dimension" <> totalSecondsText <> "Seconds"
    append $ mkTriple manifProductTypeUri crmP43 durationUri
    append $ mkTriple durationUri rdfType crmE54
    append $ mkTriple durationUri crmP2 durationTypeUri
    append $ mkTripleLit durationUri crmP90 (RDF.TypedL totalSecondsText xsdInteger)
    append $ mkTriple durationUri crmP91 unitSEC
