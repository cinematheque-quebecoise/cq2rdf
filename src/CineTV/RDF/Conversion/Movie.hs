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

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module CineTV.RDF.Conversion.Movie
  ( convertMovies
  )
where

import           Data.RDF.Types.Extended      (mkTriple, mkTripleLit)
import           Database.CineTv.Public.Model
import           Import                       hiding ((^.))
import Data.RDF.Vocabulary
import           Util                         (parseDateField, parseYearField,
                                               sqlKeyToText, utcTimeToText)

import           Data.Pool                    (Pool)
import qualified Data.RDF                     as RDF
import           Data.RDF.State
import           Data.Time.Clock              (UTCTime (..))
import           Database.Esqueleto           hiding (get)
import qualified RIO.Text                     as Text

baseUriPath :: Text
baseUriPath = "/resource"

{-|
Create all triples for representing the Recording Work concept for all rows in table Filmo.

Generated triples:

@
for each row in table Filmo
    cmtq:Work{Filmo.FilmoID} rdf:type frbroo:F1_Work
    cmtq:Work{Filmo.FilmoID} rdfs:label {Filmo.PrefixeTitreOriginal + Filmo.TitreOriginal}
    cmtq:Work{Filmo.FilmoID} crm:P102_has_title cmtq:OriginalTitleWork{Filmo.FilmoId}
    cmtq:Work{Filmo.FilmoID} crm:P48_has_preferred_identifier cmtq:IdentifierWork{Filmo.FilmoId}

    cmtq:Work{Filmo.FilmoID} cmtqo:release_event cmtq:WorkPublicRelease{Filmo.FilmoID}
    cmtq:WorkPublicRelease{Filmo.FilmoID} rdf:type cmtqo:Work_Public_Release
    cmtq:WorkPublicRelease{Filmo.FilmoID} crm:P4_has_time-span cmtq:Time-Span{Filmo.AnneeSortie}
    cmtq:Time-Span{Filmo.AnneeSortie} crm:P79_beginning_is_qualified_by {Filmo.AnneeSortie}^^xsd:dateTime
    cmtq:RecordingWork{Filmo.FilmoID} frbroo:R22_created_a_realization_of cmtq:RecordingEvent{Filmo.FilmoID}
    cmtq:RecordingEvent{Filmo.FilmoID} crm:P4_has_time-span cmtq:Time-Span{Filmo.AnneeDebProd ou Filmo.DateDebProd + Filmo.AnneeFinProd ou Filmo.DateFinProd}
    cmtq:Time-Span{Filmo.AnneeDebProd ou Filmo.DateDebProd + Filmo.AnneeFinProd ou Filmo.DateFinProd}la crm:P79_beginning_is_qualified_by {Filmo.AnneeDebProd ou Filmo.DateDebProd}
    cmtq:Time-Span{Filmo.AnneeDebProd ou Filmo.DateDebProd + Filmo.AnneeFinProd ou Filmo.DateFinProd}la crm:P80_end_is_qualified_by {Filmo.AnneeFinProd ou Filmo.DateFinProd}

    cmtq:OriginalTitleWork{Filmo.FilmoId} rdf:type crm:E35_Title
    cmtq:OriginalTitleWork{Filmo.FilmoId} crm:P190_has_symbolic_content {Filmo.PrefixeTitreOriginal + Filmo.TitreOriginal}

    cmtq:IdentifierWork{Filmo.FilmoId} rdf:type crm:E42_Identifier
    cmtq:IdentifierWork{Filmo.FilmoId} crm:P190_has_symbolic_content {Filmo.FilmoId}

for each row in table Filmo_LienWikidata
  cmtq:Work{Filmo_LienWikidata.FilmoID} owl:sameAs {Filmo_LienWikidata.LienWikidata}
@
-}
convertMovies
  :: (RDF.Rdf rdfImpl, MonadIO m) => Pool SqlBackend -> RdfState rdfImpl m ()
convertMovies pool = do
  createCurrencies
  createTriplesFromMovies pool
  createTriplesFromAllFilmoLienWikidata pool

createCurrencies :: (RDF.Rdf rdfImpl, Monad m) => RdfState rdfImpl m ()
createCurrencies = do
  let cadUri = "/resource/CanadianDollar"
  mapM_ addTriple $ mkTriple cadUri rdfType crmE98
  mapM_ addTriple
    $ mkTripleLit cadUri rdfsLabel (RDF.PlainLL "Dollar canadien" "fr")

createTriplesFromMovies
  :: (RDF.Rdf rdfImpl, MonadIO m) => Pool SqlBackend -> RdfState rdfImpl m ()
createTriplesFromMovies pool = do
  filmoEntities <- getFilmoEntities pool
  mapM_ createTriplesFromMovie filmoEntities

getFilmoEntities :: (MonadIO m) => Pool SqlBackend -> m [Entity Filmo]
getFilmoEntities pool =
  liftIO $ flip liftSqlPersistMPool pool $ select $ distinct $ from return

createTriplesFromMovie
  :: (RDF.Rdf rdfImpl, Monad m) => Entity Filmo -> RdfState rdfImpl m ()
createTriplesFromMovie filmoEntity = do
  let filmoId          = sqlKeyToText $ entityKey filmoEntity
  let filmo            = entityVal filmoEntity
  let filmoUri         = baseUriPath <> "/Work" <> filmoId
  let originalTitleUri = baseUriPath <> "/OriginalTitleWork" <> filmoId
  let identifierUri    = baseUriPath <> "/IdentifierWork" <> filmoId

  mapM_ addTriple $ mkTriple filmoUri rdfType frbrooF1

  let comment = "Oeuvre audiovisuelle avec l'identifiant " <> filmoId
  mapM_ addTriple $ mkTripleLit filmoUri rdfsComment (RDF.PlainL comment)

  let titleMaybe = mkOriginalTitleWork filmo

  forM_ titleMaybe $ \title -> do
    mapM_ addTriple $ mkTripleLit filmoUri rdfsLabel (RDF.PlainL title)

    mapM_ addTriple $ mkTriple filmoUri crmP102 originalTitleUri

    mapM_ addTriple $ mkTriple originalTitleUri rdfType crmE35
    mapM_ addTriple $ mkTripleLit originalTitleUri crmP190 (RDF.PlainL title)

  mapM_ addTriple $ mkTriple filmoUri crmP48 identifierUri

  mapM_ addTriple $ mkTriple identifierUri rdfType crmE42
  mapM_ addTriple $ mkTripleLit identifierUri crmP190 (RDF.PlainL filmoId)

  let budgetUri = baseUriPath <> "/Budget"
  let cadUri    = baseUriPath <> "/CanadianDollar"
  forM_ (filmoCout filmo) $ \movieCost -> do
    let movieCostText      = Text.pack $ show movieCost
    let budgetWorkUriLabel = "Dimension" <> movieCostText <> "CAD"
    let budgetWorkUri      = baseUriPath <> "/" <> budgetWorkUriLabel

    mapM_ addTriple $ mkTriple filmoUri crmP43 budgetWorkUri

    mapM_ addTriple $ mkTriple budgetWorkUri rdfType crmE54
    mapM_ addTriple $ mkTriple budgetWorkUri crmP2 budgetUri
    mapM_ addTriple
      $ mkTripleLit budgetWorkUri rdfsLabel (RDF.PlainL budgetWorkUriLabel)
    mapM_ addTriple $ mkTripleLit budgetWorkUri
                                  crmP181
                                  (RDF.TypedL movieCostText "xsd:double")
    mapM_ addTriple $ mkTriple budgetWorkUri crmP180 cadUri

  let recordingWorkUri    = baseUriPath <> "/RecordingWork" <> filmoId
  let recordingEventUri   = baseUriPath <> "/RecordingEvent" <> filmoId
  let recordingUri        = baseUriPath <> "/Recording" <> filmoId
  let publicationExprUri  = baseUriPath <> "/PublicationExpression" <> filmoId
  let publicationEventUri = baseUriPath <> "/PublicationEvent" <> filmoId
  let publicProjectionEventUri =
        baseUriPath <> "/PublicProjectionEvent" <> filmoId

  mapM_ addTriple $ mkTriple recordingWorkUri rdfType frbrooF21
  mapM_ addTriple $ mkTriple recordingWorkUri frbrooR2 filmoUri

  mapM_ addTriple $ mkTriple recordingEventUri rdfType frbrooF29
  mapM_ addTriple $ mkTriple recordingEventUri frbrooR22 recordingWorkUri
  mapM_ addTriple $ mkTriple recordingEventUri frbrooR21 recordingUri

  mapM_ addTriple $ mkTriple recordingUri rdfType frbrooF26
  mapM_ addTriple $ mkTriple publicationExprUri crmP165 recordingUri

  mapM_ addTriple $ mkTriple publicationEventUri rdfType frbrooF30
  mapM_ addTriple $ mkTriple publicationEventUri frbrooR24 publicationExprUri

  forM_ (filmoAnneeSortie filmo >>= parseYearField) $ \releaseYear -> do
    mapM_ addTriple $ mkTriple publicationExprUri rdfType frbrooF24

    mapM_ addTriple $ mkTriple publicProjectionEventUri rdfType crmE7
    mapM_ addTriple $ mkTriple publicProjectionEventUri
                               crmP2
                               (baseUriPath <> "/PublicProjectionEvent")
    mapM_ addTriple
      $ mkTriple publicProjectionEventUri crmP16 publicationExprUri

    mapM_ addTriple
      $ mkTriple publicationEventUri crmP183 publicProjectionEventUri

    let releaseYearLit = utcTimeToText releaseYear
    let timeSpanUri    = baseUriPath <> "/Time-Span" <> releaseYearLit
    mapM_ addTriple $ mkTriple publicProjectionEventUri crmP4 timeSpanUri
    mapM_ addTriple $ mkTripleLit timeSpanUri
                                  crmP82a
                                  (RDF.TypedL releaseYearLit xsdDateTime)

  createProdEventTimeSpanTriples filmoEntity

createProdEventTimeSpanTriples
  :: (RDF.Rdf rdfImpl, Monad m) => Entity Filmo -> RdfState rdfImpl m ()
createProdEventTimeSpanTriples filmoEntity = do
  let beginDateTimeMaybe = dateBeginMaybe <|> yearBeginMaybe
  let endDateTimeMaybe   = dateEndMaybe <|> yearEndMaybe

  createResourceTimeSpanTriples (baseUriPath <> "/RecordingEvent" <> filmoId)
                                beginDateTimeMaybe
                                endDateTimeMaybe

 where
  filmoId        = sqlKeyToText $ entityKey filmoEntity
  filmo          = entityVal filmoEntity

  -- parseTimeM False defaultTimeLocale "%d-%-m-%-y" "09-09-89" :: Maybe UTCTime
  dateBeginMaybe = filmoDateDebProd filmo >>= parseDateField
  dateEndMaybe   = filmoDateFinProd filmo >>= parseDateField

  yearBeginMaybe = filmoAnneeDebProd filmo >>= parseYearField
  yearEndMaybe   = filmoAnneeFinProd filmo >>= parseYearField

-- | From a given resource, create a time-span concept using given begin and
-- end dates (optional values).
createResourceTimeSpanTriples
  :: (RDF.Rdf rdfImpl, Monad m)
  => Text -- ^ Resource URI
  -> Maybe UTCTime -- ^ Optional begin date in UTCTime format
  -> Maybe UTCTime -- ^ Optional end date in UTCTime format
  -> RdfState rdfImpl m ()
createResourceTimeSpanTriples resourceUri beginDateMaybe endDateMaybe =
  when (isJust $ beginDateMaybe <|> endDateMaybe) $ do
    let lit = Text.intercalate "-" $ utcTimeToText <$> catMaybes
          [beginDateMaybe, endDateMaybe]
    let resourceTimeSpanUri = baseUriPath <> "/Time-Span" <> lit
    -- let resourceTimePrimitiveUri = baseUriPath <> "/Time_Primitive" <> lit

    addTriple $ RDF.triple (RDF.unode resourceUri)
                           (RDF.unode crmP4)
                           (RDF.unode resourceTimeSpanUri)
    addTriple $ RDF.triple (RDF.unode resourceTimeSpanUri)
                           (RDF.unode crmP4i)
                           (RDF.unode resourceUri)

    case beginDateMaybe of
      Just beginDate -> addTriple $ RDF.triple
        (RDF.unode resourceTimeSpanUri)
        (RDF.unode crmP79)
        (RDF.lnode $ RDF.TypedL (utcTimeToText beginDate) xsdDateTime)
      Nothing -> return ()

    case endDateMaybe of
      Just endDate -> addTriple $ RDF.triple
        (RDF.unode resourceTimeSpanUri)
        (RDF.unode crmP80)
        (RDF.lnode $ RDF.TypedL (utcTimeToText endDate) xsdDateTime)
      Nothing -> return ()

createTriplesFromAllFilmoLienWikidata
  :: (RDF.Rdf rdfImpl, MonadIO m) => Pool SqlBackend -> RdfState rdfImpl m ()
createTriplesFromAllFilmoLienWikidata pool = do
  filmoLienWikidataEntities <- getAllFilmoLienWikidataEntities pool
  mapM_ createTriplesFromFilmoLienWikidata filmoLienWikidataEntities

getAllFilmoLienWikidataEntities
  :: (MonadIO m) => Pool SqlBackend -> m [Entity Filmo_LienWikidata]
getAllFilmoLienWikidataEntities pool =
  liftIO $ flip liftSqlPersistMPool pool $ select $ distinct $ from return

createTriplesFromFilmoLienWikidata
  :: (RDF.Rdf rdfImpl, MonadIO m)
  => Entity Filmo_LienWikidata
  -> RdfState rdfImpl m ()
createTriplesFromFilmoLienWikidata filmoLienWdEntity = do
  let wikidataUriMaybe =
        filmo_LienWikidataLienWikidata $ entityVal filmoLienWdEntity

  forM_ wikidataUriMaybe $ \wikidataUri -> do
    let filmoId =
          sqlKeyToText $ filmo_LienWikidataFilmoId $ entityVal filmoLienWdEntity
    let filmoUri = baseUriPath <> "/Work" <> filmoId
    mapM_ addTriple $ mkTriple filmoUri owlSameAs wikidataUri

mkOriginalTitleWork :: Filmo -> Maybe Text
mkOriginalTitleWork filmo = do
  restTitle <- filmoTitreOriginal filmo
  case filmoPrefixeTitreOriginal filmo of
    Just prefixTitle -> if Text.isSuffixOf "'" prefixTitle
      then Just $ prefixTitle <> "" <> restTitle
      else Just $ prefixTitle <> " " <> restTitle
    Nothing -> Just restTitle
