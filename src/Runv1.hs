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

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
module Runv1
  ( run
  )
where

import           Data.RDF.State
import           Database.CineTv.Public.Model
import           Import                            hiding ((^.))
import           Namespaces
import qualified SW.Vocabulary                     as SW
import           Util                              (getCurrentDayText,
                                                    sqlKeyToText)

import           Codec.Compression.GZip            (compress)
import           Control.Monad.State               (execStateT)
import           Data.Aeson                        (ToJSON (..))
import           Data.Aeson.Text                   (encodeToLazyText)
import qualified Data.ByteString.Lazy              as BS
import           Data.Pool                         (Pool)
import           Data.RDF                          (RDF)
import qualified Data.RDF                          as RDF
import qualified Data.RDF.Namespace                as RDF
import qualified Data.Text.Lazy.IO                 as TextL
import           Data.Time.Clock                   (UTCTime (..),
                                                    getCurrentTime)
import           Data.Time.Format                  (defaultTimeLocale,
                                                    parseTimeM)
import           Database.Esqueleto                hiding (get)
import           Database.Persist.Sqlite           (SqliteConf (..))
import qualified RIO.Text                          as Text
import           System.Directory                  (createDirectoryIfMissing,
                                                    doesFileExist, removeFile)
import           System.FilePath                   (joinPath)
import           System.Process                    (callCommand)
import           Text.RDF.RDF4H.NTriplesSerializer
import           Text.RDF.RDF4H.TurtleSerializer
import           Text.RE.TDFA.Text
import           Text.XML.XSD                      (fromUTCTime)

-- Used for converting the prefix mappings in JSON format
instance ToJSON RDF.PrefixMappings

data Metadata = Metadata
    { metadataOriginalDate :: Text
    , metadataCreationDate :: Text
    }
    deriving (Generic)
instance ToJSON Metadata

-- run :: RIO App ()
-- run = do
--   let emptyRdf = RDF.empty :: RDF RDF.TList
--   let mappings = RDF.PrefixMappings $ Map.fromList [ ("mo", "http://purl.org/ontology/mo/") ]
--   let baseUrl = Just $ "http://example.org/resource"
--   liftIO $ RDF.writeRdf (TurtleSerializer baseUrl mappings) emptyRdf

baseUriPath :: Text
baseUriPath = "/resource"

-- | The director role does not exist in CineTV database.
-- Has to be manually created.
customDirectorRoleId :: Int64
customDirectorRoleId = 1

run :: RIO App ()
run = do
  env <- ask

  let sqliteDbPath = optionsSqlitePath $ appOptions env
  doesSqlitePathExist <- liftIO $ doesFileExist $ Text.unpack sqliteDbPath
  unless doesSqlitePathExist $ do
    logError $ display $ "Can't find SQLite database at: " <> sqliteDbPath
    exitFailure
  pool <- liftIO $ createPoolConfig (SqliteConf sqliteDbPath 1)

  logInfo "Converting CineTV in RDF..."

  originalDate <- getSqliteDbDate
  creationDate <- liftIO $ fmap getCurrentDayText getCurrentTime

  -- let emptyRdf = RDF.empty :: RDF RDF.TList
  let baseUri  = optionsBaseUri $ appOptions env
  -- let baseUriJust = Just $ RDF.BaseUrl baseUri
  let emptyRdf = RDF.mkRdf [] Nothing prefixMappings :: RDF RDF.TList
  graph <- liftIO $ execStateT (cinetv2rdf pool) emptyRdf

  let outputDir = Text.pack $ joinPath
        [Text.unpack $ optionsOutputDir $ appOptions env, "cmtq-dataset"]
  liftIO $ createDirectoryIfMissing True $ Text.unpack outputDir

  -- let prefixesFpath = Text.unpack $ outputDir <> "/prefixes.json"
  -- liftIO $ TextL.writeFile prefixesFpath $ encodeToLazyText $ RDF.prefixMappings graph

  let metadataFpath = Text.unpack $ outputDir <> "/metadata.json"
  liftIO $ TextL.writeFile metadataFpath $ encodeToLazyText $ Metadata
    originalDate
    creationDate

  let cmtqTurtleFpath = Text.unpack $ outputDir <> "/cmtq-dataset.ttl"
  liftIO $ withFile
    cmtqTurtleFpath
    WriteMode
    (\h -> RDF.hWriteRdf (TurtleSerializer Nothing prefixMappings) h graph)

  let cmtqNtriplesFpath = Text.unpack $ outputDir <> "/cmtq-dataset.nt"
  liftIO $ withFile cmtqNtriplesFpath
                    WriteMode
                    (\h -> RDF.hWriteRdf NTriplesSerializer h graph)

  -- Compress output Turtle file
  let cmtqTurtleFpathCompressed = cmtqTurtleFpath <> ".gz"
  liftIO
    $   BS.readFile cmtqTurtleFpath
    >>= (return . compress)
    >>= BS.writeFile cmtqTurtleFpathCompressed
  liftIO $ removeFile cmtqTurtleFpath

  -- Compress output N-triples file
  let cmtqNtriplesFpathCompressed = cmtqNtriplesFpath <> ".gz"
  liftIO
    $   BS.readFile cmtqNtriplesFpath
    >>= (return . compress)
    >>= BS.writeFile cmtqNtriplesFpathCompressed
  liftIO $ removeFile cmtqNtriplesFpath

  let cmtqHdtFpath = Text.unpack $ outputDir <> "/cmtq-dataset.hdt"
  let rdf2hdtCmd =
        "rdf2hdt -B "
          <> Text.unpack baseUri
          <> "/resource -f ttl "
          <> cmtqNtriplesFpathCompressed
          <> " "
          <> cmtqHdtFpath
  liftIO $ callCommand rdf2hdtCmd

  logInfo "Finished!"

getSqliteDbDate :: RIO App Text
getSqliteDbDate = do
  sqliteDbPath <- fmap (optionsSqlitePath . appOptions) ask
  case matchedText $ sqliteDbPath ?=~ [re|[0-9]+-[0-9]+-[0-9]+|] of
    Just date -> return date
    Nothing   -> do
      logError "The SQLite file must contain a date in the format YYYY-MM-DD"
      exitFailure

cinetv2rdf :: (MonadIO m) => Pool SqlBackend -> RdfState RDF.TList m ()
cinetv2rdf pool = do
  fonctionEntities <- getFonctionEntities pool
  createRolesTriples fonctionEntities

  nomEntities <- getNomEntities pool
  createPeopleTriples nomEntities

  nomLienWikidata <- getNomLienWikidata pool
  mkAllNomLienWikidataRdf nomLienWikidata

  legalBodies <- getLegalBodies pool
  createLegalBodiesTriples legalBodies

  movieCategories <- getMovieCategories pool
  createMovieCategoriesTriples movieCategories

  places <- getPlaces pool
  createPlacesTriples places

  paysLienWikidata <- getPaysLienWikidata pool
  mkAllPaysLienWikidataRdf paysLienWikidata

  langue <- getLangue pool
  mkAllLangueRdf langue

  langueLienWikidata <- getLangueLienWikidata pool
  mkAllLangueLienWikidataRdf langueLienWikidata

  filmos <- getFilmos pool
  createFilmosTriples filmos

  filmoLienWikidata <- getFilmoLienWikidata pool
  mkAllFilmoLienWikidataRdf filmoLienWikidata

  recordingRoleActivities <- getRecordingRoleActivities pool
  createRecordingRoleActivitiesTriples recordingRoleActivities

  filmosSubject <- getFilmosSubject pool
  createFilmosSubjectTriples filmosSubject

  filmosDirector <- getFilmosDirector pool
  createFilmosDirectorTriples filmosDirector

  filmosPlaces <- getFilmosPlaces pool
  createFilmosPlacesTriples filmosPlaces

  filmoResumes <- getFilmoResumes pool
  mkAllFilmoResumesRdf filmoResumes

  filmoResumesAnglais <- getFilmoResumesAnglais pool
  mkAllFilmoResumesAnglaisRdf filmoResumesAnglais

  filmoLangue <- getFilmoLangue pool
  mkAllFilmoLangueRdf filmoLangue

  return ()

-- |Get all rows from Nom table in database.
getNomEntities :: (MonadIO m) => Pool SqlBackend -> m [Entity Nom]
getNomEntities pool =
  liftIO $ flip liftSqlPersistMPool pool $ select $ distinct $ from $ \nom ->
    return nom

-- | Create all triples for representing the Person concept for all row in
-- table Nom.
--
-- Generated triples:
--   cmtq:Person{Nom.NomID} rdf:type crm:E21_Person
--   cmtq:Person{Nom.NomID} cmtqo:cmtq_id {Nom.NomID}
--   cmtq:Person{Nom.NomID} foaf:name {Nom.Prenom + Nom.Nom}
--   cmtq:Person{Nom.NomID} rdfs:label {Nom.Prenom + Nom.Nom}
--   cmtq:Person{Nom.NomID} foaf:familyName {Nom.Nom}
--   cmtq:Person{Nom.NomID} foaf:givenName {Nom.Prenom}
createPeopleTriples
  :: (RDF.Rdf rdfImpl, Monad m) => [Entity Nom] -> RdfState rdfImpl m ()
createPeopleTriples = mapM_ createPersonTriples

-- | Create all triples for representing the Person concept.
createPersonTriples
  :: (RDF.Rdf rdfImpl, Monad m) => Entity Nom -> RdfState rdfImpl m ()
createPersonTriples nomEntity = do
  let personUri = baseUriPath <> "/Person" <> personTextId

  addTriple $ RDF.triple (RDF.unode personUri)
                         (RDF.unode SW.rdfType)
                         (RDF.unode SW.crmE21)

  addTriple $ RDF.triple (RDF.unode personUri)
                         (RDF.unode $ RDF.mkUri cmtqo "cmtq_id")
                         (RDF.lnode $ RDF.PlainL personTextId)

  case nameOpt of
    Just name -> do
      addTriple $ RDF.triple (RDF.unode personUri)
                             (RDF.unode SW.foafName)
                             (RDF.lnode $ RDF.PlainL name)
      addTriple $ RDF.triple (RDF.unode personUri)
                             (RDF.unode SW.rdfsLabel)
                             (RDF.lnode $ RDF.PlainL name)
    Nothing -> return ()

  case lastname of
    Just l -> addTriple $ RDF.triple (RDF.unode personUri)
                                     (RDF.unode SW.foafFamilyName)
                                     (RDF.lnode $ RDF.PlainL l)
    Nothing -> return ()

  case firstname of
    Just f -> addTriple $ RDF.triple (RDF.unode personUri)
                                     (RDF.unode SW.foafGivenName)
                                     (RDF.lnode $ RDF.PlainL f)
    Nothing -> return ()

 where
  personTextId = sqlKeyToText $ entityKey nomEntity
  firstname    = nomPrenom $ entityVal nomEntity
  lastname     = nomNom $ entityVal nomEntity
  nameOpt      = case (firstname, lastname) of
    (f@(Just _), l@(Just _)) -> f <> Just " " <> l
    (f@(Just _), Nothing   ) -> f
    (Nothing   , l@(Just _)) -> l
    (Nothing   , Nothing   ) -> Nothing

-- | Get all rows from Nom table in database.
getFonctionEntities :: (MonadIO m) => Pool SqlBackend -> m [Entity Fonction]
getFonctionEntities pool =
  liftIO
    $ flip liftSqlPersistMPool pool
    $ select
    $ distinct
    $ from
    $ \fonction -> return fonction

-- | Create all triples for representing the Role concept for all rows in
-- table Fonction.
--
-- Generated triples:
--   cmtq:Fonction{Fonction.FonctionID} rdf:type cmtqo:Role
--   cmtq:Fonction{Fonction.FonctionID} cmtqo:cmtq_id {Fonction.FonctionID}
--   cmtq:Fonction{Fonction.FonctionID} rdfs:label {Fonction.Terme}
createRolesTriples
  :: (RDF.Rdf rdfImpl, Monad m) => [Entity Fonction] -> RdfState rdfImpl m ()
createRolesTriples fonctionEntities = do
  mapM_ createRoleTriples fonctionEntities

  -- The director role does not exist in CineTV database.
  -- Has to be manually created.
  createRoleTriples
    $ Entity (toSqlKey customDirectorRoleId) (Fonction "Réalisation")

-- | Create all triples for representing the Role concept.
createRoleTriples
  :: (RDF.Rdf rdfImpl, Monad m) => Entity Fonction -> RdfState rdfImpl m ()
createRoleTriples fonctionEntity = do
  let roleUri = baseUriPath <> "/Role" <> roleTextId

  addTriple $ RDF.triple (RDF.unode roleUri)
                         (RDF.unode SW.rdfType)
                         (RDF.unode $ RDF.mkUri cmtqo "Role")

  addTriple $ RDF.triple (RDF.unode roleUri)
                         (RDF.unode $ RDF.mkUri cmtqo "cmtq_id")
                         (RDF.lnode $ RDF.PlainL roleTextId)

  addTriple $ RDF.triple (RDF.unode roleUri)
                         (RDF.unode SW.rdfsLabel)
                         (RDF.lnode $ RDF.PlainL roleLabel)

 where
  roleTextId = (sqlKeyToText . entityKey) fonctionEntity
  roleLabel  = (fonctionTerme . entityVal) fonctionEntity

-- | Get all rows from Sujet table in database that are potentially a legal
-- body.
getLegalBodies :: (MonadIO m) => Pool SqlBackend -> m [Entity Sujet]
getLegalBodies pool = do
  results <-
    liftIO
    $ flip liftSqlPersistMPool pool
    $ select
    $ distinct
    $ from
    $ \(filmoGenerique, sujet) -> do
        where_
          (sujet ?. SujetId ==. filmoGenerique ^. Filmo_GeneriqueOrganismeId)
        return sujet

  return $ catMaybes results

-- | Create all triples for representing the Legal Body concept for all rows
-- in table Sujet.
--
-- Generated triples:
-- Each row in table Sujet where Sujet.SujetId appears in Filmo_Generique.OrganismeID
--   cmtq:LegalBody{Sujet.SujetID} rdf:type crm:E40_Legal_Body
--   cmtq:LegalBody{Sujet.SujetID} rdfs:label {Sujet.Terme}
createLegalBodiesTriples
  :: (RDF.Rdf rdfImpl, Monad m) => [Entity Sujet] -> RdfState rdfImpl m ()
createLegalBodiesTriples = mapM_ createLegalBodyTriples

-- | Create all triples for representing the Legal Body concept.
-- The following roles are found in table Sujet: Financement, Laboratoire,
-- Maison de services, Personnages, Société d'exportation, Société de
-- distribution, société de production, télédiffuseur.
createLegalBodyTriples
  :: (RDF.Rdf rdfImpl, Monad m) => Entity Sujet -> RdfState rdfImpl m ()
createLegalBodyTriples subjectEntity = do
  let legalBodyUri = baseUriPath <> "/LegalBody" <> legalBodyId

  addTriple $ RDF.triple (RDF.unode legalBodyUri)
                         (RDF.unode SW.rdfType)
                         (RDF.unode SW.crmE40)

  addTriple $ RDF.triple (RDF.unode legalBodyUri)
                         (RDF.unode SW.rdfsLabel)
                         (RDF.lnode $ RDF.PlainL legalBodyTerm)

 where
  legalBodyId   = sqlKeyToText $ entityKey subjectEntity
  legalBodyTerm = sujetTerme $ entityVal subjectEntity

-- | Get all rows from Sujet table in database linked with
-- Filmo_GenresCategories table.
getMovieCategories :: (MonadIO m) => Pool SqlBackend -> m [Entity Sujet]
getMovieCategories pool =
  liftIO
    $ flip liftSqlPersistMPool pool
    $ select
    $ distinct
    $ from
    $ \(filmoGenresCategories, sujet) -> do
        where_
          (   sujet
          ^.  SujetId
          ==. filmoGenresCategories
          ^.  Filmo_GenresCategoriesSujetId
          )
        return sujet

-- | Create all triples for representing the Genre concept for all rows in
-- table Sujet.
--
-- Generated triples:
-- Each row in table Sujet where Sujet.SujetId appears in Filmo_GenresCategories.SujetId
--   cmtq:Subject{Sujet.SujetID} rdf:type crm:E40_Legal_Body
--   cmtq:Subject{Sujet.SujetID} rdfs:label {Sujet.Terme}
createMovieCategoriesTriples
  :: (RDF.Rdf rdfImpl, Monad m) => [Entity Sujet] -> RdfState rdfImpl m ()
createMovieCategoriesTriples = mapM_ createMovieCategoryTriples

-- | Create all triples for representing the Genre concept.
createMovieCategoryTriples
  :: (RDF.Rdf rdfImpl, Monad m) => Entity Sujet -> RdfState rdfImpl m ()
createMovieCategoryTriples subjectEntity = do
  let subjectUri = baseUriPath <> "/Genre" <> subjectId

  addTriple $ RDF.triple (RDF.unode subjectUri)
                         (RDF.unode SW.rdfType)
                         (RDF.unode SW.wdQ483394)

  addTriple $ RDF.triple (RDF.unode subjectUri)
                         (RDF.unode SW.rdfsLabel)
                         (RDF.lnode $ RDF.PlainL $ sujetTerme subject)

 where
  subjectId = sqlKeyToText $ entityKey subjectEntity
  subject   = entityVal subjectEntity

-- | Get all rows from Filmo table in database.
getFilmos :: (MonadIO m) => Pool SqlBackend -> m [Entity Filmo]
getFilmos pool =
  liftIO $ flip liftSqlPersistMPool pool $ select $ distinct $ from $ \filmo ->
    return filmo

-- | Create all triples for representing the Recording Work concept for all
-- rows in table Filmo.
--
-- Generated triples:
-- Each row in table Filmo
--   cmtq:RecordingWork{Filmo.FilmoID} rdf:type frbroo:F21_Recording_Work
--   cmtq:RecordingWork{Filmo.FilmoID} cmtqo:cmtq_id {Filmo.FilmoID}
--   cmtq:RecordingWork{Filmo.FilmoID} rdfs:label {Filmo.PrefixeTitreOriginal + Filmo.TitreOriginal}
--   cmtq:RecordingWork{Filmo.FilmoID} dbo:budget {Filmo.Cout}
--   cmtq:RecordingWork{Filmo.FilmoID} cmtqo:release_event cmtq:WorkPublicRelease{Filmo.FilmoID}
--   cmtq:WorkPublicRelease{Filmo.FilmoID} rdf:type cmtqo:Work_Public_Release
--   cmtq:WorkPublicRelease{Filmo.FilmoID} crm:P4_has_time-span cmtq:Time-Span{Filmo.AnneeSortie}
--   cmtq:Time-Span{Filmo.AnneeSortie} crm:P79_beginning_is_qualified_by {Filmo.AnneeSortie}^^xsd:dateTime
--   cmtq:RecordingWork{Filmo.FilmoID} frbroo:R22_created_a_realization_of cmtq:RecordingEvent{Filmo.FilmoID}
--   cmtq:RecordingEvent{Filmo.FilmoID} crm:P4_has_time-span cmtq:Time-Span{Filmo.AnneeDebProd ou Filmo.DateDebProd + Filmo.AnneeFinProd ou Filmo.DateFinProd}
--   cmtq:Time-Span{Filmo.AnneeDebProd ou Filmo.DateDebProd + Filmo.AnneeFinProd ou Filmo.DateFinProd}la crm:P79_beginning_is_qualified_by {Filmo.AnneeDebProd ou Filmo.DateDebProd}
--   cmtq:Time-Span{Filmo.AnneeDebProd ou Filmo.DateDebProd + Filmo.AnneeFinProd ou Filmo.DateFinProd}la crm:P80_end_is_qualified_by {Filmo.AnneeFinProd ou Filmo.DateFinProd}
createFilmosTriples
  :: (RDF.Rdf rdfImpl, Monad m) => [Entity Filmo] -> RdfState rdfImpl m ()
createFilmosTriples = mapM_ createFilmoTriples

-- | Create all triples for representing the Role concept.
createFilmoTriples
  :: (RDF.Rdf rdfImpl, Monad m) => Entity Filmo -> RdfState rdfImpl m ()
createFilmoTriples filmoEntity = do
  let filmoUri = baseUriPath <> "/RecordingWork" <> filmoId

  addTriple $ RDF.triple (RDF.unode filmoUri)
                         (RDF.unode SW.rdfType)
                         (RDF.unode SW.frbrooF21)

  addTriple $ RDF.triple (RDF.unode filmoUri)
                         (RDF.unode $ RDF.mkUri cmtqo "cmtq_id")
                         (RDF.lnode $ RDF.PlainL filmoId)

  -- Add optional title to recording work
  case titleMaybe of
    Just title -> addTriple $ RDF.triple (RDF.unode filmoUri)
                                         (RDF.unode SW.rdfsLabel)
                                         (RDF.lnode $ RDF.PlainL title)
    Nothing -> return ()

  case filmoCout filmo of
    Just movieCost -> addTriple $ RDF.triple
      (RDF.unode filmoUri)
      (RDF.unode SW.dboBudget)
      (RDF.lnode $ RDF.TypedL (Text.pack $ show movieCost) SW.xsdDouble)
    Nothing -> return ()


  -- Associate a release event with date to a recording work
  when (isJust releaseYearMaybe) $ do
    let publicReleaseUri = baseUriPath <> "/WorkPublicRelease" <> filmoId

    addTriple $ RDF.triple (RDF.unode filmoUri)
                           (RDF.unode $ RDF.mkUri cmtqo "release_event")
                           (RDF.unode publicReleaseUri)

    addTriple $ RDF.triple (RDF.unode publicReleaseUri)
                           (RDF.unode SW.rdfType)
                           (RDF.unode $ RDF.mkUri cmtqo "Work_Public_Release")

    -- Create triples which indicate when the production public release event
    -- occured.
    createResourceTimeSpanTriples publicReleaseUri releaseYearMaybe Nothing

  -- Every recording work has an associated recording event
  let filmoRecordingEventUri = baseUriPath <> "/RecordingEvent" <> filmoId

  addTriple $ RDF.triple (RDF.unode filmoRecordingEventUri)
                         (RDF.unode SW.frbrooR22)
                         (RDF.unode filmoUri)
  addTriple $ RDF.triple (RDF.unode filmoUri)
                         (RDF.unode SW.frbrooR22i)
                         (RDF.unode filmoRecordingEventUri)

  addTriple $ RDF.triple (RDF.unode filmoRecordingEventUri)
                         (RDF.unode SW.rdfType)
                         (RDF.unode SW.frbrooF29)

  -- Create triples which indicate when the production event occured.
  createProdEventTimeSpanTriples filmoEntity

 where
  filmoId = sqlKeyToText $ entityKey filmoEntity
  filmo   = entityVal filmoEntity

  titleMaybe =
    case (filmoPrefixeTitreOriginal filmo, filmoTitreOriginal filmo) of
      (Just prefixTitle, Just restTitle) ->
        Just $ prefixTitle <> " " <> restTitle
      (Nothing, Just restTitle) -> Just restTitle
      _                         -> Nothing

  releaseYearMaybe = filmoAnneeSortie filmo >>= parseYearField

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

-- | Get all rows from Filmo_Generique table in database.
getRecordingRoleActivities
  :: (MonadIO m) => Pool SqlBackend -> m [Entity Filmo_Generique]
getRecordingRoleActivities pool =
  liftIO
    $ flip liftSqlPersistMPool pool
    $ select
    $ distinct
    $ from
    $ \filmoGenerique -> return filmoGenerique

-- | Create all triples for representing the recording event concept for all
-- rows in table Filmo_Generique.
--
-- Some roles will not be used to create the RecordingRoleActivity triples
-- such as Générique Additionnel (13).
--
-- Generated triples:
-- Each row in table Filmo_Generique
--   if {Filmo_Generique.FonctionID = 33 (Source Originale)}
--     cmtq:RecordingWork{Filmo_Generique.FilmoID} crm:R2_is_derivative_of cmtq:Work{Filmo_Generique.FilmoID}d
--     cmtq:Wor{Filmo_Generique.FilmoID}d frbroo:R16i_was_initiated_by cmtq:WorkConception{Filmo_Generique.FilmoID}
--     cmtq:WorkConception{Filmo_Generique.FilmoID} crm:P14_carried_out_by cmtq:Person{Filmo_Generique.NomID} ou cmtq:LegalBody{Filmo_Generique.OrganismeId}
--   else
--     cmtq:RecordingEvent{Filmo_Generique.FilmoID} crm:P9_consists_of cmtq:RecordingRoleActivity{Filmo_Generique.FilmoID}
--     cmtq:RecordingRoleActivity{Filmo_Generique.FilmoID} cmtqo:in_the_role_of cmtq:Fonction{Filmo_Generique.FonctionID}
--     cmtq:RecordingRoleActivity{Filmo_Generique.FilmoID} crm:P14_carried_out_by cmtq:Person{Filmo_Generique.NomID} ou cmtq:LegalBody{Filmo_Generique.OrganismeId}
createRecordingRoleActivitiesTriples
  :: (RDF.Rdf rdfImpl, Monad m)
  => [Entity Filmo_Generique]
  -> RdfState rdfImpl m ()
createRecordingRoleActivitiesTriples = mapM_ createRecordingRoleActivityTriples
  . filter (\p -> getRoleId p /= 13)
  where getRoleId = fromSqlKey . filmo_GeneriqueFonctionId . entityVal

-- | Create all triples for representing the roles/functions an agent held in
-- a production event.
createRecordingRoleActivityTriples
  :: (RDF.Rdf rdfImpl, Monad m)
  => Entity Filmo_Generique
  -> RdfState rdfImpl m ()
createRecordingRoleActivityTriples filmoGeneriqueEntity =
  -- One of legal body or person must be defined in a row
  when
      (   isJust
      $   recordingActivityPersonUriMaybe
      <|> recordingActivityLegalBodyUriMaybe
      )
    $ if recordingActivityRoleTextId == "33"
        then do
          case
              recordingActivityPersonUriMaybe
                <|> recordingActivityLegalBodyUriMaybe
            of
              Just recordingActivityAgentUri -> do
                let recordingWorkUri =
                      baseUriPath
                        <> "/RecordingWork"
                        <> recordingActivityWorkTextId
                let
                  workUri =
                    baseUriPath <> "/Work" <> recordingActivityWorkTextId <> "d"
                let workConceptionUri =
                      baseUriPath
                        <> "/WorkConception"
                        <> recordingActivityWorkTextId
                        <> "d"

                addTriple $ RDF.triple (RDF.unode recordingWorkUri)
                                       (RDF.unode SW.frbrooR2)
                                       (RDF.unode workUri)
                addTriple $ RDF.triple (RDF.unode workUri)
                                       (RDF.unode SW.frbrooR2i)
                                       (RDF.unode recordingWorkUri)

                addTriple $ RDF.triple (RDF.unode workConceptionUri)
                                       (RDF.unode SW.frbrooR16)
                                       (RDF.unode workUri)
                addTriple $ RDF.triple (RDF.unode workUri)
                                       (RDF.unode SW.frbrooR16i)
                                       (RDF.unode workConceptionUri)

                addTriple $ RDF.triple (RDF.unode workConceptionUri)
                                       (RDF.unode SW.crmP14)
                                       (RDF.unode recordingActivityAgentUri)
                addTriple $ RDF.triple (RDF.unode recordingActivityAgentUri)
                                       (RDF.unode SW.crmP14i)
                                       (RDF.unode workConceptionUri)
              Nothing -> return ()

          return ()
        else do
          let recordingEventUri =
                baseUriPath <> "/RecordingEvent" <> recordingActivityWorkTextId
          let recordingActivityUri =
                baseUriPath
                  <> "/RecordingRoleActivity"
                  <> recordingActivityTextId
          let recordingActivityRoleUri =
                baseUriPath <> "/Role" <> recordingActivityRoleTextId

          addTriple $ RDF.triple (RDF.unode recordingEventUri)
                                 (RDF.unode SW.crmP9)
                                 (RDF.unode recordingActivityUri)
          addTriple $ RDF.triple (RDF.unode recordingActivityUri)
                                 (RDF.unode SW.crmP9i)
                                 (RDF.unode recordingEventUri)

          addTriple $ RDF.triple
            (RDF.unode recordingActivityUri)
            (RDF.unode SW.rdfType)
            (RDF.unode $ RDF.mkUri cmtqo "Recording_Role_Activity")

          addTriple $ RDF.triple
            (RDF.unode recordingActivityUri)
            (RDF.unode $ RDF.mkUri cmtqo "in_the_role_of")
            (RDF.unode recordingActivityRoleUri)

          case recordingActivityPersonUriMaybe of
            Just recordingActivityPersonUri -> do
              addTriple $ RDF.triple (RDF.unode recordingActivityUri)
                                     (RDF.unode SW.crmP14)
                                     (RDF.unode recordingActivityPersonUri)
              addTriple $ RDF.triple (RDF.unode recordingActivityPersonUri)
                                     (RDF.unode SW.crmP14i)
                                     (RDF.unode recordingActivityUri)
            Nothing -> return ()

          case recordingActivityLegalBodyUriMaybe of
            Just recordingActivityLegalBodyUri -> do
              addTriple $ RDF.triple (RDF.unode recordingActivityUri)
                                     (RDF.unode SW.crmP14)
                                     (RDF.unode recordingActivityLegalBodyUri)
              addTriple $ RDF.triple (RDF.unode recordingActivityLegalBodyUri)
                                     (RDF.unode SW.crmP14i)
                                     (RDF.unode recordingActivityUri)
            Nothing -> return ()

 where
  recordingActivityTextId = (sqlKeyToText . entityKey) filmoGeneriqueEntity
  recordingActivityWorkTextId =
    (sqlKeyToText . filmo_GeneriqueFilmoId . entityVal) filmoGeneriqueEntity
  recordingActivityRoleTextId =
    (sqlKeyToText . filmo_GeneriqueFonctionId . entityVal) filmoGeneriqueEntity

  recordingActivityPersonUriMaybe =
    fmap ((baseUriPath <> "/Person") <>) recordingActivityPersonTextIdMaybe
  recordingActivityPersonTextIdMaybe =
    (fmap sqlKeyToText . filmo_GeneriqueNomId . entityVal) filmoGeneriqueEntity

  recordingActivityLegalBodyUriMaybe = fmap
    ((baseUriPath <> "/LegalBody") <>)
    recordingActivityLegalBodyTextIdMaybe
  recordingActivityLegalBodyTextIdMaybe =
    (fmap sqlKeyToText . filmo_GeneriqueOrganismeId . entityVal)
      filmoGeneriqueEntity

-- | Create all triples for representing derivative work from a work using
-- the role "Source Originale" in the database.
-- createDerivativeWork :: (RDF.Rdf rdfImpl, Monad m)
--                      => Entity Filmo_Generique
--                      -> RdfState rdfImpl m ()
-- createDerivativeWork filmoGeneriqueEntity = do
--   let recordingEventUri = baseUriPath <> "/RecordingEvent" <> recordingActivityWorkTextId
--   return ()

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
                           (RDF.unode SW.crmP4)
                           (RDF.unode resourceTimeSpanUri)
    addTriple $ RDF.triple (RDF.unode resourceTimeSpanUri)
                           (RDF.unode SW.crmP4i)
                           (RDF.unode resourceUri)

    case beginDateMaybe of
      Just beginDate -> addTriple $ RDF.triple
        (RDF.unode resourceTimeSpanUri)
        (RDF.unode SW.crmP79)
        (RDF.lnode $ RDF.TypedL (utcTimeToText beginDate) SW.xsdDateTime)
      Nothing -> return ()

    case endDateMaybe of
      Just endDate -> addTriple $ RDF.triple
        (RDF.unode resourceTimeSpanUri)
        (RDF.unode SW.crmP80)
        (RDF.lnode $ RDF.TypedL (utcTimeToText endDate) SW.xsdDateTime)
      Nothing -> return ()

-- | Get all rows from Filmo_GenresCategories table in database.
getFilmosSubject
  :: (MonadIO m) => Pool SqlBackend -> m [Entity Filmo_GenresCategories]
getFilmosSubject pool =
  liftIO
    $ flip liftSqlPersistMPool pool
    $ select
    $ distinct
    $ from
    $ \filmoGenresCategories -> return filmoGenresCategories

-- | Create all triples for linking subjects and recording works for all rows
-- in table Filmo_GenresCategories.
--
-- Generated triples:
-- Each row in table Filmo_Generique
--   cmtq:RecordingWork{Filmo_GenresCategories.FilmoID} cmtqo:work_subject cmtq:Subject{Filmo_GenresCategories.SujetID}
createFilmosSubjectTriples
  :: (RDF.Rdf rdfImpl, Monad m)
  => [Entity Filmo_GenresCategories]
  -> RdfState rdfImpl m ()
createFilmosSubjectTriples = mapM_ createFilmoSubjectTriples

createFilmoSubjectTriples
  :: (RDF.Rdf rdfImpl, Monad m)
  => Entity Filmo_GenresCategories
  -> RdfState rdfImpl m ()
createFilmoSubjectTriples filmoSubject = do
  let filmoUri   = baseUriPath <> "/RecordingWork" <> filmoId
  let subjectUri = baseUriPath <> "/Genre" <> subjectId

  addTriple $ RDF.triple (RDF.unode filmoUri)
                         (RDF.unode SW.wdtP136)
                         (RDF.unode subjectUri)

 where
  filmoId =
    sqlKeyToText $ filmo_GenresCategoriesFilmoId $ entityVal filmoSubject
  subjectId =
    sqlKeyToText $ filmo_GenresCategoriesSujetId $ entityVal filmoSubject

-- | Get all rows from Filmo_Realisation table in database.
getFilmosDirector
  :: (MonadIO m) => Pool SqlBackend -> m [Entity Filmo_Realisation]
getFilmosDirector pool =
  liftIO
    $ flip liftSqlPersistMPool pool
    $ select
    $ distinct
    $ from
    $ \filmoRealisation -> return filmoRealisation

-- | Create all triples for linking recording works and directors for all rows
-- in table Filmo_Realisation.
--
-- Generated triples:
-- Each row in table Filmo_Realisation
--   cmtq:RecordingEvent{Filmo_Realisation.FilmoID} crm:P9_consists_of cmtq:RecordingRoleActivity{Filmo_Realisation.FilmoID + Filmo_Realisation.NomID + 1}
--   cmtq:RecordingRoleActivity{Filmo_Realisation.FilmoID} cmtqo:in_the_role_of cmtq:Fonction1
--   cmtq:RecordingRoleActivity{Filmo_Realisation.FilmoID} crm:P14_carried_out_by cmtq:Person{Filmo_Realisation.NomID}
createFilmosDirectorTriples
  :: (RDF.Rdf rdfImpl, Monad m)
  => [Entity Filmo_Realisation]
  -> RdfState rdfImpl m ()
createFilmosDirectorTriples = mapM_ createFilmoDirectorTriples

-- | Create all triples for representing the link between a recording work and
-- the directory.
createFilmoDirectorTriples
  :: (RDF.Rdf rdfImpl, Monad m)
  => Entity Filmo_Realisation
  -> RdfState rdfImpl m ()
createFilmoDirectorTriples filmoRealisationEntity = do
  let recordingActivityUri =
        baseUriPath <> "/RecordingRoleActivity" <> filmoDirectorId
  let recordingActivityRoleUri =
        baseUriPath <> "/Role" <> Text.pack (show customDirectorRoleId)
  let recordingActivityPersonUri = baseUriPath <> "/Person" <> directorId
  let recordingEventUri          = baseUriPath <> "/RecordingEvent" <> filmoId

  addTriple $ RDF.triple (RDF.unode recordingEventUri)
                         (RDF.unode SW.crmP9)
                         (RDF.unode recordingActivityUri)
  addTriple $ RDF.triple (RDF.unode recordingActivityUri)
                         (RDF.unode SW.crmP9i)
                         (RDF.unode recordingEventUri)

  addTriple $ RDF.triple
    (RDF.unode recordingActivityUri)
    (RDF.unode SW.rdfType)
    (RDF.unode $ RDF.mkUri cmtqo "Recording_Role_Activity")

  addTriple $ RDF.triple (RDF.unode recordingActivityUri)
                         (RDF.unode $ RDF.mkUri cmtqo "in_the_role_of")
                         (RDF.unode recordingActivityRoleUri)

  addTriple $ RDF.triple (RDF.unode recordingActivityUri)
                         (RDF.unode SW.crmP14)
                         (RDF.unode recordingActivityPersonUri)
  addTriple $ RDF.triple (RDF.unode recordingActivityPersonUri)
                         (RDF.unode SW.crmP14i)
                         (RDF.unode recordingActivityUri)
  return ()

 where
  filmoId =
    (sqlKeyToText . filmo_RealisationFilmoId . entityVal) filmoRealisationEntity
  directorId =
    (sqlKeyToText . filmo_RealisationNomId . entityVal) filmoRealisationEntity
  filmoDirectorId =
    filmoId <> "-" <> directorId <> "-" <> Text.pack (show customDirectorRoleId)

-- | Get all rows from Pays table in database.
getPlaces :: (MonadIO m) => Pool SqlBackend -> m [Entity Pays]
getPlaces pool =
  liftIO $ flip liftSqlPersistMPool pool $ select $ distinct $ from return

-- | Create all triples for representing the Place concept from all rows in
-- table Pays. Even though the table is called 'Pays', the instances it
-- contains represent the more general concept 'Place'.
--
-- Generated triples:
-- Each row in table Pays
--   cmtq:Place{Pays.PaysID} rdf:type crm:E53_Place
--   cmtq:Place{Pays.PaysID} rdfs:label {Pays.Terme}
createPlacesTriples
  :: (RDF.Rdf rdfImpl, Monad m) => [Entity Pays] -> RdfState rdfImpl m ()
createPlacesTriples = mapM_ createPlaceTriples

-- | Create all triples for representing the Place concept.
createPlaceTriples
  :: (RDF.Rdf rdfImpl, Monad m) => Entity Pays -> RdfState rdfImpl m ()
createPlaceTriples placeEntity = do
  let placeId  = sqlKeyToText $ entityKey placeEntity
  let place    = entityVal placeEntity
  let placeUri = baseUriPath <> "/Place" <> placeId

  addTriple $ RDF.triple (RDF.unode placeUri)
                         (RDF.unode SW.rdfType)
                         (RDF.unode SW.crmE53)

  addTriple $ RDF.triple (RDF.unode placeUri)
                         (RDF.unode SW.rdfsLabel)
                         (RDF.lnode $ RDF.PlainL $ paysTerme place)

-- | Get all rows from Filmo_Pays table in database.
getFilmosPlaces :: (MonadIO m) => Pool SqlBackend -> m [Entity Filmo_Pays]
getFilmosPlaces pool =
  liftIO
    $ flip liftSqlPersistMPool pool
    $ select
    $ distinct
    $ from
    $ \filmoPays -> return filmoPays

-- | Create all triples for linking recording works and places for all rows
-- in table Filmo_Pays.
--
-- Generated triples:
-- Each row in table Filmo_Pays
--   cmtq:RecordingEvent{Filmo_Pays.FilmoID} crm:P7_took_place_at cmtq:Place{Filmo_Pays.PaysID}
createFilmosPlacesTriples
  :: (RDF.Rdf rdfImpl, Monad m) => [Entity Filmo_Pays] -> RdfState rdfImpl m ()
createFilmosPlacesTriples = mapM_ createFilmoPlaceTriples

-- | Create all triples for representing the link between a recording work and
-- a place.
createFilmoPlaceTriples
  :: (RDF.Rdf rdfImpl, Monad m) => Entity Filmo_Pays -> RdfState rdfImpl m ()
createFilmoPlaceTriples filmoPaysEntity = do
  let filmoId = sqlKeyToText $ filmo_PaysFilmoId $ entityVal filmoPaysEntity
  let placeId      = sqlKeyToText $ filmo_PaysPaysId $ entityVal filmoPaysEntity
  let prodEventUri = baseUriPath <> "/RecordingEvent" <> filmoId
  let placeUri     = baseUriPath <> "/Place" <> placeId

  addTriple $ RDF.triple (RDF.unode prodEventUri)
                         (RDF.unode SW.crmP7)
                         (RDF.unode placeUri)
  addTriple $ RDF.triple (RDF.unode placeUri)
                         (RDF.unode SW.crmP7i)
                         (RDF.unode prodEventUri)

-- | Get all rows from Pays_LienWikidata table in database.
getPaysLienWikidata
  :: (MonadIO m) => Pool SqlBackend -> m [Entity Pays_LienWikidata]
getPaysLienWikidata pool =
  liftIO $ flip liftSqlPersistMPool pool $ select $ distinct $ from return

-- | Create all triples for representing the link between the Place concept from all rows in
-- table Pays_LienWikidata and Wikidata.
--
-- Generated triples:
-- Each row in table Pays_LienWikidata
--   cmtq:Place{Pays_LienWikidata.PaysID} owl:sameAs {Pays_LienWikidata.LienWikidata}
mkAllPaysLienWikidataRdf
  :: (RDF.Rdf rdfImpl, Monad m)
  => [Entity Pays_LienWikidata]
  -> RdfState rdfImpl m ()
mkAllPaysLienWikidataRdf = mapM_ mkPaysLienWikidataRdf

-- | Create triples for representing owl:sameAs between the Place concept and
-- a Wikidata entity.
mkPaysLienWikidataRdf
  :: (RDF.Rdf rdfImpl, Monad m)
  => Entity Pays_LienWikidata
  -> RdfState rdfImpl m ()
mkPaysLienWikidataRdf paysLienWdEntity = do
  let placeId =
        sqlKeyToText $ pays_LienWikidataPaysId $ entityVal paysLienWdEntity
  let placeUri = baseUriPath <> "/Place" <> placeId
  let wikidataUriMaybe =
        pays_LienWikidataLienWikidata $ entityVal paysLienWdEntity

  case wikidataUriMaybe of
    Just wikidataUri -> addTriple $ RDF.triple (RDF.unode placeUri)
                                               (RDF.unode SW.owlSameAs)
                                               (RDF.unode wikidataUri)
    Nothing -> return ()

-- | Get all rows from Filmo_LienWikidata table in database.
getFilmoLienWikidata
  :: (MonadIO m) => Pool SqlBackend -> m [Entity Filmo_LienWikidata]
getFilmoLienWikidata pool =
  liftIO $ flip liftSqlPersistMPool pool $ select $ distinct $ from return

-- | Create all triples for representing the link between the Recording work
-- concept and a Wikidata entity from all rows in table Pays_LienWikidata.
--
-- Generated triples:
-- Each row in table Filmo_LienWikidata
--   cmtq:RecordingWork{Filmo_LienWikidata.FilmoID} owl:sameAs {Filmo_LienWikidata.LienWikidata}
mkAllFilmoLienWikidataRdf
  :: (RDF.Rdf rdfImpl, Monad m)
  => [Entity Filmo_LienWikidata]
  -> RdfState rdfImpl m ()
mkAllFilmoLienWikidataRdf = mapM_ mkFilmoLienWikidataRdf

-- | Create triples for representing owl:sameAs between the Recording work
-- concept and a Wikidata entity.
mkFilmoLienWikidataRdf
  :: (RDF.Rdf rdfImpl, Monad m)
  => Entity Filmo_LienWikidata
  -> RdfState rdfImpl m ()
mkFilmoLienWikidataRdf filmoLienWdEntity = do
  let filmoId =
        sqlKeyToText $ filmo_LienWikidataFilmoId $ entityVal filmoLienWdEntity
  let filmoUri = baseUriPath <> "/RecordingWork" <> filmoId
  let wikidataUriMaybe =
        filmo_LienWikidataLienWikidata $ entityVal filmoLienWdEntity

  case wikidataUriMaybe of
    Just wikidataUri -> addTriple $ RDF.triple (RDF.unode filmoUri)
                                               (RDF.unode SW.owlSameAs)
                                               (RDF.unode wikidataUri)
    Nothing -> return ()

-- | Get all rows from Nom_LienWikidata table in database.
getNomLienWikidata
  :: (MonadIO m) => Pool SqlBackend -> m [Entity Nom_LienWikidata]
getNomLienWikidata pool =
  liftIO $ flip liftSqlPersistMPool pool $ select $ distinct $ from return

-- | Create all triples for representing the link between the Person concept
-- and a Wikidata entity from all rows in table Nom_LienWikidata.
--
-- Generated triples:
-- Each row in table Nom_LienWikidata
--   cmtq:Person{Nom_LienWikidata.NomID} owl:sameAs {Nom_LienWikidata.LienWikidata}
mkAllNomLienWikidataRdf
  :: (RDF.Rdf rdfImpl, Monad m)
  => [Entity Nom_LienWikidata]
  -> RdfState rdfImpl m ()
mkAllNomLienWikidataRdf = mapM_ mkNomLienWikidataRdf

-- | Create triples for representing owl:sameAs between the Person concept and
-- a Wikidata entity.
mkNomLienWikidataRdf
  :: (RDF.Rdf rdfImpl, Monad m)
  => Entity Nom_LienWikidata
  -> RdfState rdfImpl m ()
mkNomLienWikidataRdf nomLienWdEntity = do
  let wikidataUriMaybe =
        nom_LienWikidataLienWikidata $ entityVal nomLienWdEntity

  case wikidataUriMaybe of
    Just wikidataUri -> do
      let nomId =
            sqlKeyToText $ nom_LienWikidataNomId $ entityVal nomLienWdEntity
      let nomUri = baseUriPath <> "/Person" <> nomId
      addTriple $ RDF.triple (RDF.unode nomUri)
                             (RDF.unode SW.owlSameAs)
                             (RDF.unode wikidataUri)
    Nothing -> return ()

-- | Get all rows from Nom_LienWikidata table in database.
getFilmoResumes :: (MonadIO m) => Pool SqlBackend -> m [Entity FilmoResumes]
getFilmoResumes pool =
  liftIO $ flip liftSqlPersistMPool pool $ select $ distinct $ from return

-- | Create all triples for representing the link between the RecordingWor concept
-- and a synopsis in french.
--
-- Generated triples:
-- Each row in table FilmoResumes
--   cmtq:RecordingWork{FilmoResumes.FilmoID} cmtqo:synopsis {FilmoResumes.Resume}@fr
mkAllFilmoResumesRdf
  :: (RDF.Rdf rdfImpl, Monad m)
  => [Entity FilmoResumes]
  -> RdfState rdfImpl m ()
mkAllFilmoResumesRdf = mapM_ mkFilmoResumesRdf

-- | Create triples for representing the link between the RecordingWork concept and
-- a synopsis.
mkFilmoResumesRdf
  :: (RDF.Rdf rdfImpl, Monad m) => Entity FilmoResumes -> RdfState rdfImpl m ()
mkFilmoResumesRdf filmoResumeEntity = do
  let filmoId =
        sqlKeyToText $ filmoResumesFilmoId $ entityVal filmoResumeEntity
  let filmoUri    = baseUriPath <> "/RecordingWork" <> filmoId

  let resumeMaybe = filmoResumesResume $ entityVal filmoResumeEntity
  forM_ resumeMaybe $ \resume -> addTriple $ RDF.triple
    (RDF.unode filmoUri)
    (RDF.unode $ RDF.mkUri cmtqo "synopsis")
    (RDF.lnode $ RDF.PlainLL resume "fr")

-- | Get all rows from FilmoResumesAnglais table in database.
getFilmoResumesAnglais
  :: (MonadIO m) => Pool SqlBackend -> m [Entity FilmoResumesAnglais]
getFilmoResumesAnglais pool =
  liftIO $ flip liftSqlPersistMPool pool $ select $ distinct $ from return

-- | Create all triples for representing the link between the RecordingWork
-- concept and an english synopsis.
--
-- Generated triples:
-- Each row in table FilmoResumesAnglais
--   cmtq:RecordingWork{FilmoResumesAnglais.FilmoID} cmtqo:synopsis {FilmoResumesAnglais.ResumeAnglais}@en
mkAllFilmoResumesAnglaisRdf
  :: (RDF.Rdf rdfImpl, Monad m)
  => [Entity FilmoResumesAnglais]
  -> RdfState rdfImpl m ()
mkAllFilmoResumesAnglaisRdf = mapM_ mkFilmoResumesAnglaisRdf

-- | Create triples for representing the link between the RecordingWork concept and
-- an english synopsis.
mkFilmoResumesAnglaisRdf
  :: (RDF.Rdf rdfImpl, Monad m)
  => Entity FilmoResumesAnglais
  -> RdfState rdfImpl m ()
mkFilmoResumesAnglaisRdf filmoResumeEntity = do
  let filmoId =
        sqlKeyToText $ filmoResumesAnglaisFilmoId $ entityVal filmoResumeEntity
  let filmoUri = baseUriPath <> "/RecordingWork" <> filmoId

  let resumeMaybe =
        filmoResumesAnglaisResumeAnglais $ entityVal filmoResumeEntity
  forM_ resumeMaybe $ \resume -> addTriple $ RDF.triple
    (RDF.unode filmoUri)
    (RDF.unode $ RDF.mkUri cmtqo "synopsis")
    (RDF.lnode $ RDF.PlainLL resume "en")

-- | Get all rows from Langue table in database.
getLangue :: (MonadIO m) => Pool SqlBackend -> m [Entity Langue]
getLangue pool =
  liftIO $ flip liftSqlPersistMPool pool $ select $ distinct $ from return

-- | Create all triples for representing the link between the Language concept.
--
-- Generated triples:
-- Each row in table Langue
--   cmtq:Language{Langue.LangueID} rdf:type cmtqo:Language
--   cmtq:Language{Langue.LangueID} rdfs:label {Langue.Terme}
mkAllLangueRdf
  :: (RDF.Rdf rdfImpl, Monad m) => [Entity Langue] -> RdfState rdfImpl m ()
mkAllLangueRdf = mapM_ mkLangue

-- | Create all triples for representing the Language concept.
mkLangue :: (RDF.Rdf rdfImpl, Monad m) => Entity Langue -> RdfState rdfImpl m ()
mkLangue langueEntity = do
  let languageId    = sqlKeyToText $ entityKey langueEntity
  let languageUri   = baseUriPath <> "/Language" <> languageId
  let languageLabel = (langueTerme . entityVal) langueEntity

  addTriple $ RDF.triple (RDF.unode languageUri)
                         (RDF.unode SW.rdfType)
                         (RDF.unode $ RDF.mkUri cmtqo "Language")

  addTriple $ RDF.triple (RDF.unode languageUri)
                         (RDF.unode SW.rdfsLabel)
                         (RDF.lnode $ RDF.PlainL languageLabel)

-- | Get all rows from Langue_LienWikidata table in database.
getLangueLienWikidata
  :: (MonadIO m) => Pool SqlBackend -> m [Entity Langue_LienWikidata]
getLangueLienWikidata pool =
  liftIO $ flip liftSqlPersistMPool pool $ select $ distinct $ from return

-- | Create all triples for representing the link between the Language concept
-- and the equivalent concept in Wikidata.
--
-- Generated triples:
-- Each row in table Langue_LienWikidata
--   cmtq:Place{Langue_LienWikidata.LangueID} owl:sameAs {Langue_LienWikidata.LienWikidata}
mkAllLangueLienWikidataRdf
  :: (RDF.Rdf rdfImpl, Monad m)
  => [Entity Langue_LienWikidata]
  -> RdfState rdfImpl m ()
mkAllLangueLienWikidataRdf = mapM_ mkLangueLienWikidata

-- | Create all triples for representing the link between the Language concept
-- and the equivalent concept in Wikidata.
mkLangueLienWikidata
  :: (RDF.Rdf rdfImpl, Monad m)
  => Entity Langue_LienWikidata
  -> RdfState rdfImpl m ()
mkLangueLienWikidata langueLienWikidataEntity = do
  let languageId = sqlKeyToText $ langue_LienWikidataLangueId $ entityVal
        langueLienWikidataEntity
  let languageUri = baseUriPath <> "/Language" <> languageId
  let wikidataUriMaybe =
        langue_LienWikidataLienWikidata $ entityVal langueLienWikidataEntity

  case wikidataUriMaybe of
    Just wikidataUri -> addTriple $ RDF.triple (RDF.unode languageUri)
                                               (RDF.unode SW.owlSameAs)
                                               (RDF.unode wikidataUri)
    Nothing -> return ()

-- | Get all rows from Filmo_Langue table in database.
getFilmoLangue :: (MonadIO m) => Pool SqlBackend -> m [Entity Filmo_Langue]
getFilmoLangue pool =
  liftIO $ flip liftSqlPersistMPool pool $ select $ distinct $ from return

-- | Create all triples for linking recording works and languages for all rows
-- in table Filmo_Langue.
--
-- Generated triples:
-- Each row in table Filmo_Langue
--   cmtq:RecordingWor{Filmo_Langue.FilmoID} wdt:P407 cmtq:Language{Filmo_Langue.LangueID}
mkAllFilmoLangueRdf
  :: (RDF.Rdf rdfImpl, Monad m)
  => [Entity Filmo_Langue]
  -> RdfState rdfImpl m ()
mkAllFilmoLangueRdf = mapM_ mkFilmoLangueRdf

-- | Create all triples for representing the link between a recording work and
-- a place.
mkFilmoLangueRdf
  :: (RDF.Rdf rdfImpl, Monad m) => Entity Filmo_Langue -> RdfState rdfImpl m ()
mkFilmoLangueRdf filmoLangueEntity = do
  let filmoId =
        sqlKeyToText $ filmo_LangueFilmoId $ entityVal filmoLangueEntity
  let langueId =
        sqlKeyToText $ filmo_LangueLangueId $ entityVal filmoLangueEntity
  let recordWorkUri = baseUriPath <> "/RecordingWork" <> filmoId
  let langueUri     = baseUriPath <> "/Language" <> langueId

  addTriple $ RDF.triple (RDF.unode recordWorkUri)
                         (RDF.unode SW.wdtP407)
                         (RDF.unode langueUri)

-- | Parses a DATE field in the database.
parseDateField
  :: Text -- ^ Date field in the format DD-MM-YY
  -> Maybe UTCTime
parseDateField date =
  parseTimeM False defaultTimeLocale "%d-%-m-%-y" (Text.unpack date) :: Maybe
      UTCTime

-- | Parses a YEAR field in the database.
parseYearField
  :: (Show a)
  => a -- ^ Year field in the format YYYY
  -> Maybe UTCTime
parseYearField year =
  parseTimeM False defaultTimeLocale "%-Y" (show year) :: Maybe UTCTime

-- | Formats a UTCTime in Text format. The format is: 1988-01-01T00:00:00Z
utcTimeToText :: UTCTime -> Text
utcTimeToText = Text.pack . show . fromUTCTime

