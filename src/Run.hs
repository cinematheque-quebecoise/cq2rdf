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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
module Run (run) where

import Import hiding ((^.))
import Namespaces
import Data.RDF.State
import Util (getCurrentDayText)
import Database.CineTv.Public.Model
import qualified RIO.Text as Text
import qualified Data.Text.Lazy.IO as TextL
import Control.Monad.State
import Data.RDF (RDF)
import qualified Data.RDF as RDF
import qualified Data.RDF.Namespace as RDF
import Text.RDF.RDF4H.TurtleSerializer
import Text.RDF.RDF4H.NTriplesSerializer
import Data.Pool (Pool)
import Database.Esqueleto hiding (get)
import Database.Persist.Sqlite (SqliteConf(..))
import Data.Time.Clock (UTCTime(..), getCurrentTime)
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Data.Aeson (ToJSON(..))
import Data.Aeson.Text (encodeToLazyText)
import System.Directory (createDirectoryIfMissing)
import Codec.Compression.GZip (compress)
import qualified Data.ByteString.Lazy as BS
import System.Directory (doesFileExist, removeFile)
import Text.XML.XSD (fromUTCTime)
import System.FilePath (joinPath)
import Text.RE.TDFA.Text
import System.Process (callCommand)

-- Used for converting the prefix mappings in JSON format
instance ToJSON RDF.PrefixMappings

data Metadata = Metadata { metadataOriginalDate :: Text
                         , metadataCreationDate :: Text
                         } deriving (Generic)
instance ToJSON Metadata

-- run :: RIO App ()
-- run = do
--   let emptyRdf = RDF.empty :: RDF RDF.TList
--   let mappings = RDF.PrefixMappings $ Map.fromList [ ("mo", "http://purl.org/ontology/mo/") ]
--   let baseUrl = Just $ "http://example.org/resource"
--   liftIO $ RDF.writeRdf (TurtleSerializer baseUrl mappings) emptyRdf

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
  let baseUri = optionsBaseUri $ appOptions env
  let baseUriJust = Just $ RDF.BaseUrl baseUri
  let emptyRdf = RDF.mkRdf [] baseUriJust prefixMappings :: RDF RDF.TList
  graph <- liftIO $ execStateT (cq2rdf pool) emptyRdf

  let outputDir = Text.pack $ joinPath [ Text.unpack $ optionsOutputDir $ appOptions env
                                       , "cmtq-dataset"
                                       ]
  liftIO $ createDirectoryIfMissing True $ Text.unpack outputDir

  -- let prefixesFpath = Text.unpack $ outputDir <> "/prefixes.json"
  -- liftIO $ TextL.writeFile prefixesFpath $ encodeToLazyText $ RDF.prefixMappings graph

  let metadataFpath = Text.unpack $ outputDir <> "/metadata.json"
  liftIO $ TextL.writeFile metadataFpath $ encodeToLazyText $ Metadata originalDate creationDate

  let cmtqTurtleFpath = Text.unpack $ outputDir <> "/cmtq-dataset.ttl"
  liftIO $ withFile cmtqTurtleFpath WriteMode (\h -> RDF.hWriteRdf (TurtleSerializer Nothing prefixMappings) h graph)

  let cmtqNtriplesFpath = Text.unpack $ outputDir <> "/cmtq-dataset.nt"
  liftIO $ withFile cmtqNtriplesFpath WriteMode (\h -> RDF.hWriteRdf NTriplesSerializer h graph)

  -- Compress output Turtle file
  let cmtqTurtleFpathCompressed = cmtqTurtleFpath <> ".gz"
  liftIO $ BS.readFile cmtqTurtleFpath >>= (return . compress) >>= BS.writeFile cmtqTurtleFpathCompressed
  liftIO $ removeFile cmtqTurtleFpath

  -- Compress output N-triples file
  let cmtqNtriplesFpathCompressed = cmtqNtriplesFpath <> ".gz"
  liftIO $ BS.readFile cmtqNtriplesFpath >>= (return . compress) >>= BS.writeFile cmtqNtriplesFpathCompressed
  liftIO $ removeFile cmtqNtriplesFpath

  let cmtqHdtFpath = Text.unpack $ outputDir <> "/cmtq-dataset.hdt"
  let rdf2hdtCmd = "rdf2hdt -B " <> Text.unpack baseUri <> "/resource -f ttl " <> cmtqNtriplesFpathCompressed <> " " <> cmtqHdtFpath
  liftIO $ callCommand rdf2hdtCmd

  logInfo "Finished!"

getSqliteDbDate :: RIO App Text
getSqliteDbDate = do
  sqliteDbPath <- fmap (optionsSqlitePath . appOptions) ask
  case matchedText $ sqliteDbPath ?=~ [re|[0-9]+-[0-9]+-[0-9]+|] of
    Just date -> return date
    Nothing -> do
      logError "The SQLite file must contain a date in the format YYYY-MM-DD"
      exitFailure

cq2rdf :: (MonadIO m)
       => Pool SqlBackend
       -> RdfState RDF.TList m ()
cq2rdf pool = do
  nomEntities <- getNomEntities pool
  createPeopleTriples nomEntities

  fonctionEntities <- getFonctionEntities pool
  createRolesTriples fonctionEntities

  recordingRoleActivities <- getRecordingRoleActivities pool
  createRecordingRoleActivitiesTriples recordingRoleActivities

  filmos <- getFilmos pool
  createFilmosTriples filmos

  legalBodies <- getLegalBodies pool
  createLegalBodiesTriples legalBodies

  movieCategories <- getMovieCategories pool
  createMovieCategoriesTriples movieCategories

  filmosSubject <- getFilmosSubject pool
  createFilmosSubjectTriples filmosSubject

  filmosDirector <- getFilmosDirector pool
  createFilmosDirectorTriples filmosDirector

  filmosPlaces <- getFilmosPlaces pool
  createFilmosPlacesTriples filmosPlaces

  places <- getPlaces pool
  createPlacesTriples places

  paysLienWikidata <- getPaysLienWikidata pool
  mkAllPaysLienWikidataRdf paysLienWikidata

  filmoLienWikidata <- getFilmoLienWikidata pool
  mkAllFilmoLienWikidataRdf filmoLienWikidata

  nomLienWikidata <- getNomLienWikidata pool
  mkAllNomLienWikidataRdf nomLienWikidata

  filmoResumes <- getFilmoResumes pool
  mkAllFilmoResumesRdf filmoResumes

  filmoResumesAnglais <- getFilmoResumesAnglais pool
  mkAllFilmoResumesAnglaisRdf filmoResumesAnglais

  langue <- getLangue pool
  mkAllLangueRdf langue

  langueLienWikidata <- getLangueLienWikidata pool
  mkAllLangueLienWikidataRdf langueLienWikidata

  filmoLangue <- getFilmoLangue pool
  mkAllFilmoLangueRdf filmoLangue

  return ()

-- |Get all rows from Nom table in database.
getNomEntities :: (MonadIO m)
               => Pool SqlBackend
               -> m [Entity Nom]
getNomEntities pool = do
  liftIO $ flip liftSqlPersistMPool pool $ do
    select $ distinct $ from $ \nom -> do return nom

-- | Create all triples for representing the Person concept for all row in
-- table Nom.
createPeopleTriples :: (RDF.Rdf rdfImpl, Monad m)
                    => [Entity Nom]
                    -> RdfState rdfImpl m ()
createPeopleTriples nomEntities = mapM_ createPersonTriples nomEntities

-- | Create all triples for representing the Person concept.
createPersonTriples :: (RDF.Rdf rdfImpl, Monad m)
                    => Entity Nom
                    -> RdfState rdfImpl m ()
createPersonTriples nomEntity = do
  let personUri = "/resource/Person" <> personTextId

  addTriple $ RDF.triple (RDF.unode personUri)
                         (RDF.unode $ RDF.mkUri RDF.rdf "type")
                         (RDF.unode $ RDF.mkUri crm "E21_Person")

  addTriple $ RDF.triple (RDF.unode personUri)
                         (RDF.unode "/resource/cmtq_id")
                         (RDF.lnode $ RDF.PlainL $ personTextId)

  case nameOpt of
    Just name -> do
      addTriple $ RDF.triple (RDF.unode personUri)
                             (RDF.unode $ RDF.mkUri RDF.foaf $ "name")
                             (RDF.lnode $ RDF.PlainL name)
      addTriple $ RDF.triple (RDF.unode personUri)
                             (RDF.unode $ RDF.mkUri RDF.rdfs $ "label")
                             (RDF.lnode $ RDF.PlainL name)
    Nothing -> return ()

  case lastname of
    Just l -> addTriple $ RDF.triple (RDF.unode personUri)
                                     (RDF.unode $ RDF.mkUri RDF.foaf $ "familyName")
                                     (RDF.lnode $ RDF.PlainL l)
    Nothing -> return ()

  case firstname of
    Just f -> addTriple $ RDF.triple (RDF.unode personUri)
                                     (RDF.unode $ RDF.mkUri RDF.foaf $ "givenName")
                                     (RDF.lnode $ RDF.PlainL f)
    Nothing -> return ()

  where
    personTextId = Text.pack $ show $ fromSqlKey $ entityKey nomEntity
    firstname = nomPrenom $ entityVal nomEntity
    lastname = nomNom $ entityVal nomEntity
    nameOpt = case (firstname, lastname) of
      (f@(Just _), l@(Just _)) -> f <> (Just " ") <> l
      (f@(Just _), Nothing) -> f
      (Nothing, l@(Just _)) -> l
      (Nothing, Nothing) -> Nothing

-- | Get all rows from Nom table in database.
getFonctionEntities :: (MonadIO m)
                    => Pool SqlBackend
                    -> m [Entity Fonction]
getFonctionEntities pool = do
  liftIO $ flip liftSqlPersistMPool pool $ do
    select $ distinct $ from $ \fonction -> do return fonction

-- | Create all triples for representing the Role concept for all rows in
-- table Fonction.
createRolesTriples :: (RDF.Rdf rdfImpl, Monad m)
                    => [Entity Fonction]
                    -> RdfState rdfImpl m ()
createRolesTriples fonctionEntities = do
  mapM_ createRoleTriples fonctionEntities

  -- The director role does not exist in CineTV database.
  -- Has to be manually created.
  createRoleTriples $ Entity (toSqlKey customDirectorRoleId)
                             (Fonction "Réalisation")

-- | Create all triples for representing the Role concept.
createRoleTriples :: (RDF.Rdf rdfImpl, Monad m)
                  => Entity Fonction
                  -> RdfState rdfImpl m ()
createRoleTriples fonctionEntity = do
  let roleUri = "/resource/Role" <> roleTextId

  addTriple $ RDF.triple (RDF.unode roleUri)
                         (RDF.unode $ RDF.mkUri RDF.rdf "type")
                         (RDF.unode $ RDF.mkUri cmtqo "Role")

  addTriple $ RDF.triple (RDF.unode roleUri)
                         (RDF.unode $ RDF.mkUri cmtqo "cmtq_id")
                         (RDF.lnode $ RDF.PlainL roleTextId)

  addTriple $ RDF.triple (RDF.unode roleUri)
                         (RDF.unode $ RDF.mkUri RDF.rdfs "label")
                         (RDF.lnode $ RDF.PlainL roleLabel)

  where
    roleTextId = (Text.pack . show . fromSqlKey . entityKey) fonctionEntity
    roleLabel = (fonctionTerme . entityVal) fonctionEntity

-- | Get all rows from Sujet table in database that are potentially a legal
-- body.
getLegalBodies :: (MonadIO m)
               => Pool SqlBackend
               -> m [Entity Sujet]
getLegalBodies pool = do
  results <- liftIO $ flip liftSqlPersistMPool pool $ do
    select $
      distinct $
      from $ \(filmoGenerique, sujet) -> do
      where_ ( sujet ?. SujetId ==. filmoGenerique ^. Filmo_GeneriqueOrganismeId )
      return sujet

  return $ catMaybes results

-- | Create all triples for representing the Legal Body concept for all rows
-- in table Sujet.
createLegalBodiesTriples :: (RDF.Rdf rdfImpl, Monad m)
                         => [Entity Sujet]
                         -> RdfState rdfImpl m ()
createLegalBodiesTriples subjects = do
  mapM_ createLegalBodyTriples subjects

-- | Create all triples for representing the Legal Body concept.
-- The following roles are found in table Sujet: Financement, Laboratoire,
-- Maison de services, Personnages, Société d'exportation, Société de
-- distribution, société de production, télédiffuseur.
createLegalBodyTriples :: (RDF.Rdf rdfImpl, Monad m)
                       => Entity Sujet
                       -> RdfState rdfImpl m ()
createLegalBodyTriples subjectEntity = do
  let legalBodyUri = "/resource/LegalBody" <> legalBodyId

  addTriple $ RDF.triple (RDF.unode legalBodyUri)
                         (RDF.unode $ RDF.mkUri RDF.rdf "type")
                         (RDF.unode $ RDF.mkUri crm "E40_Legal_Body")

  addTriple $ RDF.triple (RDF.unode legalBodyUri)
                         (RDF.unode $ RDF.mkUri RDF.rdfs "label")
                         (RDF.lnode $ RDF.PlainL legalBodyTerm)

  where
    legalBodyId = sqlKeyToText $ entityKey subjectEntity
    legalBodyTerm = sujetTerme $ entityVal subjectEntity

-- | Get all rows from Sujet table in database linked with
-- Filmo_GenresCategories table.
getMovieCategories :: (MonadIO m)
                   => Pool SqlBackend
                   -> m [Entity Sujet]
getMovieCategories pool = do
  liftIO $ flip liftSqlPersistMPool pool $ do
    select $
      distinct $
      from $ \(filmoGenresCategories, sujet) -> do
        where_ ( sujet ^. SujetId ==. filmoGenresCategories ^. Filmo_GenresCategoriesSujetId )
        return sujet

-- | Create all triples for representing the Suject concept for all rows in
-- table Sujet.
createMovieCategoriesTriples :: (RDF.Rdf rdfImpl, Monad m)
                             => [Entity Sujet]
                             -> RdfState rdfImpl m ()
createMovieCategoriesTriples subjects = do
  mapM_ createMovieCategoryTriples subjects

-- | Create all triples for representing the Role concept.
createMovieCategoryTriples :: (RDF.Rdf rdfImpl, Monad m)
                           => Entity Sujet
                           -> RdfState rdfImpl m ()
createMovieCategoryTriples subjectEntity = do
  let subjectUri = "/resource/Subject" <> subjectId

  addTriple $ RDF.triple (RDF.unode subjectUri)
                         (RDF.unode $ RDF.mkUri RDF.rdf "type")
                         (RDF.unode $ RDF.mkUri cmtqo "Subject")

  addTriple $ RDF.triple (RDF.unode subjectUri)
                         (RDF.unode $ RDF.mkUri RDF.rdfs "label")
                         (RDF.lnode $ RDF.PlainL $ sujetTerme subject)

  where
    subjectId = sqlKeyToText $ entityKey subjectEntity
    subject = entityVal subjectEntity

-- | Get all rows from Sujet table in database.
getFilmos :: (MonadIO m)
          => Pool SqlBackend
          -> m [Entity Filmo]
getFilmos pool = do
  liftIO $ flip liftSqlPersistMPool pool $ do
    select $ distinct $ from $ \filmo -> do return filmo

-- | Create all triples for representing the Suject concept for all rows in
-- table Sujet.
createFilmosTriples :: (RDF.Rdf rdfImpl, Monad m)
                    => [Entity Filmo]
                    -> RdfState rdfImpl m ()
createFilmosTriples filmos = do
  mapM_ createFilmoTriples filmos

-- | Create all triples for representing the Role concept.
createFilmoTriples :: (RDF.Rdf rdfImpl, Monad m)
                   => Entity Filmo
                   -> RdfState rdfImpl m ()
createFilmoTriples filmoEntity = do
  let filmoUri = "/resource/RecordingWork" <> filmoId

  addTriple $ RDF.triple (RDF.unode filmoUri)
                         (RDF.unode $ RDF.mkUri RDF.rdf "type")
                         (RDF.unode $ RDF.mkUri frbroo "F21_Recording_Work")

  addTriple $ RDF.triple (RDF.unode filmoUri)
                         (RDF.unode $ RDF.mkUri cmtqo "cmtq_id")
                         (RDF.lnode $ RDF.PlainL filmoId)

  -- Add optional title to recording wor
  case titleMaybe of
    Just title -> addTriple $ RDF.triple (RDF.unode filmoUri)
                                         (RDF.unode $ RDF.mkUri RDF.rdfs "label")
                                         (RDF.lnode $ RDF.PlainL title)
    Nothing -> return ()

  case filmoCout filmo of
    Just movieCost -> do
      addTriple $ RDF.triple (RDF.unode filmoUri)
                             (RDF.unode $ RDF.mkUri cmtqo "cost")
                             (RDF.lnode $ RDF.TypedL (Text.pack $ show movieCost) (RDF.mkUri RDF.xsd "integer"))
    Nothing -> return ()


  -- Associate a release event with date to a recording work
  when (isJust releaseYearMaybe) $ do
    let publicReleaseUri = "/resource/WorkPublicRelease" <> filmoId

    addTriple $ RDF.triple (RDF.unode filmoUri)
                           (RDF.unode $ RDF.mkUri cmtqo "release_event")
                           (RDF.unode publicReleaseUri)

    addTriple $ RDF.triple (RDF.unode publicReleaseUri)
                           (RDF.unode $ RDF.mkUri RDF.rdf "type")
                           (RDF.unode $ RDF.mkUri cmtqo "Work_Public_Release")

    -- Create triples which indicate when the production public release event
    -- occured.
    createResourceTimeSpanTriples publicReleaseUri releaseYearMaybe Nothing

  -- Every recording work has an associated recording event
  let filmoRecordingEventUri = "/resource/RecordingEvent" <> filmoId

  addTriple $ RDF.triple (RDF.unode filmoRecordingEventUri)
                         (RDF.unode $ RDF.mkUri frbroo "R22_created_a_realization_of")
                         (RDF.unode filmoUri)
  addTriple $ RDF.triple (RDF.unode filmoUri)
                         (RDF.unode $ RDF.mkUri frbroo "R22i_was_realised_through")
                         (RDF.unode filmoRecordingEventUri)

  addTriple $ RDF.triple (RDF.unode filmoRecordingEventUri)
                         (RDF.unode $ RDF.mkUri RDF.rdf "type")
                         (RDF.unode $ RDF.mkUri frbroo "F29_Recording_Event")

  -- Create triples which indicate when the production event occured.
  createProdEventTimeSpanTriples filmoEntity

  where
    filmoId = sqlKeyToText $ entityKey filmoEntity
    filmo = entityVal filmoEntity

    titleMaybe = case (filmoPrefixeTitreOriginal filmo, filmoTitreOriginal filmo) of
                   (Just prefixTitle, Just restTitle) -> Just $ prefixTitle <> " " <> restTitle
                   (Nothing, Just restTitle) -> Just restTitle
                   _ -> Nothing

    releaseYearMaybe = filmoAnneeSortie filmo >>= parseYearField

createProdEventTimeSpanTriples :: (RDF.Rdf rdfImpl, Monad m)
                               => Entity Filmo
                               -> RdfState rdfImpl m ()
createProdEventTimeSpanTriples filmoEntity = do
  let beginDateTimeMaybe = dateBeginMaybe <|> yearBeginMaybe
  let endDateTimeMaybe = dateEndMaybe <|> yearEndMaybe

  createResourceTimeSpanTriples ("/resource/RecordingEvent" <> filmoId)
                                beginDateTimeMaybe
                                endDateTimeMaybe

  where
    filmoId = sqlKeyToText $ entityKey filmoEntity
    filmo = entityVal filmoEntity

    -- parseTimeM False defaultTimeLocale "%d-%-m-%-y" "09-09-89" :: Maybe UTCTime
    dateBeginMaybe = filmoDateDebProd filmo >>= parseDateField
    dateEndMaybe = filmoDateFinProd filmo >>= parseDateField

    yearBeginMaybe = filmoAnneeDebProd filmo >>= parseYearField
    yearEndMaybe = filmoAnneeFinProd filmo >>= parseYearField

-- | Get all rows from Filmo_Generique table in database.
getRecordingRoleActivities :: (MonadIO m)
                           => Pool SqlBackend
                           -> m [Entity Filmo_Generique]
getRecordingRoleActivities pool = do
  liftIO $ flip liftSqlPersistMPool pool $ do
    select $ distinct $ from $ \filmoGenerique -> return filmoGenerique

-- | Create all triples for representing the recording event concept for all
-- rows in table Filmo_Generique.
--
-- Some roles will not be used to create the RecordingRoleActivity triples
-- such as Générique Additionnel (13).
createRecordingRoleActivitiesTriples :: (RDF.Rdf rdfImpl, Monad m)
                                     => [Entity Filmo_Generique]
                                     -> RdfState rdfImpl m ()
createRecordingRoleActivitiesTriples prodEvents = do
  mapM_ createRecordingRoleActivityTriples $ filter (\p -> getRoleId p `notElem` [13]) $ prodEvents

  where
    getRoleId = fromSqlKey . filmo_GeneriqueFonctionId . entityVal

-- | Create all triples for representing the roles/functions an agent held in
-- a production event.
createRecordingRoleActivityTriples :: (RDF.Rdf rdfImpl, Monad m)
                                   => Entity Filmo_Generique
                                   -> RdfState rdfImpl m ()
createRecordingRoleActivityTriples filmoGeneriqueEntity = do
  -- One of legal body or person must be defined in a row
  when (isJust $ recordingActivityPersonUriMaybe <|> recordingActivityLegalBodyUriMaybe) $ do
    if recordingActivityRoleTextId == "33"
    then do
      case recordingActivityPersonUriMaybe <|> recordingActivityLegalBodyUriMaybe of
        Just recordingActivityAgentUri -> do
          let recordingWorkUri = "/resource/RecordingWork" <> recordingActivityWorkTextId
          let workUri = "/resource/Work" <> recordingActivityWorkTextId <> "d"
          let workConceptionUri = "/resource/WorkConception" <> recordingActivityWorkTextId <> "d"

          addTriple $ RDF.triple (RDF.unode recordingWorkUri)
                                 (RDF.unode $ RDF.mkUri frbroo "R2_is_derivative_of")
                                 (RDF.unode workUri)
          addTriple $ RDF.triple (RDF.unode workUri)
                                 (RDF.unode $ RDF.mkUri frbroo "R2i_has_derivative")
                                 (RDF.unode recordingWorkUri)

          addTriple $ RDF.triple (RDF.unode workConceptionUri)
                                 (RDF.unode $ RDF.mkUri frbroo "R16_initiated")
                                 (RDF.unode workUri)
          addTriple $ RDF.triple (RDF.unode workUri)
                                 (RDF.unode $ RDF.mkUri frbroo "R16i_was_initiated_by")
                                 (RDF.unode workConceptionUri)

          addTriple $ RDF.triple (RDF.unode workConceptionUri)
                                 (RDF.unode $ RDF.mkUri crm "P14_carried_out_by")
                                 (RDF.unode recordingActivityAgentUri)
          addTriple $ RDF.triple (RDF.unode recordingActivityAgentUri)
                                 (RDF.unode $ RDF.mkUri crm "P14i_performed")
                                 (RDF.unode workConceptionUri)
        Nothing -> return ()

      return ()

    else do
      let recordingEventUri = "/resource/RecordingEvent" <> recordingActivityWorkTextId
      let recordingActivityUri = "/resource/RecordingRoleActivity" <> recordingActivityTextId
      let recordingActivityRoleUri = "/resource/Role" <> recordingActivityRoleTextId

      addTriple $ RDF.triple (RDF.unode recordingEventUri)
                             (RDF.unode $ RDF.mkUri crm "P9_consists_of")
                             (RDF.unode recordingActivityUri)
      addTriple $ RDF.triple (RDF.unode recordingActivityUri)
                             (RDF.unode $ RDF.mkUri crm "P9i_forms_part_of")
                             (RDF.unode recordingEventUri)

      addTriple $ RDF.triple (RDF.unode recordingActivityUri)
                             (RDF.unode $ RDF.mkUri RDF.rdf "type")
                             (RDF.unode $ RDF.mkUri cmtqo "Recording_Role_Activity")

      addTriple $ RDF.triple (RDF.unode recordingActivityUri)
                             (RDF.unode $ RDF.mkUri cmtqo "in_the_role_of")
                             (RDF.unode recordingActivityRoleUri)

      case recordingActivityPersonUriMaybe of
        Just recordingActivityPersonUri -> do
          addTriple $ RDF.triple (RDF.unode recordingActivityUri)
                                 (RDF.unode $ RDF.mkUri crm "P14_carried_out_by")
                                 (RDF.unode recordingActivityPersonUri)
          addTriple $ RDF.triple (RDF.unode recordingActivityPersonUri)
                                 (RDF.unode $ RDF.mkUri crm "P14i_performed")
                                 (RDF.unode recordingActivityUri)
        Nothing -> return ()

      case recordingActivityLegalBodyUriMaybe of
        Just recordingActivityLegalBodyUri -> do
          addTriple $ RDF.triple (RDF.unode recordingActivityUri)
                                 (RDF.unode $ RDF.mkUri crm "P14_carried_out_by")
                                 (RDF.unode recordingActivityLegalBodyUri)
          addTriple $ RDF.triple (RDF.unode recordingActivityLegalBodyUri)
                                 (RDF.unode $ RDF.mkUri crm "P14i_performed")
                                 (RDF.unode recordingActivityUri)
        Nothing -> return ()

  where
    showableToText = Text.pack . show
    recordingActivityTextId = ( showableToText
                              . fromSqlKey
                              . entityKey
                              ) filmoGeneriqueEntity
    recordingActivityWorkTextId = ( showableToText
                                  . fromSqlKey
                                  . filmo_GeneriqueFilmoId
                                  . entityVal
                                  ) filmoGeneriqueEntity
    recordingActivityRoleTextId = ( showableToText
                                  . fromSqlKey
                                  . filmo_GeneriqueFonctionId
                                  . entityVal
                                  ) filmoGeneriqueEntity

    recordingActivityPersonUriMaybe = fmap ("/resource/Person" <>) recordingActivityPersonTextIdMaybe
    recordingActivityPersonTextIdMaybe = ( fmap (showableToText . fromSqlKey)
                                         . filmo_GeneriqueNomId
                                         . entityVal
                                         ) filmoGeneriqueEntity

    recordingActivityLegalBodyUriMaybe = fmap ("/resource/LegalBody" <>) recordingActivityLegalBodyTextIdMaybe
    recordingActivityLegalBodyTextIdMaybe = ( fmap (showableToText . fromSqlKey)
                                            . filmo_GeneriqueOrganismeId
                                            . entityVal
                                            ) filmoGeneriqueEntity

-- | Create all triples for representing derivative work from a work using
-- the role "Source Originale" in the database.
-- createDerivativeWork :: (RDF.Rdf rdfImpl, Monad m)
--                      => Entity Filmo_Generique
--                      -> RdfState rdfImpl m ()
-- createDerivativeWork filmoGeneriqueEntity = do
--   let recordingEventUri = "/resource/RecordingEvent" <> recordingActivityWorkTextId
--   return ()

-- | From a given resource, create a time-span concept using given begin and
-- end dates (optional values).
createResourceTimeSpanTriples :: (RDF.Rdf rdfImpl, Monad m)
                              => Text -- ^ Resource URI
                              -> Maybe UTCTime -- ^ Optional begin date in UTCTime format
                              -> Maybe UTCTime -- ^ Optional end date in UTCTime format
                              -> RdfState rdfImpl m ()
createResourceTimeSpanTriples resourceUri beginDateMaybe endDateMaybe = do
  if isJust $ beginDateMaybe <|> endDateMaybe
  then do
    let lit = Text.intercalate "-" $ fmap utcTimeToText $ catMaybes [beginDateMaybe, endDateMaybe]
    let resourceTimeSpanUri = "/resource/Time-Span" <> lit
    -- let resourceTimePrimitiveUri = "/resource/Time_Primitive" <> lit

    addTriple $ RDF.triple (RDF.unode $ resourceUri)
                           (RDF.unode $ RDF.mkUri crm "P4_has_time-span")
                           (RDF.unode $ resourceTimeSpanUri)
    addTriple $ RDF.triple (RDF.unode $ resourceTimeSpanUri)
                           (RDF.unode $ RDF.mkUri crm "P4i_is_time-span_of")
                           (RDF.unode $ resourceUri)

    case beginDateMaybe of
      Just beginDate -> do
        addTriple $ RDF.triple (RDF.unode resourceTimeSpanUri)
                               (RDF.unode $ RDF.mkUri crm "P79_beginning_is_qualified_by")
                               (RDF.lnode $ RDF.TypedL (utcTimeToText beginDate) (RDF.mkUri RDF.xsd "dateTime"))
      Nothing -> return ()

    case endDateMaybe of
      Just endDate -> do
        addTriple $ RDF.triple (RDF.unode resourceTimeSpanUri)
                               (RDF.unode $ RDF.mkUri crm "P80_end_is_qualified_by")
                               (RDF.lnode $ RDF.TypedL (utcTimeToText endDate) (RDF.mkUri RDF.xsd "dateTime"))
      Nothing -> return ()

  else
    return ()

-- | Get all rows from Filmo_GenresCategories table in database.
getFilmosSubject :: (MonadIO m)
                 => Pool SqlBackend
                 -> m [Entity Filmo_GenresCategories]
getFilmosSubject pool = do
  liftIO $ flip liftSqlPersistMPool pool $ do
    select $ distinct $ from $ \filmoGenresCategories -> return filmoGenresCategories

-- | Create all triples for linking subjects and recording works for all rows
-- in table Filmo_GenresCategories.
createFilmosSubjectTriples :: (RDF.Rdf rdfImpl, Monad m)
                           => [Entity Filmo_GenresCategories]
                           -> RdfState rdfImpl m ()
createFilmosSubjectTriples filmosSubject =
  mapM_ createFilmoSubjectTriples filmosSubject

createFilmoSubjectTriples :: (RDF.Rdf rdfImpl, Monad m)
                          => Entity Filmo_GenresCategories
                          -> RdfState rdfImpl m ()
createFilmoSubjectTriples filmoSubject = do
  let filmoUri = "/resource/RecordingWork" <> filmoId
  let subjectUri = "/resource/Subject" <> subjectId

  addTriple $ RDF.triple (RDF.unode filmoUri)
                         (RDF.unode $ RDF.mkUri cmtqo "has_subject")
                         (RDF.unode $ subjectUri)

  where
    filmoId = sqlKeyToText $ filmo_GenresCategoriesFilmoId $ entityVal filmoSubject
    subjectId = sqlKeyToText $ filmo_GenresCategoriesSujetId $ entityVal filmoSubject

-- | Get all rows from Filmo_Realisation table in database.
getFilmosDirector :: (MonadIO m)
                  => Pool SqlBackend
                  -> m [Entity Filmo_Realisation]
getFilmosDirector pool = do
  liftIO $ flip liftSqlPersistMPool pool $ do
    select $ distinct $ from $ \filmoRealisation -> return filmoRealisation

-- | Create all triples for linking recording works and directors for all rows
-- in table Filmo_Realisation.
createFilmosDirectorTriples :: (RDF.Rdf rdfImpl, Monad m)
                            => [Entity Filmo_Realisation]
                            -> RdfState rdfImpl m ()
createFilmosDirectorTriples filmosDirector =
  mapM_ createFilmoDirectorTriples filmosDirector

-- | Create all triples for representing the link between a recording work and
-- the directory.
createFilmoDirectorTriples :: (RDF.Rdf rdfImpl, Monad m)
                           => Entity Filmo_Realisation
                           -> RdfState rdfImpl m ()
createFilmoDirectorTriples filmoRealisationEntity = do
  let recordingActivityUri = "/resource/RecordingRoleActivity" <> filmoDirectorId
  let recordingActivityRoleUri = "/resource/Role" <> (Text.pack $ show customDirectorRoleId)
  let recordingActivityPersonUri = "/resource/Person" <> directorId
  let recordingEventUri = "/resource/RecordingEvent" <> filmoId

  addTriple $ RDF.triple (RDF.unode recordingEventUri)
                         (RDF.unode $ RDF.mkUri crm "P9_consists_of")
                         (RDF.unode recordingActivityUri)
  addTriple $ RDF.triple (RDF.unode recordingActivityUri)
                         (RDF.unode $ RDF.mkUri crm "P9i_forms_part_of")
                         (RDF.unode recordingEventUri)

  addTriple $ RDF.triple (RDF.unode recordingActivityUri)
                         (RDF.unode $ RDF.mkUri RDF.rdf "type")
                         (RDF.unode $ RDF.mkUri cmtqo "Recording_Role_Activity")

  addTriple $ RDF.triple (RDF.unode recordingActivityUri)
                         (RDF.unode $ RDF.mkUri cmtqo "in_the_role_of")
                         (RDF.unode recordingActivityRoleUri)

  addTriple $ RDF.triple (RDF.unode recordingActivityUri)
                         (RDF.unode $ RDF.mkUri crm "P14_carried_out_by")
                         (RDF.unode recordingActivityPersonUri)
  addTriple $ RDF.triple (RDF.unode recordingActivityPersonUri)
                         (RDF.unode $ RDF.mkUri crm "P14i_performed")
                         (RDF.unode recordingActivityUri)
  return ()

  where
    filmoId = ( sqlKeyToText
              . filmo_RealisationFilmoId
              . entityVal
              ) filmoRealisationEntity
    directorId = ( sqlKeyToText
                 . filmo_RealisationNomId
                 . entityVal
                 ) filmoRealisationEntity
    filmoDirectorId = filmoId
                   <> "-"
                   <> directorId
                   <> "-"
                   <> (Text.pack $ show customDirectorRoleId)

-- | Get all rows from Pays table in database.
getPlaces :: (MonadIO m)
          => Pool SqlBackend
          -> m [Entity Pays]
getPlaces pool = do
  liftIO $ flip liftSqlPersistMPool pool $ do
    select $ distinct $ from $ return

-- | Create all triples for representing the Place concept from all rows in
-- table Pays. Even though the table is called 'Pays', the instances it
-- contains represent the more general concept 'Place'.
createPlacesTriples :: (RDF.Rdf rdfImpl, Monad m)
                    => [Entity Pays]
                    -> RdfState rdfImpl m ()
createPlacesTriples places =
  mapM_ createPlaceTriples places

-- | Create all triples for representing the Place concept.
createPlaceTriples :: (RDF.Rdf rdfImpl, Monad m)
                   => Entity Pays
                   -> RdfState rdfImpl m ()
createPlaceTriples placeEntity = do
  let placeId = sqlKeyToText $ entityKey placeEntity
  let place = entityVal placeEntity
  let placeUri = "/resource/Place" <> placeId

  addTriple $ RDF.triple (RDF.unode placeUri)
                         (RDF.unode $ RDF.mkUri RDF.rdf "type")
                         (RDF.unode $ RDF.mkUri crm $ "E53_Place")

  addTriple $ RDF.triple (RDF.unode placeUri)
                         (RDF.unode $ RDF.mkUri RDF.rdfs "label")
                         (RDF.lnode $ RDF.PlainL $ paysTerme place)

-- | Get all rows from Filmo_Pays table in database.
getFilmosPlaces :: (MonadIO m)
                => Pool SqlBackend
                -> m [Entity Filmo_Pays]
getFilmosPlaces pool = do
  liftIO $ flip liftSqlPersistMPool pool $ do
    select $ distinct $ from $ \filmoPays -> return filmoPays

-- | Create all triples for linking recording works and places for all rows
-- in table Filmo_Pays.
createFilmosPlacesTriples :: (RDF.Rdf rdfImpl, Monad m)
                          => [Entity Filmo_Pays]
                          -> RdfState rdfImpl m ()
createFilmosPlacesTriples filmosPlaces =
  mapM_ createFilmoPlaceTriples filmosPlaces

-- | Create all triples for representing the link between a recording work and
-- a place.
createFilmoPlaceTriples :: (RDF.Rdf rdfImpl, Monad m)
                        => Entity Filmo_Pays
                        -> RdfState rdfImpl m ()
createFilmoPlaceTriples filmoPaysEntity = do
  let filmoId = sqlKeyToText $ filmo_PaysFilmoId $ entityVal filmoPaysEntity
  let placeId = sqlKeyToText $ filmo_PaysPaysId $ entityVal filmoPaysEntity
  let prodEventUri = "/resource/RecordingEvent" <> filmoId
  let placeUri = "/resource/Place" <> placeId

  addTriple $ RDF.triple (RDF.unode prodEventUri)
                         (RDF.unode $ RDF.mkUri crm "P7_took_place_at")
                         (RDF.unode placeUri)
  addTriple $ RDF.triple (RDF.unode placeUri)
                         (RDF.unode $ RDF.mkUri crm "P7i_witnessed")
                         (RDF.unode prodEventUri)

-- | Get all rows from Pays_LienWikidata table in database.
getPaysLienWikidata :: (MonadIO m)
                    => Pool SqlBackend
                    -> m [Entity Pays_LienWikidata]
getPaysLienWikidata pool = do
  liftIO $ flip liftSqlPersistMPool pool $ do
    select $ distinct $ from $ return

-- | Create all triples for representing the link between the Place concept from all rows in
-- table Pays_LienWikidata and Wikidata.
mkAllPaysLienWikidataRdf :: (RDF.Rdf rdfImpl, Monad m)
                         => [Entity Pays_LienWikidata]
                         -> RdfState rdfImpl m ()
mkAllPaysLienWikidataRdf paysLienWikidataEntities = do
  mapM_ mkPaysLienWikidataRdf paysLienWikidataEntities

-- | Create triples for representing owl:sameAs between the Place concept and
-- a Wikidata entity.
mkPaysLienWikidataRdf :: (RDF.Rdf rdfImpl, Monad m)
                      => Entity Pays_LienWikidata
                      -> RdfState rdfImpl m ()
mkPaysLienWikidataRdf paysLienWdEntity = do
  let placeId = sqlKeyToText $ pays_LienWikidataPaysId $ entityVal paysLienWdEntity
  let placeUri = "/resource/Place" <> placeId
  let wikidataUriMaybe = pays_LienWikidataLienWikidata $ entityVal paysLienWdEntity

  case wikidataUriMaybe of
    Just wikidataUri -> do
      addTriple $ RDF.triple (RDF.unode placeUri)
                             (RDF.unode $ RDF.mkUri RDF.owl "sameAs")
                             (RDF.unode wikidataUri)
    Nothing -> return ()

-- | Get all rows from Filmo_LienWikidata table in database.
getFilmoLienWikidata :: (MonadIO m)
                     => Pool SqlBackend
                     -> m [Entity Filmo_LienWikidata]
getFilmoLienWikidata pool = do
  liftIO $ flip liftSqlPersistMPool pool $ do
    select $ distinct $ from $ return

-- | Create all triples for representing the link between the Recording work
-- concept and a Wikidata entity from all rows in table Pays_LienWikidata.
mkAllFilmoLienWikidataRdf :: (RDF.Rdf rdfImpl, Monad m)
                          => [Entity Filmo_LienWikidata]
                          -> RdfState rdfImpl m ()
mkAllFilmoLienWikidataRdf filmoLienWikidataEntities = do
  mapM_ mkFilmoLienWikidataRdf filmoLienWikidataEntities

-- | Create triples for representing owl:sameAs between the Recording work
-- concept and a Wikidata entity.
mkFilmoLienWikidataRdf :: (RDF.Rdf rdfImpl, Monad m)
                       => Entity Filmo_LienWikidata
                       -> RdfState rdfImpl m ()
mkFilmoLienWikidataRdf filmoLienWdEntity = do
  let filmoId = sqlKeyToText $ filmo_LienWikidataFilmoId $ entityVal filmoLienWdEntity
  let filmoUri = "/resource/RecordingWork" <> filmoId
  let wikidataUriMaybe = filmo_LienWikidataLienWikidata $ entityVal filmoLienWdEntity

  case wikidataUriMaybe of
    Just wikidataUri -> do
      addTriple $ RDF.triple (RDF.unode filmoUri)
                             (RDF.unode $ RDF.mkUri RDF.owl "sameAs")
                             (RDF.unode wikidataUri)
    Nothing -> return ()

-- | Get all rows from Nom_LienWikidata table in database.
getNomLienWikidata :: (MonadIO m)
                   => Pool SqlBackend
                   -> m [Entity Nom_LienWikidata]
getNomLienWikidata pool = do
  liftIO $ flip liftSqlPersistMPool pool $ do
    select $ distinct $ from $ return

-- | Create all triples for representing the link between the Person concept
-- and a Wikidata entity from all rows in table Nom_LienWikidata.
mkAllNomLienWikidataRdf :: (RDF.Rdf rdfImpl, Monad m)
                        => [Entity Nom_LienWikidata]
                        -> RdfState rdfImpl m ()
mkAllNomLienWikidataRdf nomLienWikidataEntities = do
  mapM_ mkNomLienWikidataRdf nomLienWikidataEntities

-- | Create triples for representing owl:sameAs between the Person concept and
-- a Wikidata entity.
mkNomLienWikidataRdf :: (RDF.Rdf rdfImpl, Monad m)
                      => Entity Nom_LienWikidata
                      -> RdfState rdfImpl m ()
mkNomLienWikidataRdf nomLienWdEntity = do
  let wikidataUriMaybe = nom_LienWikidataLienWikidata $ entityVal nomLienWdEntity

  case wikidataUriMaybe of
    Just wikidataUri -> do
      let nomId = sqlKeyToText $ nom_LienWikidataNomId $ entityVal nomLienWdEntity
      let nomUri = "/resource/Person" <> nomId
      addTriple $ RDF.triple (RDF.unode nomUri)
                             (RDF.unode $ RDF.mkUri RDF.owl "sameAs")
                             (RDF.unode wikidataUri)
    Nothing -> return ()

-- | Get all rows from Nom_LienWikidata table in database.
getFilmoResumes :: (MonadIO m)
                => Pool SqlBackend
                -> m [Entity FilmoResumes]
getFilmoResumes pool = do
  liftIO $ flip liftSqlPersistMPool pool $ do
    select $ distinct $ from $ return

-- | Create all triples for representing the link between the Person concept
-- and a Wikidata entity from all rows in table Nom_LienWikidata.
mkAllFilmoResumesRdf :: (RDF.Rdf rdfImpl, Monad m)
                     => [Entity FilmoResumes]
                     -> RdfState rdfImpl m ()
mkAllFilmoResumesRdf filmoResumesEntities = do
  mapM_ mkFilmoResumesRdf filmoResumesEntities

-- | Create triples for representing the link between the RecordingWork concept and
-- a synopsis.
mkFilmoResumesRdf :: (RDF.Rdf rdfImpl, Monad m)
                  => Entity FilmoResumes
                  -> RdfState rdfImpl m ()
mkFilmoResumesRdf filmoResumeEntity = do
  let filmoId = sqlKeyToText $ filmoResumesFilmoId $ entityVal filmoResumeEntity
  let filmoUri = "/resource/RecordingWork" <> filmoId

  case filmoResumesResume $ entityVal filmoResumeEntity of
    Just resume -> do
      addTriple $ RDF.triple (RDF.unode filmoUri)
                             (RDF.unode $ RDF.mkUri cmtqo "synopsis")
                             (RDF.lnode $ RDF.PlainLL resume "fr")
    Nothing -> return ()

-- | Get all rows from FilmoResumesAnglais table in database.
getFilmoResumesAnglais :: (MonadIO m)
                       => Pool SqlBackend
                       -> m [Entity FilmoResumesAnglais]
getFilmoResumesAnglais pool = do
  liftIO $ flip liftSqlPersistMPool pool $ do
    select $ distinct $ from $ return

-- | Create all triples for representing the link between the RecordingWork
-- concept and an english synopsis.
mkAllFilmoResumesAnglaisRdf :: (RDF.Rdf rdfImpl, Monad m)
                            => [Entity FilmoResumesAnglais]
                            -> RdfState rdfImpl m ()
mkAllFilmoResumesAnglaisRdf filmoResumesEntities = do
  mapM_ mkFilmoResumesAnglaisRdf filmoResumesEntities

-- | Create triples for representing the link between the RecordingWork concept and
-- an english synopsis.
mkFilmoResumesAnglaisRdf :: (RDF.Rdf rdfImpl, Monad m)
                         => Entity FilmoResumesAnglais
                         -> RdfState rdfImpl m ()
mkFilmoResumesAnglaisRdf filmoResumeEntity = do
  let filmoId = sqlKeyToText $ filmoResumesAnglaisFilmoId $ entityVal filmoResumeEntity
  let filmoUri = "/resource/RecordingWork" <> filmoId

  case filmoResumesAnglaisResumeAnglais $ entityVal filmoResumeEntity of
    Just resume -> do
      addTriple $ RDF.triple (RDF.unode filmoUri)
                             (RDF.unode $ RDF.mkUri cmtqo "synopsis")
                             (RDF.lnode $ RDF.PlainLL resume "en")
    Nothing -> return ()

-- | Get all rows from Langue table in database.
getLangue :: (MonadIO m)
          => Pool SqlBackend
          -> m [Entity Langue]
getLangue pool = do
  liftIO $ flip liftSqlPersistMPool pool $ do
    select $ distinct $ from $ return

-- | Create all triples for representing the link between the Language concept.
mkAllLangueRdf :: (RDF.Rdf rdfImpl, Monad m)
               => [Entity Langue]
               -> RdfState rdfImpl m ()
mkAllLangueRdf langueEntities =
  mapM_ mkLangue langueEntities

-- | Create all triples for representing the Language concept.
mkLangue :: (RDF.Rdf rdfImpl, Monad m)
         => Entity Langue
         -> RdfState rdfImpl m ()
mkLangue langueEntity = do
  let languageId = sqlKeyToText $ entityKey langueEntity
  let languageUri = "/resource/Language" <> languageId
  let languageLabel = (langueTerme . entityVal) langueEntity

  addTriple $ RDF.triple (RDF.unode languageUri)
                         (RDF.unode $ RDF.mkUri RDF.rdf "type")
                         (RDF.unode $ RDF.mkUri cmtqo "Language")

  addTriple $ RDF.triple (RDF.unode languageUri)
                         (RDF.unode $ RDF.mkUri RDF.rdfs "label")
                         (RDF.lnode $ RDF.PlainL languageLabel)

-- | Get all rows from Langue_LienWikidata table in database.
getLangueLienWikidata :: (MonadIO m)
                      => Pool SqlBackend
                      -> m [Entity Langue_LienWikidata]
getLangueLienWikidata pool = do
  liftIO $ flip liftSqlPersistMPool pool $ do
    select $ distinct $ from $ return

-- | Create all triples for representing the link between the Language concept
-- and the equivalent concept in Wikidata.
mkAllLangueLienWikidataRdf :: (RDF.Rdf rdfImpl, Monad m)
                           => [Entity Langue_LienWikidata]
                           -> RdfState rdfImpl m ()
mkAllLangueLienWikidataRdf langueLienWikidataEntities =
  mapM_ mkLangueLienWikidata langueLienWikidataEntities

-- | Create all triples for representing the link between the Language concept
-- and the equivalent concept in Wikidata.
mkLangueLienWikidata :: (RDF.Rdf rdfImpl, Monad m)
                     => Entity Langue_LienWikidata
                     -> RdfState rdfImpl m ()
mkLangueLienWikidata langueLienWikidataEntity = do
  let languageId = sqlKeyToText $ langue_LienWikidataLangueId $ entityVal langueLienWikidataEntity
  let languageUri = "/resource/Language" <> languageId
  let wikidataUriMaybe = langue_LienWikidataLienWikidata $ entityVal langueLienWikidataEntity

  case wikidataUriMaybe of
    Just wikidataUri -> do
      addTriple $ RDF.triple (RDF.unode languageUri)
                             (RDF.unode $ RDF.mkUri RDF.owl "sameAs")
                             (RDF.unode wikidataUri)
    Nothing -> return ()

-- | Get all rows from Filmo_Langue table in database.
getFilmoLangue :: (MonadIO m)
               => Pool SqlBackend
               -> m [Entity Filmo_Langue]
getFilmoLangue pool = do
  liftIO $ flip liftSqlPersistMPool pool $ do
    select $ distinct $ from $ return

-- | Create all triples for linking recording works and languages for all rows
-- in table Filmo_Langue.
mkAllFilmoLangueRdf :: (RDF.Rdf rdfImpl, Monad m)
                    => [Entity Filmo_Langue]
                    -> RdfState rdfImpl m ()
mkAllFilmoLangueRdf filmoLangueEntities =
  mapM_ mkFilmoLangueRdf filmoLangueEntities

-- | Create all triples for representing the link between a recording work and
-- a place.
mkFilmoLangueRdf :: (RDF.Rdf rdfImpl, Monad m)
                 => Entity Filmo_Langue
                 -> RdfState rdfImpl m ()
mkFilmoLangueRdf filmoLangueEntity = do
  let filmoId = sqlKeyToText $ filmo_LangueFilmoId $ entityVal filmoLangueEntity
  let langueId = sqlKeyToText $ filmo_LangueLangueId $ entityVal filmoLangueEntity
  let recordWorkUri = "/resource/RecordingWork" <> filmoId
  let langueUri = "/resource/Language" <> langueId

  addTriple $ RDF.triple (RDF.unode recordWorkUri)
                         (RDF.unode $ RDF.mkUri wdt "P407")
                         (RDF.unode langueUri)

sqlKeyToText :: (ToBackendKey SqlBackend a) => Key a -> Text
sqlKeyToText key = Text.pack $ show $ fromSqlKey key

-- | Parses a DATE field in the database.
parseDateField :: Text -- ^ Date field in the format DD-MM-YY
               -> Maybe UTCTime
parseDateField date =
  parseTimeM False defaultTimeLocale "%d-%-m-%-y" (Text.unpack date) :: Maybe UTCTime

-- | Parses a YEAR field in the database.
parseYearField :: (Show a)
               => a -- ^ Year field in the format YYYY
               -> Maybe UTCTime
parseYearField year =
  parseTimeM False defaultTimeLocale "%-Y" (show year) :: Maybe UTCTime

-- | Formats a UTCTime in Text format. The format is: 1988-01-01T00:00:00Z
utcTimeToText :: UTCTime -> Text
utcTimeToText = Text.pack . show . fromUTCTime
