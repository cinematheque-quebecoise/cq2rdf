{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module Run (run) where

import Import hiding ((^.))
import Model
import Namespaces
import Data.RDF.State
import qualified RIO.Map as Map
import qualified RIO.Text as Text
import qualified Data.Text.Lazy.IO as TextL
import Control.Monad.State
import Data.RDF (RDF)
import qualified Data.RDF as RDF
import qualified Data.RDF.Namespace as RDF
import Text.RDF.RDF4H.TurtleSerializer
import Data.Pool (Pool)
import Database.Esqueleto hiding (get)
import Database.Persist.Sqlite (SqliteConf(..))
import Data.Time.Clock (UTCTime(..), getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale, iso8601DateFormat)
import Data.Aeson (ToJSON(..))
import Data.Aeson.Text (encodeToLazyText)
import System.Directory (createDirectoryIfMissing)

data Person = Person { personId :: Text
                     , firstname :: Maybe Text
                     , lastname :: Maybe Text
                     }

data Role = Role { roleId :: Text
                 , roleLabel :: Text
                 }

data ProductionRoleActivity = ProductionRoleActivity { prodRoleActivityRoleId :: Text
                                                     , prodRoleActivityWorkId :: Text
                                                     , prodRoleActivityPersonId :: Text
                                                     }

instance ToJSON RDF.PrefixMappings

-- instance ToJSON RDF.PrefixMappings where
--   toJSON (RDF.PrefixMappings mappings) = toJSON mappings

test :: RIO App ()
test = do
  let emptyRdf = RDF.empty :: RDF RDF.TList
  let mappings = RDF.PrefixMappings $ Map.fromList [ ("mo", "http://purl.org/ontology/mo/") ]
  let graph1 = RDF.addPrefixMappings emptyRdf mappings True
  liftIO $ RDF.writeRdf (TurtleSerializer Nothing mappings) graph1
  logInfo "Hello"

run :: RIO App ()
run = do
  pool <- liftIO $ createPoolConfig (SqliteConf "cinetv.db" 1)

  logInfo "Conversion de CineTV en RDF..."

  currentTime <- liftIO $ fmap (Text.pack . (formatTime defaultTimeLocale (iso8601DateFormat Nothing)) . utctDay) getCurrentTime

  let emptyRdf = RDF.empty :: RDF RDF.TList
  graph <- liftIO $ execStateT (cq2rdf pool) emptyRdf

  let outputDir = currentTime
  liftIO $ createDirectoryIfMissing True $ Text.unpack outputDir

  let prefixesFpath = Text.unpack $ outputDir <> "/prefixes.json"
  liftIO $ TextL.writeFile prefixesFpath $ encodeToLazyText $ RDF.prefixMappings graph

  let cmtqFpath = Text.unpack $ outputDir <> "/cmtq-dataset-" <> currentTime <> ".ttl"
  liftIO $ withFile cmtqFpath WriteMode (\h -> RDF.hWriteRdf (TurtleSerializer Nothing prefixMappings) h graph)

  logInfo "Conversion terminée!"

  -- Vérifier que les données sont valides

cq2rdf :: (MonadIO m)
       => Pool SqlBackend
       -> RdfState RDF.TList m ()
cq2rdf pool = do
  addPrefixMappings prefixMappings False
  addTriple $ RDF.triple (RDF.unode $ RDF.mkUri cmtq "Filmo1")
                         (RDF.unode $ RDF.mkUri RDF.rdf "type")
                         (RDF.unode $ RDF.mkUri cmtq "Recording_Work")

  people <- liftIO $ getPeople pool
  createPeopleTriples people

  roles <- liftIO $ getRoles pool
  createRolesTriples roles

  productionRoleActivities <- liftIO $ getProductionRoleActivities pool
  createWorkProductionTriples productionRoleActivities

  filmos <- liftIO $ getFilmos pool
  createFilmosTriples filmos

  subjects <- liftIO $ getSubjects pool
  createSubjectsTriples subjects

  filmosSubject <- liftIO $ getFilmosSubject pool
  createFilmosSubjectTriples filmosSubject

  filmosDirector <- liftIO $ getFilmosDirector pool
  createFilmosDirectorTriples filmosDirector

  filmosPlaces <- liftIO $ getFilmosPlaces pool
  createFilmosPlacesTriples filmosPlaces

  places <- liftIO $ getPlaces pool
  createPlacesTriples places

  return ()

createPeopleTriples :: (RDF.Rdf rdfImpl, Monad m)
                    => [Person]
                    -> RdfState rdfImpl m ()
createPeopleTriples people = mapM_ createPersonTriples people

createPersonTriples :: (RDF.Rdf rdfImpl, Monad m)
                    => Person
                    -> RdfState rdfImpl m ()
createPersonTriples person = do
  let personUri = RDF.mkUri cmtq $ "Person" <> personId person

  addTriple $ RDF.triple (RDF.unode personUri)
                         (RDF.unode $ RDF.mkUri RDF.rdf $ "type")
                         (RDF.unode $ RDF.mkUri crm $ "E21_Person")

  addTriple $ RDF.triple (RDF.unode personUri)
                         (RDF.unode $ RDF.mkUri cmtqo $ "cmtq_id")
                         (RDF.lnode $ RDF.PlainL $ personId person)

  case nameOpt of
    Just name -> do
      addTriple $ RDF.triple (RDF.unode personUri)
                             (RDF.unode $ RDF.mkUri RDF.foaf $ "name")
                             (RDF.lnode $ RDF.PlainL name)
      addTriple $ RDF.triple (RDF.unode personUri)
                             (RDF.unode $ RDF.mkUri RDF.rdfs $ "label")
                             (RDF.lnode $ RDF.PlainL name)
    Nothing -> return ()

  case lastname person of
    Just l -> addTriple $ RDF.triple (RDF.unode personUri)
                                     (RDF.unode $ RDF.mkUri RDF.foaf $ "familyName")
                                     (RDF.lnode $ RDF.PlainL l)
    Nothing -> return ()

  case firstname person of
    Just f -> addTriple $ RDF.triple (RDF.unode personUri)
                                     (RDF.unode $ RDF.mkUri RDF.foaf $ "givenName")
                                     (RDF.lnode $ RDF.PlainL f)
    Nothing -> return ()

  where
    nameOpt = case (firstname person, lastname person) of
      (f@(Just _), l@(Just _)) -> f <> (Just " ") <> l
      (f@(Just _), Nothing) -> f
      (Nothing, l@(Just _)) -> l
      (Nothing, Nothing) -> Nothing

getPeople :: Pool SqlBackend -> IO [Person]
getPeople pool = do
  peopleResults <- liftIO $ flip runSqlPersistMPool pool $ do
    select $
      distinct $
      from $ \nom -> do
      return nom

  return $ fmap toPerson peopleResults

toPerson :: Entity Nom -> Person
toPerson nomTable = Person (Text.pack $ show $ fromSqlKey nomKey)
                           (nomNom nomVal)
                           (nomPrenom nomVal)
  where nomVal = entityVal nomTable
        nomKey = entityKey nomTable

createRolesTriples :: (RDF.Rdf rdfImpl, Monad m)
                    => [Role]
                    -> RdfState rdfImpl m ()
createRolesTriples roles = do
  mapM_ createRoleTriples roles

  createRoleTriples $ Role "Director" "Réalisation"

createRoleTriples :: (RDF.Rdf rdfImpl, Monad m)
                    => Role
                    -> RdfState rdfImpl m ()
createRoleTriples role = do
  let roleUri = RDF.mkUri cmtq $ "Role" <> roleId role

  addTriple $ RDF.triple (RDF.unode roleUri)
                         (RDF.unode $ RDF.mkUri RDF.rdf $ "type")
                         (RDF.unode $ RDF.mkUri crm $ "E55_Type")

  addTriple $ RDF.triple (RDF.unode roleUri)
                         (RDF.unode $ RDF.mkUri cmtqo $ "cmtq_id")
                         (RDF.lnode $ RDF.PlainL $ roleId role)

  addTriple $ RDF.triple (RDF.unode roleUri)
                         (RDF.unode $ RDF.mkUri RDF.rdfs $ "label")
                         (RDF.lnode $ RDF.PlainL $ roleLabel role)

getRoles :: Pool SqlBackend
         -> IO [Role]
getRoles pool = do
  rolesResults <- liftIO $ flip runSqlPersistMPool pool $ do
    select $
      distinct $
      from $ \fonction -> do
      return fonction

  return $ fmap toRole rolesResults

toRole :: Entity Fonction -> Role
toRole fonctionTable = Role (Text.pack $ show $ fromSqlKey $ entityKey fonctionTable)
                            (fonctionTerme $ entityVal fonctionTable)

createWorkProductionTriples :: (RDF.Rdf rdfImpl, Monad m)
                            => [ProductionRoleActivity]
                            -> RdfState rdfImpl m ()
createWorkProductionTriples prodEvents = do
  mapM_ createProductionRoleActivityTriples prodEvents

createProductionRoleActivityTriples :: (RDF.Rdf rdfImpl, Monad m)
                                    => ProductionRoleActivity
                                    -> RdfState rdfImpl m ()
createProductionRoleActivityTriples prod = do
  let prodEventUri = RDF.mkUri cmtq $ "ProductionEvent" <> prodRoleActivityWorkId prod
  let prodRoleActivityUri = RDF.mkUri cmtq $ "ProductionRoleActivity"
                                          <> prodRoleActivityWorkId prod
                                          <> "-"
                                          <> prodRoleActivityRoleId prod
                                          <> "-"
                                          <> prodRoleActivityPersonId prod
  let prodRoleActivityRoleUri = RDF.mkUri cmtq $ "Role" <> prodRoleActivityRoleId prod
  let prodRoleActivityPersonUri = RDF.mkUri cmtq $ "Person" <> prodRoleActivityPersonId prod

  addTriple $ RDF.triple (RDF.unode prodRoleActivityUri)
                         (RDF.unode $ RDF.mkUri cmtqo $ "participated_in")
                         (RDF.unode prodEventUri)

  addTriple $ RDF.triple (RDF.unode prodRoleActivityUri)
                         (RDF.unode $ RDF.mkUri RDF.rdf $ "type")
                         (RDF.unode $ RDF.mkUri cmtqo $ "ProductionRoleActivity")

  addTriple $ RDF.triple (RDF.unode prodRoleActivityUri)
                         (RDF.unode $ RDF.mkUri cmtqo $ "has_role")
                         (RDF.unode prodRoleActivityRoleUri)

  addTriple $ RDF.triple (RDF.unode prodRoleActivityUri)
                         (RDF.unode $ RDF.mkUri cmtqo $ "executed_by")
                         (RDF.unode prodRoleActivityPersonUri)

getProductionRoleActivities :: Pool SqlBackend
                            -> IO [ProductionRoleActivity]
getProductionRoleActivities pool = do
  results <- liftIO $ flip runSqlPersistMPool pool $ do
    select $
      distinct $
      from $ \filmoGenerique -> do
      return filmoGenerique

  return $ catMaybes $ fmap toProductionRoleActivity results

toProductionRoleActivity :: Entity Filmo_Generique
                         -> Maybe ProductionRoleActivity
toProductionRoleActivity prodEventTable =
  case personIdMaybe of
    Just prodRolePersonId ->
      Just $ ProductionRoleActivity (sqlKeyToText $ filmo_GeneriqueFonctionId $ entityVal prodEventTable)
                                    (sqlKeyToText $ filmo_GeneriqueFilmoId $ entityVal prodEventTable)
                                    (sqlKeyToText $ prodRolePersonId)
    Nothing -> Nothing

  where personIdMaybe = filmo_GeneriqueNomId $ entityVal prodEventTable

createFilmosTriples :: (RDF.Rdf rdfImpl, Monad m)
                    => [Entity Filmo]
                    -> RdfState rdfImpl m ()
createFilmosTriples filmos = do
  mapM_ createFilmoTriples filmos

createFilmoTriples :: (RDF.Rdf rdfImpl, Monad m)
                   => Entity Filmo
                   -> RdfState rdfImpl m ()
createFilmoTriples filmoEntity = do
  let filmoUri = RDF.mkUri cmtq $ "RecordingWork" <> filmoId

  addTriple $ RDF.triple (RDF.unode filmoUri)
                         (RDF.unode $ RDF.mkUri RDF.rdf $ "type")
                         (RDF.unode $ RDF.mkUri cmtqo $ "RecordingWork")

  addTriple $ RDF.triple (RDF.unode filmoUri)
                         (RDF.unode $ RDF.mkUri cmtqo $ "cmtq_id")
                         (RDF.lnode $ RDF.PlainL $ filmoId)

  addTriple $ RDF.triple (RDF.unode $ RDF.mkUri cmtq $ "ProductionEvent" <> filmoId)
                         (RDF.unode $ RDF.mkUri cmtqo $ "realises")
                         (RDF.unode filmoUri)

  addTriple $ RDF.triple (RDF.unode $ RDF.mkUri cmtq $ "ProductionEvent" <> filmoId)
                         (RDF.unode $ RDF.mkUri RDF.rdf $ "type")
                         (RDF.unode $ RDF.mkUri cmtqo $ "ProductionEvent")

  case titleMaybe of
    Just title -> addTriple $ RDF.triple (RDF.unode filmoUri)
                                         (RDF.unode $ RDF.mkUri RDF.rdfs $ "label")
                                         (RDF.lnode $ RDF.PlainL $ title)
    Nothing -> return ()

  if isJust releaseYearMaybe
  then do
    let publicReleaseUri = RDF.mkUri cmtq $ "WorkPublicRelease" <> filmoId

    addTriple $ RDF.triple (RDF.unode filmoUri)
                           (RDF.unode $ RDF.mkUri cmtqo $ "release_event")
                           (RDF.unode publicReleaseUri)

    addTriple $ RDF.triple (RDF.unode publicReleaseUri)
                           (RDF.unode $ RDF.mkUri RDF.rdf $ "type")
                           (RDF.unode $ RDF.mkUri cmtqo $ "WorkPublicRelease")

    createRessourceTimeSpanTriples publicReleaseUri releaseYearMaybe Nothing
  else
    return ()

  createProdEventTimeSpanTriples filmoEntity

  where
    filmoId = sqlKeyToText $ entityKey filmoEntity
    filmo = entityVal filmoEntity

    titleMaybe = case (filmoPrefixeTitreOriginal filmo, filmoTitreOriginal filmo) of
                   (Just prefixTitle, Just restTitle) -> Just $ prefixTitle <> " " <> restTitle
                   (Nothing, Just restTitle) -> Just restTitle
                   _ -> Nothing

    releaseYearMaybe = fmap (Text.pack . show) $ filmoAnneeSortie filmo

createProdEventTimeSpanTriples :: (RDF.Rdf rdfImpl, Monad m)
                               => Entity Filmo
                               -> RdfState rdfImpl m ()
createProdEventTimeSpanTriples filmoEntity = do
  let beginDateMaybe = dateBeginProdMaybe <|> yearBeginProdMaybe
  let endDateMaybe = dateEndProdMaybe <|> yearEndProdMaybe

  createRessourceTimeSpanTriples (RDF.mkUri cmtq $ "ProductionEvent" <> filmoId)
                                 beginDateMaybe
                                 endDateMaybe

  where
    filmoId = sqlKeyToText $ entityKey filmoEntity
    filmo = entityVal filmoEntity

    dateBeginProdMaybe = filmoDateDebProd filmo
    dateEndProdMaybe = filmoDateFinProd filmo

    yearBeginProdMaybe = fmap (Text.pack . show) $ filmoAnneeDebProd filmo
    yearEndProdMaybe = fmap (Text.pack . show) $ filmoAnneeFinProd filmo

createRessourceTimeSpanTriples :: (RDF.Rdf rdfImpl, Monad m)
                               => Text -- ^ Ressource URI
                               -> Maybe Text -- ^ Begin date
                               -> Maybe Text -- ^ End date
                               -> RdfState rdfImpl m ()
createRessourceTimeSpanTriples ressourceUri beginDateMaybe endDateMaybe = do
  if isJust $ beginDateMaybe <|> endDateMaybe
  then do
    let lit = Text.intercalate "-" $ catMaybes [beginDateMaybe, endDateMaybe]
    let ressourceTimeSpanUri = RDF.mkUri cmtq $ "Time-Span" <> lit
    let ressourceTimePrimitiveUri = RDF.mkUri cmtq $ "Time_Primitive" <> lit

    addTriple $ RDF.triple (RDF.unode $ ressourceUri)
                           (RDF.unode $ RDF.mkUri crm $ "P4_has_time-span")
                           (RDF.unode $ ressourceTimeSpanUri)

    addTriple $ RDF.triple (RDF.unode ressourceTimeSpanUri)
                           (RDF.unode $ RDF.mkUri crm $ "P82_at_some_time_within")
                           (RDF.unode $ ressourceTimePrimitiveUri)

    case beginDateMaybe of
      Just beginDate -> do
        addTriple $ RDF.triple (RDF.unode ressourceTimePrimitiveUri)
                               (RDF.unode $ RDF.mkUri cmtqo $ "begin_date")
                               (RDF.lnode $ RDF.TypedL beginDate ("xsd:datetime"))
      Nothing -> return ()

    case endDateMaybe of
      Just endDate -> do
        addTriple $ RDF.triple (RDF.unode ressourceTimePrimitiveUri)
                               (RDF.unode $ RDF.mkUri cmtqo $ "end_date")
                               (RDF.lnode $ RDF.TypedL endDate ("xsd:datetime"))
      Nothing -> return ()

  else
    return ()

getFilmos :: Pool SqlBackend
          -> IO [Entity Filmo]
getFilmos pool = do
  liftIO $ flip runSqlPersistMPool pool $ do
    select $
      distinct $
      from $ \filmo -> do
      return filmo

createSubjectsTriples :: (RDF.Rdf rdfImpl, Monad m)
                      => [Entity Sujet]
                      -> RdfState rdfImpl m ()
createSubjectsTriples subjects = do
  mapM_ createSubjectTriples subjects

createSubjectTriples :: (RDF.Rdf rdfImpl, Monad m)
                     => Entity Sujet
                     -> RdfState rdfImpl m ()
createSubjectTriples subjectEntity = do
  let subjectUri = RDF.mkUri cmtq $ "Subject" <> subjectId

  addTriple $ RDF.triple (RDF.unode subjectUri)
                         (RDF.unode $ RDF.mkUri RDF.rdf $ "type")
                         (RDF.unode $ RDF.mkUri cmtqo $ "Subject")

  addTriple $ RDF.triple (RDF.unode subjectUri)
                         (RDF.unode $ RDF.mkUri RDF.rdfs $ "label")
                         (RDF.lnode $ RDF.PlainL $ sujetTerme subject)

  where
    subjectId = sqlKeyToText $ entityKey subjectEntity
    subject = entityVal subjectEntity

getSubjects :: Pool SqlBackend
            -> IO [Entity Sujet]
getSubjects pool = do
  liftIO $ flip runSqlPersistMPool pool $ do
    select $
      distinct $
      from $ \sujet -> do
      return sujet

createFilmosSubjectTriples :: (RDF.Rdf rdfImpl, Monad m)
                           => [Entity Filmo_GenresCategories]
                           -> RdfState rdfImpl m ()
createFilmosSubjectTriples filmosSubject =
  mapM_ createFilmoSubjectTriples filmosSubject

createFilmoSubjectTriples :: (RDF.Rdf rdfImpl, Monad m)
                          => Entity Filmo_GenresCategories
                          -> RdfState rdfImpl m ()
createFilmoSubjectTriples filmoSubject = do
  let filmoUri = RDF.mkUri cmtq $ "RecordingWork" <> filmoId
  let subjectUri = RDF.mkUri cmtq $ "Subject" <> subjectId

  addTriple $ RDF.triple (RDF.unode filmoUri)
                         (RDF.unode $ RDF.mkUri cmtqo $ "has_subject")
                         (RDF.unode $ subjectUri)

  where
    filmoId = sqlKeyToText $ filmo_GenresCategoriesFilmoId $ entityVal filmoSubject
    subjectId = sqlKeyToText $ filmo_GenresCategoriesSujetId $ entityVal filmoSubject

getFilmosSubject :: Pool SqlBackend
                 -> IO [Entity Filmo_GenresCategories]
getFilmosSubject pool = do
  liftIO $ flip runSqlPersistMPool pool $ do
    select $
      distinct $
      from $ \filmoGenresCategories -> do
      return filmoGenresCategories

createFilmosDirectorTriples :: (RDF.Rdf rdfImpl, Monad m)
                            => [(Key Filmo, Key Nom)]
                            -> RdfState rdfImpl m ()
createFilmosDirectorTriples filmosDirector =
  mapM_ createFilmoDirectorTriples filmosDirector

createFilmoDirectorTriples :: (RDF.Rdf rdfImpl, Monad m)
                           => (Key Filmo, Key Nom)
                           -> RdfState rdfImpl m ()
createFilmoDirectorTriples (filmoKey, nomKey) = do
  let filmoId = sqlKeyToText filmoKey
  let directorId = sqlKeyToText nomKey
  let prodRoleActivityUri = RDF.mkUri cmtq $ "ProductionRoleActivity"
                                          <> filmoId
                                          <> "-Director-"
                                          <> directorId
  let prodRoleActivityRoleUri = RDF.mkUri cmtq $ "RoleDirector"
  let prodRoleActivityPersonUri = RDF.mkUri cmtq $ "Person" <> directorId

  addTriple $ RDF.triple (RDF.unode prodRoleActivityUri)
                         (RDF.unode $ RDF.mkUri cmtqo $ "participated_in")
                         (RDF.unode $ RDF.mkUri cmtq $ "ProductionEvent" <> filmoId)

  addTriple $ RDF.triple (RDF.unode prodRoleActivityUri)
                         (RDF.unode $ RDF.mkUri cmtqo $ "has_role")
                         (RDF.unode prodRoleActivityRoleUri)

  addTriple $ RDF.triple (RDF.unode prodRoleActivityUri)
                         (RDF.unode $ RDF.mkUri cmtqo $ "executed_by")
                         (RDF.unode prodRoleActivityPersonUri)
  return ()

getFilmosDirector :: Pool SqlBackend
                  -> IO [(Key Filmo, Key Nom)]
                  -- -> IO [Entity Filmo_Realisation]
getFilmosDirector pool = do
  filmosDirector <- liftIO $ flip runSqlPersistMPool pool $ do
    select $
      distinct $
      from $ \filmoRealisation -> do
      return (filmoRealisation ^. Filmo_RealisationFilmoId, filmoRealisation ^. Filmo_RealisationNomId)
  return $ fmap (\(Value filmoKey, Value nomKey) -> (filmoKey, nomKey)) filmosDirector

createFilmosPlacesTriples :: (RDF.Rdf rdfImpl, Monad m)
                          => [(Key Filmo, Key Pays)]
                          -> RdfState rdfImpl m ()
createFilmosPlacesTriples filmosPlaces =
  mapM_ createFilmoPlaceTriples filmosPlaces

createFilmoPlaceTriples :: (RDF.Rdf rdfImpl, Monad m)
                        => (Key Filmo, Key Pays)
                        -> RdfState rdfImpl m ()
createFilmoPlaceTriples (filmoKey, placeKey) = do
  let filmoId = sqlKeyToText filmoKey
  let placeId = sqlKeyToText placeKey
  let prodEventUri = RDF.mkUri cmtq $ "ProductionEvent" <> filmoId
  let placeUri = RDF.mkUri cmtq $ "Place" <> placeId

  addTriple $ RDF.triple (RDF.unode prodEventUri)
                         (RDF.unode $ RDF.mkUri crm $ "P7_took_place_at")
                         (RDF.unode placeUri)

getFilmosPlaces :: Pool SqlBackend
                -> IO [(Key Filmo, Key Pays)]
getFilmosPlaces pool = do
  filmosCountries <- liftIO $ flip runSqlPersistMPool pool $ do
    select $
      distinct $
      from $ \filmoPays -> do
      return (filmoPays ^. Filmo_PaysFilmoId, filmoPays ^. Filmo_PaysPaysId)
  return $ fmap (\(Value filmoKey, Value paysKey) -> (filmoKey, paysKey)) filmosCountries

createPlacesTriples :: (RDF.Rdf rdfImpl, Monad m)
                    => [Entity Pays]
                    -> RdfState rdfImpl m ()
createPlacesTriples places =
  mapM_ createPlaceTriples places

createPlaceTriples :: (RDF.Rdf rdfImpl, Monad m)
                   => Entity Pays
                   -> RdfState rdfImpl m ()
createPlaceTriples placeEntity = do
  let placeId = sqlKeyToText $ entityKey placeEntity
  let place = entityVal placeEntity
  let placeUri = RDF.mkUri cmtq $ "Place" <> placeId

  addTriple $ RDF.triple (RDF.unode placeUri)
                         (RDF.unode $ RDF.mkUri RDF.rdf $ "type")
                         (RDF.unode $ RDF.mkUri crm $ "E53_Place")

  addTriple $ RDF.triple (RDF.unode placeUri)
                         (RDF.unode $ RDF.mkUri RDF.rdfs $ "label")
                         (RDF.lnode $ RDF.PlainL $ paysTerme place)

getPlaces :: Pool SqlBackend
          -> IO [Entity Pays]
getPlaces pool = do
  liftIO $ flip runSqlPersistMPool pool $ do
    select $
      distinct $
      from $ \pays -> do
      return pays

sqlKeyToText :: (ToBackendKey SqlBackend a) => Key a -> Text
sqlKeyToText key = Text.pack $ show $ fromSqlKey key
