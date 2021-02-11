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
module Data.CQLOD.Readers.CineTVSpec
  ( spec
  )
where

import           Data.CQLOD
import           Data.CQLOD.Readers.CineTV
import           Database.CineTv.Public.Model
import           Import
import Util (parseYearField, parseDateField)

import           Test.Hspec
import           Test.Hspec.Expectations.Extended (shouldContainElems)
-- import           Test.Hspec.QuickCheck
-- import           Test.QuickCheck
import           Data.Pool                          (Pool)
import           Database.Esqueleto hiding (get)
import           Database.Persist.Sqlite          (SqliteConf (..))

spec :: Spec
spec = do
  pool  <- runIO dbSetup
  cqlod <- runIO $ readCineTV pool
  let statements = cqlodStatements cqlod

  describe "readCineTV" $ do
    it "should read roles from table Fonction" $ do
      let filteredStatements = [ s | s@(RoleDeclaration _) <- statements]
      filteredStatements `shouldContainElems`
        [ RoleDeclaration (Role (RoleId "13") "Interprétation")
        , RoleDeclaration (Role (RoleId "34") "Société de production")
        , RoleDeclaration (Role (RoleId "33") "Source originale")
        ]
      length filteredStatements `shouldBe` 3

    -- it "should read roles linked to Wikidata from table Fonction_LienWikidata" $ do
    --   let filteredStatements = [ s | s@RoleWikidataLink {} <- statements]
    --   filteredStatements `shouldContainElems`
    --     [ RoleWikidataLink (RoleId "13") "http://www.wikidata.org/entity/Q33999"
    --     ]
    --   length filteredStatements `shouldBe` 1

    it "should read genres from table Sujet if linked to table Filmo_GenresCategories" $ do
      let filteredStatements = [ s | s@(GenreCategoryDeclaration _) <- statements]
      filteredStatements `shouldContainElems`
        [ GenreCategoryDeclaration (GenreCategory (GenreCategoryId "20006") "FICTION")
        ]
      length filteredStatements `shouldBe` 1

    it "should read genres linked to Wikidata from table GenresCategories_LienWikidata" $ do
      let filteredStatements = [ s | s@GenreCategoryWikidataLink {} <- statements]
      filteredStatements `shouldContainElems`
        [ GenreCategoryWikidataLink (GenreCategoryId "20006") (WikidataUri "http://www.wikidata.org/entity/Q8253")
        ]
      length filteredStatements `shouldBe` 1

    it "should read places from table Pays" $ do
      let placeStatements = [ s | s@(PlaceDeclaration _) <- statements]
      placeStatements `shouldContainElems`
        [ PlaceDeclaration (Place (PlaceId "216") "Canada : Québec")
        ]
      length placeStatements `shouldBe` 1

    it "should read places linked to Wikidata from table Pays_LienWikidata" $ do
      let filteredStatements = [ s | s@PlaceWikidataLink {} <- statements]
      filteredStatements `shouldContainElems`
        [ PlaceWikidataLink (PlaceId "216") (WikidataUri "http://www.wikidata.org/entity/Q176")
        ]
      length filteredStatements `shouldBe` 1

    it "should read people from table Nom if linked to table Filmo_Generique and Filmo_Realisation" $ do
      let filteredStatements = [ s | s@(PersonDeclaration _) <- statements]
      filteredStatements `shouldContainElems`
        [ PersonDeclaration (Person (PersonId "100") (Just "Ruggero") (Just "Maccari"))
        , PersonDeclaration (Person (PersonId "15334") (Just "Denys") (Just "Arcand"))
        ]
      length filteredStatements `shouldBe` 2

    it "should read people linked to Wikidata from table Nom_LienWikidata if linked to table Filmo_Generique and Filmo_Realisation" $ do
      let filteredStatements = [ s | s@PersonWikidataLink {} <- statements]
      filteredStatements `shouldContainElems`
        [ PersonWikidataLink (PersonId "100") (WikidataUri "http://www.wikidata.org/entity/Q968421")
        , PersonWikidataLink (PersonId "15334") (WikidataUri "http://www.wikidata.org/entity/Q363231")
        ]
      length filteredStatements `shouldBe` 2

    it "should read languages from table Langue" $ do
      let filteredStatements = [ s | s@(LanguageDeclaration _) <- statements]
      filteredStatements `shouldContainElems`
        [ LanguageDeclaration (Language (LanguageId "38") "français")
        ]
      length filteredStatements `shouldBe` 1

    it "should read language linked to Wikidata from table Langue_LienWikidata" $ do
      let filteredStatements = [ s | s@LanguageWikidataLink {} <- statements]
      filteredStatements `shouldContainElems`
        [ LanguageWikidataLink (LanguageId "38") (WikidataUri "http://www.wikidata.org/entity/Q150")
        ]
      length filteredStatements `shouldBe` 1

    it "should read legal bodies from table Sujet" $ do
      let filteredStatements = [ s | s@LegalBodyDeclaration {} <- statements]
      filteredStatements `shouldContainElems`
        [ LegalBodyDeclaration (LegalBody (LegalBodyId "19310") "PÈRE FILM (QUÉBEC)")
        ]
      length filteredStatements `shouldBe` 1

    it "should read synopsi from table FilmoResumes and FilmoResumesAnglais" $ do
      let filteredStatements = [ s | s@SynopsisDeclaration {} <- statements]
      filteredStatements `shouldContainElems`
        [ SynopsisDeclaration (Synopsis (SynopsisId "Work63563-38") "Résumé en français" (LanguageId "38"))
        , SynopsisDeclaration (Synopsis (SynopsisId "Work63563-8") "Resume in english" (LanguageId "8"))
        , SynopsisDeclaration (Synopsis (SynopsisId "Work2-38") "Résumé en français" (LanguageId "38"))
        , SynopsisDeclaration (Synopsis (SynopsisId "Work2-8") "Resume in english" (LanguageId "8"))
        , SynopsisDeclaration (Synopsis (SynopsisId "1-38") "Résumé en français 2" (LanguageId "38"))
        , SynopsisDeclaration (Synopsis (SynopsisId "1-8") "Resume in english 2" (LanguageId "8"))
        ]
      length filteredStatements `shouldBe` 6

    it "should read recording event locations from table Filmo_Pays" $ do
      let recordingLocationStatements = [ s | s@(RecordingEventLocation _ _) <- statements]
      recordingLocationStatements `shouldContainElems`
        [ RecordingEventLocation (RecordingEventId "63563") (PlaceId "216")
        ]
      length recordingLocationStatements `shouldBe` 1

    it "should read works from table Filmo" $ do
      let workStatements = [ s | s@(WorkDeclaration _) <- statements]
      workStatements `shouldContainElems`
        [ WorkDeclaration $ WorkId "63563"
        , WorkDeclaration $ WorkId "2"
        , WorkDeclaration $ WorkId "95672"
        , WorkDeclaration $ WorkId "95672"
        ]

    it "should read work linked to Wikidata from table Filmo_LienWikidata" $ do
      let filteredStatements = [ s | s@WorkWikidataLink {} <- statements]
      filteredStatements `shouldContainElems`
        [ WorkWikidataLink (WorkId "63563") (WikidataUri "http://www.wikidata.org/entity/Q549012")
        ]
      length filteredStatements `shouldBe` 1

    it "should read work's person original source from table Filmo_Generique" $ do
      let filteredStatements = [ s | s@WorkSourcePerson {} <- statements]
      filteredStatements `shouldContainElems`
        [ WorkSourcePerson (WorkId "63563") (WorkId "63563-100") (PersonId "100")
        ]
      length filteredStatements `shouldBe` 1

    it "should read work's legal body original source from table Filmo_Generique" $ do
      let filteredStatements = [ s | s@WorkSourceLegalBody {} <- statements]
      filteredStatements `shouldContainElems`
        [ WorkSourceLegalBody (WorkId "63563") (WorkId "63563-101") (LegalBodyId "19310")
        ]
      length filteredStatements `shouldBe` 1

    it "should have one recording work per work from table Filmo" $ do
      let workStatements = [ s | s@(RecordingWorkDeclaration _) <- statements]
      workStatements `shouldContainElems`
        [ RecordingWorkDeclaration $ RecordingWorkId "63563"
        , RecordingWorkDeclaration $ RecordingWorkId "2"
        , RecordingWorkDeclaration $ RecordingWorkId "95672"
        ]

    it "should have one recording work per work from table Filmo" $ do
      let workStatements = [ s | s@(RecordingWorkDeclaration _) <- statements]
      workStatements `shouldContainElems`
        [ RecordingWorkDeclaration $ RecordingWorkId "63563"
        , RecordingWorkDeclaration $ RecordingWorkId "2"
        , RecordingWorkDeclaration $ RecordingWorkId "95672"
        ]

    it "should link every recording work to derived work from table Filmo" $ do
      let workStatements = [ s | s@(RecordingWorkWorkDerivative _ _) <- statements]
      workStatements `shouldContainElems`
        [ RecordingWorkWorkDerivative (RecordingWorkId "63563") (WorkId "63563")
        , RecordingWorkWorkDerivative (RecordingWorkId "2") (WorkId "2")
        , RecordingWorkWorkDerivative (RecordingWorkId "95672") (WorkId "95672")
        ]

    it "should link every recording event to recording work" $ do
      let filteredStatements = [ s | s@(RecordingEventRealisation _ _) <- statements]
      filteredStatements `shouldContainElems`
        [ RecordingEventRealisation (RecordingEventId "63563") (RecordingWorkId "63563")
        , RecordingEventRealisation (RecordingEventId "2") (RecordingWorkId "2")
        , RecordingEventRealisation (RecordingEventId "95672") (RecordingWorkId "95672")
        ]

    it "should link every recording event to recording" $ do
      let filteredStatements = [ s | s@(RecordingEventCreatedRecording _ _) <- statements]
      filteredStatements `shouldContainElems`
        [ RecordingEventCreatedRecording (RecordingEventId "63563") (RecordingId "63563")
        , RecordingEventCreatedRecording (RecordingEventId "2") (RecordingId "2")
        , RecordingEventCreatedRecording (RecordingEventId "95672") (RecordingId "95672")
        ]

    it "should link every publication expression to recording" $ do
      let filteredStatements = [ s | s@(PublicationExpressionIncorporatesExpression _ _) <- statements]
      filteredStatements `shouldContainElems`
        [ PublicationExpressionIncorporatesExpression (PublicationExpressionId "63563") (RecordingId "63563")
        , PublicationExpressionIncorporatesExpression (PublicationExpressionId "2") (RecordingId "2")
        , PublicationExpressionIncorporatesExpression (PublicationExpressionId "95672") (RecordingId "95672")
        ]

    it "should have one publication event per work form table Filmo" $ do
      let filteredStatements = [ s | s@(PublicationEventDeclaration _) <- statements]
      filteredStatements `shouldContainElems`
        [ PublicationEventDeclaration (PublicationEventId "63563")
        , PublicationEventDeclaration (PublicationEventId "2")
        , PublicationEventDeclaration (PublicationEventId "95672")
        ]

    it "should link every publication event to publication expression" $ do
      let filteredStatements = [ s | s@(PublicationEventCreatedPublicationExpression _ _) <- statements]
      filteredStatements `shouldContainElems`
        [ PublicationEventCreatedPublicationExpression (PublicationEventId "63563") (PublicationExpressionId "63563")
        , PublicationEventCreatedPublicationExpression (PublicationEventId "2") (PublicationExpressionId "2")
        , PublicationEventCreatedPublicationExpression (PublicationEventId "95672") (PublicationExpressionId "95672")
        ]

    -- it "should link every publication event to public projection event" $ do
    --   let filteredStatements = [ s | s@(PublicationEventCreatedPublicationExpression _ _) <- statements]
    --   filteredStatements `shouldContainElems`
    --     [ PublicationEventCreatedPublicationExpressionStatement (PublicationEventId "63563") (PublicationExpressionId "63563")
    --     , PublicationEventCreatedPublicationExpressionStatement (PublicationEventId "2") (PublicationExpressionId "2")
    --     , PublicationEventCreatedPublicationExpressionStatement (PublicationEventId "95672") (PublicationExpressionId "95672")
    --     ]

    it "should have one recording event per work from table Filmo" $ do
      let filteredStatements = [ s | s@(RecordingEventDeclaration _) <- statements]
      filteredStatements `shouldContainElems`
        [ RecordingEventDeclaration $ RecordingEventId "63563"
        , RecordingEventDeclaration $ RecordingEventId "2"
        , RecordingEventDeclaration $ RecordingEventId "95672"
        ]

    it "should have one recording per work from table Filmo" $ do
      let filteredStatements = [ s | s@(RecordingDeclaration _) <- statements]
      filteredStatements `shouldContainElems`
        [ RecordingDeclaration $ RecordingId "63563"
        , RecordingDeclaration $ RecordingId "2"
        , RecordingDeclaration $ RecordingId "95672"
        ]

    it "should create statement linking recording with language from table Filmo_Langue" $ do
      let filteredStatements = [ s | s@RecordingLanguage {} <- statements]
      filteredStatements `shouldContainElems`
        [ RecordingLanguage (RecordingId "63563") (LanguageId "38")
        ]
      length filteredStatements `shouldBe` 1

    it "should have one publication expression per work from table Filmo" $ do
      let pubExprStatement = [ s | s@(PublicationExpressionDeclaration _) <- statements]
      pubExprStatement `shouldContainElems`
        [ PublicationExpressionDeclaration $ PublicationExpressionId "63563"
        , PublicationExpressionDeclaration $ PublicationExpressionId "2"
        , PublicationExpressionDeclaration $ PublicationExpressionId "95672"
        ]

    it "should read work original titles from table Filmo" $ do
      let filteredStatements = [ s | s@(WorkOriginalTitle _ _) <- statements]
      filteredStatements `shouldContainElems`
        [ WorkOriginalTitle (WorkId "63563") "LES INVASIONS BARBARES"
        , WorkOriginalTitle (WorkId "2") "L'HOMME DE L'ISLE"
        ]
      length filteredStatements `shouldBe` 3

    it "should read work other titles from table FilmoTitres" $ do
      let filteredStatements = [ s | s@WorkOtherTitle {} <- statements]
      filteredStatements `shouldContainElems`
        [ WorkOtherTitle (WorkId "63563") (WorkTitleId "1") "Les Invasions Barbares"
        ]
      length filteredStatements `shouldBe` 1

    it "should link work with genre from table Filmo_GenresCategories" $ do
      let filteredStatements = [ s | s@WorkGenreCategory {} <- statements]
      filteredStatements `shouldContainElems`
        [ WorkGenreCategory (WorkId "63563") (GenreCategoryId "20006")
        ]
      length filteredStatements `shouldBe` 1

    it "should create statements for work synopsis from table Filmo, FilmoResumes and FilmoResumesAnglais" $ do
      let filteredStatements = [ s | s@WorkSynopsis {} <- statements]
      filteredStatements `shouldContainElems`
        [ WorkSynopsis (WorkId "63563") (SynopsisId "Work63563-38")
        , WorkSynopsis (WorkId "63563") (SynopsisId "Work63563-8")
        , WorkSynopsis (WorkId "2") (SynopsisId "Work2-38")
        , WorkSynopsis (WorkId "2") (SynopsisId "Work2-8")
        , WorkSynopsis (WorkId "63563") (SynopsisId "1-38")
        , WorkSynopsis (WorkId "63563") (SynopsisId "1-8")
        ]
      length filteredStatements `shouldBe` 6

    it "should create statements for work production cost from table Filmo" $ do
      let filteredStatements = [ s | s@WorkProductionCost {} <- statements]
      filteredStatements `shouldContainElems`
        [ WorkProductionCost (WorkId "2") 2300000 CAD
        ]

    it "should create public projection event statements from each work from table Filmo with a release year" $ do
      let filteredStatements = [ s | s@(PublicProjectionEventDeclaration _) <- statements]
      filteredStatements `shouldContainElems`
        [ PublicProjectionEventDeclaration $ PublicProjectionEventId "63563"
        , PublicProjectionEventDeclaration $ PublicProjectionEventId "2"
        ]

    it "should create statement linking public projection event with publication expression from table Filmo" $ do
      let filteredStatements = [ s | s@PublicProjectionEventUsedPublicationExpression {} <- statements]
      filteredStatements `shouldContainElems`
        [ PublicProjectionEventUsedPublicationExpression (PublicProjectionEventId "63563") (PublicationExpressionId "63563")
        , PublicProjectionEventUsedPublicationExpression (PublicProjectionEventId "2") (PublicationExpressionId "2")
        , PublicProjectionEventUsedPublicationExpression (PublicProjectionEventId "95672") (PublicationExpressionId "95672")
        ]

    it "should create public projection event release date statements from each work from table Filmo with a release year" $ do
      let filteredStatements = [ s | s@(PublicProjectionEventTimeSpan _ _) <- statements]
      let year = 2003 :: Integer
      filteredStatements `shouldContainElems`
        [ PublicProjectionEventTimeSpan (PublicProjectionEventId "63563") (TimeSpan (parseYearField year) Nothing)
        , PublicProjectionEventTimeSpan (PublicProjectionEventId "2") (TimeSpan (parseYearField year) Nothing)
        ]

    it "should create recording event time span from each work from table Filmo" $ do
      let filteredStatements = [ s | s@RecordingEventTimeSpan {} <- statements]
      filteredStatements `shouldContainElems`
        [ RecordingEventTimeSpan (RecordingEventId "63563") (TimeSpan (parseDateField "01-01-11") (parseDateField "01-01-12"))
        , RecordingEventTimeSpan (RecordingEventId "2") (TimeSpan (parseDateField "01-01-11") (parseDateField "01-01-12"))
        , RecordingEventTimeSpan (RecordingEventId "95672") (TimeSpan Nothing Nothing)
        ]
      length filteredStatements `shouldBe` 3

    it "should create recording event activity from table Filmo_Generique" $ do
      let filteredStatements = [ s | s@RecordingEventActivity {} <- statements ]
      filteredStatements `shouldContainElems`
        [ RecordingEventActivity (RecordingEventId "63563") (RecordingActivityId "1")
        , RecordingEventActivity (RecordingEventId "63563") (RecordingActivityId "2")
        , RecordingEventActivity (RecordingEventId "63563") (RecordingActivityId "Director63563-15334")
        ]
      length filteredStatements `shouldBe` 3

    it "should create recording person activity from table Filmo_Generique" $ do
      let filteredStatements = [ s | s@RecordingActivityPersonDeclaration {} <- statements ]
      filteredStatements `shouldContainElems`
        [ RecordingActivityPersonDeclaration (RecordingActivityId "1") (PersonId "100") (RoleId "13")
        , RecordingActivityPersonDeclaration (RecordingActivityId "Director63563-15334") (PersonId "15334") (RoleId "1")
        ]
      length filteredStatements `shouldBe` 2

    it "should create recording legal body activity from table Filmo_Generique" $ do
      let filteredStatements = [ s | s@RecordingActivityLegalBodyDeclaration {} <- statements ]
      filteredStatements `shouldContainElems`
        [ RecordingActivityLegalBodyDeclaration (RecordingActivityId "2") (LegalBodyId "19310") (RoleId "34")
        ]
      length filteredStatements `shouldBe` 1

    it "should create manifestation product type declaration from table FilmoDureesOriginales" $ do
      let filteredStatements = [ s | s@ManifestationProductTypeDeclaration {} <- statements ]
      filteredStatements `shouldContainElems`
        [ ManifestationProductTypeDeclaration (ManifestationProductTypeId "63563")
        ]
      length filteredStatements `shouldBe` 1

    it "should create statement linking manifestation product type with publication expession from table FilmoDureesOriginales" $ do
      let filteredStatements = [ s | s@ManifestationProductTypeCarriesPublicationExpression {} <- statements ]
      filteredStatements `shouldContainElems`
        [ ManifestationProductTypeCarriesPublicationExpression (ManifestationProductTypeId "63563") (PublicationExpressionId "63563")
        ]
      length filteredStatements `shouldBe` 1

    it "should create statement linking manifestation product type with duration from table FilmoDureesOriginales" $ do
      let filteredStatements = [ s | s@ManifestationProductTypeDuration {} <- statements ]
      filteredStatements `shouldContainElems`
        [ ManifestationProductTypeDuration (ManifestationProductTypeId "63563") 6030
        ]
      length filteredStatements `shouldBe` 1

dbSetup :: IO (Pool SqlBackend)
dbSetup = do
  pool <- createPoolConfig (SqliteConf ":memory:" 1)
  _    <- liftIO $ flip liftSqlPersistMPool pool $ do
    runMigration migrateAll
    _ <- insertKey (toSqlKey 13) $ Fonction "Interprétation"
    _ <- insertKey (toSqlKey 34) $ Fonction "Société de production"
    _ <- insertKey (toSqlKey 33) $ Fonction "Source originale"
    -- _ <- insertKey (toSqlKey 13) $ Fonction_LienWikidata "http://www.wikidata.org/entity/Q33999"

    _ <- insertKey (toSqlKey 19310) $ Sujet "PÈRE FILM (QUÉBEC)"
    _ <- insertKey (toSqlKey 20006) $ Sujet "FICTION"
    _ <- insertKey (toSqlKey 20007) $ Sujet "GALA"
    _ <- insert $ GenresCategories_LienWikidata (toSqlKey 20006) (Just "http://www.wikidata.org/entity/Q8253")

    _ <- insertKey (toSqlKey 216) $ Pays "Canada : Québec"

    _ <- insert $ Pays_LienWikidata (toSqlKey 216)
                               (Just "http://www.wikidata.org/entity/Q176")

    _ <- insertKey (toSqlKey 100) $ Nom (Just "Maccari") (Just "Ruggero")
    _ <- insertKey (toSqlKey 101) $ Nom (Just "Altman") (Just "Robert")
    _ <- insertKey (toSqlKey 15334) $ Nom (Just "Arcand") (Just "Denys")
    _ <- insertKey (toSqlKey 42654) $ Nom (Just "Curzi") (Just "Pierre")
    _ <- insertKey (toSqlKey 151742) $ Nom (Just "Côté") (Just "Michel")
    _ <- insert $ Nom_LienWikidata
      (toSqlKey 100)
      (Just "http://www.wikidata.org/entity/Q968421")
    _ <- insert $ Nom_LienWikidata
      (toSqlKey 101)
      (Just "http://www.wikidata.org/entity/Q55163")
    _ <- insert $ Nom_LienWikidata
      (toSqlKey 15334)
      (Just "http://www.wikidata.org/entity/Q363231")
    _ <- insert $ Nom_LienWikidata (toSqlKey 151742) Nothing

    _ <- insertKey (toSqlKey 38) $ Langue "français"
    _ <- insert $ Langue_LienWikidata (toSqlKey 38) (Just "http://www.wikidata.org/entity/Q150")

    _ <- insertKey (toSqlKey 63563) $ Filmo (Just "LES")
                                   (Just "INVASIONS BARBARES")
                                   (Just 2003)
                                   Nothing
                                   Nothing
                                   Nothing
                                   Nothing
                                   (Just "Résumé en français")
                                   (Just "Resume in english")
                                   (Just "01-01-11")
                                   (Just "01-01-12")
                                   Nothing
                                   Nothing
                                   Nothing
    _ <- insertKey (toSqlKey 2) $ Filmo (Just "L'")
                                   (Just "HOMME DE L'ISLE")
                                   (Just 2003)
                                   Nothing
                                   Nothing
                                   Nothing
                                   Nothing
                                   (Just "Résumé en français")
                                   (Just "Resume in english")
                                   (Just "01-01-11")
                                   (Just "01-01-12")
                                   Nothing
                                   (Just 2300000)
                                   Nothing
    _ <- insertKey (toSqlKey 95672) $ Filmo Nothing
                                      (Just "19-2")
                                      Nothing
                                      Nothing
                                      Nothing
                                      Nothing
                                      Nothing
                                      Nothing
                                      Nothing
                                      Nothing
                                      Nothing
                                      Nothing
                                      Nothing
                                      Nothing

    _ <- insert $ Filmo_LienWikidata (toSqlKey 63563)
                                (Just "http://www.wikidata.org/entity/Q549012")

    _ <- insert $ Filmo_Realisation (toSqlKey 63563) (toSqlKey 15334)

    _ <- insert $ Filmo_Generique (toSqlKey 13)
                             (toSqlKey 63563)
                             Nothing
                             (Just $ toSqlKey 100)

    _ <- insert $ Filmo_Generique (toSqlKey 34)
                             (toSqlKey 63563)
                             (Just $ toSqlKey 19310)
                             Nothing

    _ <- insertKey (toSqlKey 100) $ Filmo_Generique (toSqlKey 33)
                             (toSqlKey 63563)
                             Nothing
                             (Just $ toSqlKey 100)

    _ <- insertKey (toSqlKey 101) $ Filmo_Generique (toSqlKey 33)
                             (toSqlKey 63563)
                             (Just $ toSqlKey 19310)
                             Nothing

    _ <- insert $ Filmo_Pays (toSqlKey 63563) (toSqlKey 216)

    _ <- insert $ Filmo_GenresCategories (toSqlKey 63563) (toSqlKey 20006)

    _ <- insertKey (toSqlKey 1) $ FilmoResumes (toSqlKey 63563) (Just "Résumé en français 2")
    _ <- insertKey (toSqlKey 1) $ FilmoResumesAnglais (toSqlKey 63563) (Just "Resume in english 2")

    _ <- insertKey (toSqlKey 1) $ TypeTitre "1"
    _ <- insert $ FilmoTitres (toSqlKey 63563) (toSqlKey 1) (Just "Les") "Invasions Barbares"

    _ <- insert $ Filmo_Langue (toSqlKey 63563) (toSqlKey 38)

    _ <- insert $ FilmoDureesOriginales (toSqlKey 63563) 100 30

    return ()

  return pool

