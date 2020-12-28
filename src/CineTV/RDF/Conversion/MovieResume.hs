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
module CineTV.RDF.Conversion.MovieResume
  ( convertMoviesResume
  )
where

import qualified Data.RDF.Types.Extended      as RDF (mkTriple, mkTripleLit)
import           Database.CineTv.Public.Model
import           Import                       hiding (isNothing, on, (^.))
import qualified SW.Vocabulary                as SW
import           Util                         (sqlKeyToText)

import           Data.Pool                    (Pool)
import qualified Data.RDF                     as RDF
import           Data.RDF.State
import           Database.Esqueleto           hiding (get)

data MovieSynopses = MovieSynopses
    { movieId  :: Text
    , synopses :: [Synopsis]
    }

data Synopsis = Synopsis
    { synopsisId     :: Text
    , synopsisText   :: Text
    , synopsisLangId :: Text
    }
    deriving (Eq)

baseUriPath :: Text
baseUriPath = "/resource"

synopsisTypeUri :: Text
synopsisTypeUri = baseUriPath <> "/Synopsis"

{-|
Create all triples for representing the resume of the work.

Generated triples:

@
cmtq:Synopsis rdf:type crm:E55_Type

for each row in table FilmoResumes
  cmtq:Synopsis{FilmoResumes.id}Language38 rdf:type crm:E33_Linguistic_Object
  cmtq:Synopsis{FilmoResumes.id}Language38 crm:P67_refers_to cmtq:Work{FilmoResumes.FilmoId}
  cmtq:Synopsis{FilmoResumes.id}Language38 crm:P2_has_type cmtq:Synopsis
  cmtq:Synopsis{FilmoResumes.id}Language38 crm:P190_has_symbolic_content {FilmoResumes.Resume}
  cmtq:Synopsis{FilmoResumes.id}Language38 crm:P72_has_language cmtq:Language38
  cmtq:Synopsis{FilmoResumes.id}Language38 crm:P73_has_translation cmtq:Synopsis{FilmoResumeAnglais.id}Language8

for each row in table FilmoResumesAnglais
  cmtq:Synopsis{FilmoResumesAnglais.id}Language38 rdf:type crm:E33_Linguistic_Object
  cmtq:Synopsis{FilmoResumesAnglais.id}Language38 crm:P67_refers_to cmtq:Work{FilmoResumesAnglais.FilmoId}
  cmtq:Synopsis{FilmoResumesAnglais.id}Language38 crm:P2_has_type cmtq:Synopsis
  cmtq:Synopsis{FilmoResumesAnglais.id}Language38 crm:P190_has_symbolic_content {FilmoResumesAnglais.ResumeAnglais}
  cmtq:Synopsis{FilmoResumesAnglais.id}Language38 crm:P72_has_language cmtq:Language8
  cmtq:Synopsis{FilmoResumesAnglais.id}Language38 crm:P73_has_translation cmtq:Synopsis{FilmoResume.id}Language38
@
-}
convertMoviesResume
  :: (RDF.Rdf rdfImpl, MonadIO m) => Pool SqlBackend -> RdfState rdfImpl m ()
convertMoviesResume pool = do
  createSynopsisType
  createTriplesFromMoviesResume pool

createSynopsisType :: (RDF.Rdf rdfImpl, Monad m) => RdfState rdfImpl m ()
createSynopsisType = do
  mapM_ addTriple $ RDF.mkTriple synopsisTypeUri SW.rdfType SW.crmE55
  mapM_ addTriple
    $ RDF.mkTripleLit synopsisTypeUri SW.rdfsLabel (RDF.PlainL "Synopsis")

createTriplesFromMoviesResume
  :: (RDF.Rdf rdfImpl, MonadIO m) => Pool SqlBackend -> RdfState rdfImpl m ()
createTriplesFromMoviesResume pool = do
  filmoResumesEntities <- getFilmoResumeEntities pool
  mapM_ createTriplesFromFilmoResume filmoResumesEntities

{-|
@
SELECT FilmoResumes.*, FilmoResumesAnglais.*
FROM   Filmo, FilmoResumes
       LEFT JOIN FilmoResumesAnglais
          ON FilmoResumes.filmoId = FilmoResumesAnglais.filmoId
WHERE Filmo.filmoid = FilmoResumes.filmoid
UNION ALL
SELECT FilmoResumes.*, FilmoResumesAnglais.*
FROM   Filmo, FilmoResumesAnglais
       LEFT JOIN FilmoResumes
          ON FilmoResumes.filmoId = FilmoResumesAnglais.filmoId
WHERE  Filmo.filmoid = FilmoResumesAnglais.filmoid and FilmoResumes.filmoId IS NULL
@
-}
getFilmoResumeEntities :: (MonadIO m) => Pool SqlBackend -> m [MovieSynopses]
getFilmoResumeEntities pool = do
  resumesPartOne <-
    liftIO
    $ flip liftSqlPersistMPool pool
    $ select
    $ distinct
    $ from
    $ \(filmo, filmoResumes `LeftOuterJoin` filmoResumesAnglais) -> do
        on
          (   just (filmoResumes ^. FilmoResumesFilmoId)
          ==. filmoResumesAnglais
          ?.  FilmoResumesAnglaisFilmoId
          )
        where_ (filmo ^. FilmoId ==. filmoResumes ^. FilmoResumesFilmoId)
        return (filmoResumes, filmoResumesAnglais)

  synopsesPartOne <- forM resumesPartOne $ \(resumeFr, resumeEnMaybe) -> do
    let synopsesMultiLang = catMaybes
          [ resumeEntityToSynopsis resumeFr
          , resumeAnglaisEntityToSynopsis =<< resumeEnMaybe
          ]
    -- let synopsisFr = resumeEntityToSynopsis resumeFr
    -- let synopsesMultiLang = case fmap resumeAnglaisEntityToSynopsis resumeEnMaybe of
    --       Just synopsisEn -> [ synopsisFr, synopsisEn ]
    --       Nothing -> [ synopsisFr ]

    let filmoId = sqlKeyToText $ filmoResumesFilmoId $ entityVal resumeFr
    return $ MovieSynopses filmoId synopsesMultiLang

  resumesPartTwo <-
    liftIO
    $ flip liftSqlPersistMPool pool
    $ select
    $ distinct
    $ from
    $ \(filmo, filmoResumesAnglais `LeftOuterJoin` filmoResumes) -> do
        on
          (   just (filmoResumesAnglais ^. FilmoResumesAnglaisFilmoId)
          ==. filmoResumes
          ?.  FilmoResumesFilmoId
          )
        where_
          (   filmo
          ^.  FilmoId
          ==. filmoResumesAnglais
          ^.  FilmoResumesAnglaisFilmoId
          &&. isNothing (filmoResumes ?. FilmoResumesFilmoId)
          )
        return filmoResumesAnglais

  synopsesPartTwo <- forM resumesPartTwo $ \resumeEn -> do
    let filmoId =
          sqlKeyToText $ filmoResumesAnglaisFilmoId $ entityVal resumeEn
    let synopsis = resumeAnglaisEntityToSynopsis resumeEn
    return $ MovieSynopses filmoId (catMaybes [synopsis])

  return $ synopsesPartOne ++ synopsesPartTwo

resumeEntityToSynopsis :: Entity FilmoResumes -> Maybe Synopsis
resumeEntityToSynopsis filmoResumesEntity =
  let resumeEntityId        = sqlKeyToText $ entityKey filmoResumesEntity
      resumeEntityTextMaybe = filmoResumesResume $ entityVal filmoResumesEntity
  in  fmap (\resumeText -> Synopsis resumeEntityId resumeText "38")
           resumeEntityTextMaybe

resumeAnglaisEntityToSynopsis :: Entity FilmoResumesAnglais -> Maybe Synopsis
resumeAnglaisEntityToSynopsis filmoAnglaisResumesEntity =
  let resumeEntityId = sqlKeyToText $ entityKey filmoAnglaisResumesEntity
      resumeEntityTextMaybe =
          filmoResumesAnglaisResumeAnglais $ entityVal filmoAnglaisResumesEntity
  in  fmap (\resumeText -> Synopsis resumeEntityId resumeText "8")
           resumeEntityTextMaybe

createTriplesFromFilmoResume
  :: (RDF.Rdf rdfImpl, Monad m) => MovieSynopses -> RdfState rdfImpl m ()
createTriplesFromFilmoResume movieSynopses = do
  let workUri = baseUriPath <> "/Work" <> movieId movieSynopses

  forM_ (synopses movieSynopses) $ \synopsis -> do
    let synopsisUri =
          baseUriPath
            <> "/Synopsis"
            <> synopsisId synopsis
            <> "Language"
            <> synopsisLangId synopsis
    let languageUri = baseUriPath <> "/Language" <> synopsisLangId synopsis

    mapM_ addTriple $ RDF.mkTriple workUri SW.crmP67 synopsisUri
    mapM_ addTriple $ RDF.mkTriple synopsisUri SW.rdfType SW.crmE33
    mapM_ addTriple $ RDF.mkTriple synopsisUri SW.crmP67 workUri
    mapM_ addTriple $ RDF.mkTriple synopsisUri SW.crmP2 synopsisTypeUri
    mapM_ addTriple $ RDF.mkTriple synopsisUri SW.crmP72 languageUri
    mapM_ addTriple $ RDF.mkTripleLit synopsisUri
                                      SW.crmP190
                                      (RDF.PlainL (synopsisText synopsis))

    forM_ (filter (synopsis /=) $ synopses movieSynopses) $ \otherSynopsis -> do
      let otherSynopsisUri =
            baseUriPath
              <> "/Synopsis"
              <> synopsisId otherSynopsis
              <> "Language"
              <> synopsisLangId otherSynopsis
      mapM_ addTriple $ RDF.mkTriple synopsisUri SW.crmP73 otherSynopsisUri
