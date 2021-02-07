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
module Data.CQLOD.Readers.CineTV.FilmoResume
  ( readFilmoResume
  )
where

import           Data.CQLOD                   (CQLODStatements, addStatement, CQLODStatement (..),
                                               Synopsis (..), SynopsisId(..), WorkId(..), LanguageId(..))
import           Database.CineTv.Public.Model
import           Import                       hiding (isNothing, on, (^.))
import           Util                         (sqlKeyToText)

import           Data.Pool                    (Pool)
import           Database.Esqueleto           hiding (get)

data MovieSynopses = MovieSynopses
    { movieId  :: Text
    , synopses :: [Synopsis]
    }

readFilmoResume :: (MonadIO m) => Pool SqlBackend -> CQLODStatements m ()
readFilmoResume pool = do
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

  mapM_ createStatements (synopsesPartOne ++ synopsesPartTwo)

resumeEntityToSynopsis :: Entity FilmoResumes -> Maybe Synopsis
resumeEntityToSynopsis filmoResumesEntity =
  let resumeEntityId        = sqlKeyToText $ entityKey filmoResumesEntity
      resumeEntityTextMaybe = filmoResumesResume $ entityVal filmoResumesEntity
  in  fmap (\resumeText -> Synopsis (SynopsisId resumeEntityId) resumeText (LanguageId "38"))
           resumeEntityTextMaybe

resumeAnglaisEntityToSynopsis :: Entity FilmoResumesAnglais -> Maybe Synopsis
resumeAnglaisEntityToSynopsis filmoAnglaisResumesEntity =
  let resumeEntityId = sqlKeyToText $ entityKey filmoAnglaisResumesEntity
      resumeEntityTextMaybe =
          filmoResumesAnglaisResumeAnglais $ entityVal filmoAnglaisResumesEntity
  in  fmap (\resumeText -> Synopsis (SynopsisId resumeEntityId) resumeText (LanguageId "8"))
           resumeEntityTextMaybe

createStatements :: (Monad m) => MovieSynopses -> CQLODStatements m ()
createStatements movieSynopses = do
  let workId = WorkId $ movieId movieSynopses
  forM_ (synopses movieSynopses) $ \synopsis -> do
    let langId = synopsisLanguageId synopsis
    let synopsisWithLangId = SynopsisId $ unSynopsisId (synopsisId synopsis) <> "-" <> unLanguageId (synopsisLanguageId synopsis)
    addStatement $ SynopsisDeclaration (Synopsis synopsisWithLangId (synopsisText synopsis) langId)
    addStatement $ WorkSynopsis workId synopsisWithLangId

    forM_ (filter (synopsis /=) $ synopses movieSynopses) $ \otherSynopsis -> do
      let otherSynopsisWithLangId = SynopsisId $ unSynopsisId (synopsisId otherSynopsis) <> "-" <> unLanguageId (synopsisLanguageId otherSynopsis)
      addStatement $ SynopsisTranslation synopsisWithLangId otherSynopsisWithLangId
