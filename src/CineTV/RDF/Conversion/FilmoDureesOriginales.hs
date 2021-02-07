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
module CineTV.RDF.Conversion.FilmoDureesOriginales
  ( convertFilmoDureesOriginales
  )
where

import           Data.RDF.Types.Extended      (mkTriple, mkTripleLit)
import           Database.CineTv.Public.Model (FilmoDureesOriginales (..))
import           Import                       hiding ((^.))
import Data.RDF.Vocabulary
import           Util                         (sqlKeyToText)

import           Data.Pool                    (Pool)
import qualified Data.RDF                     as RDF
import           Data.RDF.State
import qualified Data.Text                    as Text
import           Database.Esqueleto           hiding (get)

baseUriPath :: Text
baseUriPath = "/resource"

{-|
Create all triples for representing the Language concept for all rows in table Langue.

Generated triples:

@
cmtq:Duration rdf:type crm:E55_Type
cmtq:Second rdf:type crm:E58_Measurement_Unit

for each row in table FilmoDureesOriginales
    cmtq:ManifestationProductType{FilmoDureesOriginales.filmoId} rdf:type frbroo:F3_Manifestation_Product_Type
    cmtq:ManifestationProductType{FilmoDureesOriginales.filmoId} frbroo:CLR6_should_carry cmtq:PublicationExpression{FilmoDureesOriginales.filmoId}
    cmtq:ManifestationProductType{FilmoDureesOriginales.filmoId} crm:P43_has_dimension cmtq:DimensionXSeconds
@
-}
convertFilmoDureesOriginales
  :: (RDF.Rdf rdfImpl, MonadIO m) => Pool SqlBackend -> RdfState rdfImpl m ()
convertFilmoDureesOriginales pool = do
  mapM_ addTriple $ mkTriple (baseUriPath <> "/Duration") rdfType crmE55
  mapM_ addTriple $ mkTriple (baseUriPath <> "/Second") rdfType crmE58

  filmoDureesOriginales <- getFilmoDureesOriginales pool
  mapM_ addFilmoDureesOriginalesTriples filmoDureesOriginales

getFilmoDureesOriginales
  :: (MonadIO m) => Pool SqlBackend -> m [Entity FilmoDureesOriginales]
getFilmoDureesOriginales pool =
  liftIO $ flip liftSqlPersistMPool pool $ select $ distinct $ from return

addFilmoDureesOriginalesTriples
  :: (RDF.Rdf rdfImpl, Monad m)
  => Entity FilmoDureesOriginales
  -> RdfState rdfImpl m ()
addFilmoDureesOriginalesTriples filmoDureesOriginalesEntity = do
  let filmoDureesOriginales = entityVal filmoDureesOriginalesEntity

  let filmoId =
        sqlKeyToText $ filmoDureesOriginalesFilmoId filmoDureesOriginales
  let minutes            = filmoDureesOriginalesMinutes filmoDureesOriginales
  let seconds            = filmoDureesOriginalesSecondes filmoDureesOriginales
  let totalSecondsText   = Text.pack $ show $ minutes * 60 + seconds

  let manifestationUri   = baseUriPath <> "/ManifestationProductType" <> filmoId
  let publicationExprUri = baseUriPath <> "/PublicationExpression" <> filmoId
  let dimensionUri =
        baseUriPath <> "/Dimension" <> totalSecondsText <> "Seconds"

  mapM_ addTriple $ mkTriple manifestationUri rdfType frbrooF3
  mapM_ addTriple $ mkTriple manifestationUri frbrooCLR6 publicationExprUri
  mapM_ addTriple $ mkTriple manifestationUri crmP43 dimensionUri

  mapM_ addTriple $ mkTriple dimensionUri rdfType crmE54
  mapM_ addTriple $ mkTriple dimensionUri crmP2 (baseUriPath <> "/Duration")
  mapM_ addTriple $ mkTripleLit dimensionUri
                                crmP90
                                (RDF.TypedL totalSecondsText xsdInteger)
  mapM_ addTriple $ mkTriple dimensionUri crmP91 (baseUriPath <> "/Second")
