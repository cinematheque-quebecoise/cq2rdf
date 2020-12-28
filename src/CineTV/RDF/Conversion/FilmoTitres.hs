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
module CineTV.RDF.Conversion.FilmoTitres
  ( convertFilmoTitres
  )
where

import           Data.RDF.Types.Extended      (mkTriple, mkTripleLit)
import           Database.CineTv.Public.Model (FilmoTitres (..))
import           Import                       hiding ((^.))
import qualified SW.Vocabulary                as SW
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
for each row in table Langue
    cmtq:Work{FilmoTitres.workId} crm:P102_has_title cmtq:WorkTitle{FilmoTitres.id}
    cmtq:WorkTitle{FilmoTitres.id} rdf:type crm:E35_Title
    cmtq:WorkTitle{FilmoTitres.id} crm:P190_has_symbolic_content {FilmoTitres.Titre}
@
-}
convertFilmoTitres
  :: (RDF.Rdf rdfImpl, MonadIO m) => Pool SqlBackend -> RdfState rdfImpl m ()
convertFilmoTitres pool = do
  filmoTitres <- getFilmoTitresEntities pool

  mapM_ addFilmoTitresTriples filmoTitres

getFilmoTitresEntities
  :: (MonadIO m) => Pool SqlBackend -> m [Entity FilmoTitres]
getFilmoTitresEntities pool =
  liftIO $ flip liftSqlPersistMPool pool $ select $ distinct $ from return

addFilmoTitresTriples
  :: (RDF.Rdf rdfImpl, Monad m) => Entity FilmoTitres -> RdfState rdfImpl m ()
addFilmoTitresTriples filmoTitresEntity = do
  let filmoTitreId = sqlKeyToText $ entityKey filmoTitresEntity
  let filmoTitres  = entityVal filmoTitresEntity

  let workId       = sqlKeyToText $ filmoTitresFilmoId filmoTitres
  let workUri      = baseUriPath <> "/Work" <> workId
  let workTitleUri = baseUriPath <> "/WorkTitle" <> filmoTitreId

  mapM_ addTriple $ mkTriple workUri SW.crmP102 workTitleUri

  mapM_ addTriple $ mkTriple workTitleUri SW.rdfType SW.crmE35

  let title = mkTitleWork filmoTitres
  mapM_ addTriple $ mkTripleLit workTitleUri SW.crmP190 (RDF.PlainL title)

mkTitleWork :: FilmoTitres -> Text
mkTitleWork filmoTitres = do
  let restTitle = filmoTitresTitre filmoTitres
  case filmoTitresPrefixe filmoTitres of
    Just prefixTitle -> if Text.isSuffixOf "'" prefixTitle
      then prefixTitle <> "" <> restTitle
      else prefixTitle <> " " <> restTitle
    Nothing -> restTitle
