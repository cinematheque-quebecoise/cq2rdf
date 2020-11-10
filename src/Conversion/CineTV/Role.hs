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
module Conversion.CineTV.Role
  ( convertRoles
  )
where

import           Data.RDF.Types.Extended      (mkTriple, mkTripleLit)
import           Database.CineTv.Public.Model (Fonction (..))
import           Import                       hiding ((^.))
import qualified SW.Vocabulary                as SW
import           Util                         (sqlKeyToText)

import           Data.Pool                    (Pool)
import qualified Data.RDF                     as RDF
import           Data.RDF.State
import           Database.Esqueleto           hiding (get)

baseUriPath :: Text
baseUriPath = "/resource"

-- | The director role does not exist in CineTV database. Has to be manually created.
customDirectorRoleId :: Int64
customDirectorRoleId = 1

{-|
Create all triples for representing the Role concept for all rows in table Fonction.

Generated triples:

@
cmtq:Role rdf:type crm:E55_Type
cmtq:Role rdfs:label "Role"

for each row in table Fonction
 cmtq:Fonction{Fonction.FonctionID} rdf:type crm:E55_Type
 cmtq:Fonction{Fonction.FonctionID} rdfs:label {Fonction.Terme}@fr
 cmtq:Fonction{Fonction.FonctionID} crm:P2_has_type cmtq:Role
 cmtq:Fonction{Fonction.FonctionID} crm:P48 cmtq:IdentifierRole{Fonction.FonctionID}
 cmtq:IdentifierRole{Fonction.FonctionID} crm:P190 {Fonction.FonctionID}
@
-}
convertRoles
  :: (RDF.Rdf rdfImpl, MonadIO m) => Pool SqlBackend -> RdfState rdfImpl m ()
convertRoles pool = do
  fonctionEntities <- getFonctionEntities pool

  createRoleType
  createTriplesFromRoles fonctionEntities

createRoleType :: (RDF.Rdf rdfImpl, Monad m) => RdfState rdfImpl m ()
createRoleType = do
  let roleTypeUri = baseUriPath <> "/Role"
  mapM_ addTriple $ mkTriple roleTypeUri SW.rdfType SW.crmE55
  mapM_ addTriple $ mkTripleLit roleTypeUri SW.rdfsLabel (RDF.PlainL "Role")
  mapM_ addTriple $ mkTripleLit
    roleTypeUri
    SW.rdfsComment
    (RDF.PlainLL "Role occupé par un agent dans la production d'une oeuvre" "fr"
    )

getFonctionEntities :: (MonadIO m) => Pool SqlBackend -> m [Entity Fonction]
getFonctionEntities pool =
  liftIO
    $ flip liftSqlPersistMPool pool
    $ select
    $ distinct
    $ from
    $ \fonction -> return fonction

createTriplesFromRoles
  :: (RDF.Rdf rdfImpl, Monad m) => [Entity Fonction] -> RdfState rdfImpl m ()
createTriplesFromRoles fonctionEntities = do
  mapM_ createTriplesFromRole fonctionEntities

  -- The director role does not exist in CineTV database.
  -- Has to be manually created.
  -- WARNING: Be sure that the custom ID is not used in CineTV
  createTriplesFromRole
    $ Entity (toSqlKey customDirectorRoleId) (Fonction "Réalisation")

createTriplesFromRole
  :: (RDF.Rdf rdfImpl, Monad m) => Entity Fonction -> RdfState rdfImpl m ()
createTriplesFromRole fonctionEntity = do
  let roleTypeUri       = baseUriPath <> "/Role"
  let roleUri           = roleTypeUri <> roleTextId
  let roleIdentifierUri = baseUriPath <> "/IdentifierRole" <> roleTextId

  mapM_ addTriple $ mkTriple roleUri SW.crmP2 roleTypeUri
  mapM_ addTriple $ mkTriple roleUri SW.rdfType SW.crmE55
  mapM_ addTriple
    $ mkTripleLit roleUri SW.rdfsLabel (RDF.PlainLL roleLabel "fr")
  mapM_ addTriple $ mkTriple roleUri SW.crmP48 roleIdentifierUri
  mapM_ addTriple
    $ mkTripleLit roleIdentifierUri SW.crmP190 (RDF.PlainL roleTextId)

 where
  roleTextId = (sqlKeyToText . entityKey) fonctionEntity
  roleLabel  = (fonctionTerme . entityVal) fonctionEntity
