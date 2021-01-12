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

module Data.RDF.State where

import           Control.Monad.State
import           Data.RDF            (RDF)
import qualified Data.RDF            as RDF
import           Import

-- newtype RdfState rdfImpl m a = RdfState { unRdfState :: StateT (RDF rdfImpl) m a }
--   deriving (Functor, Applicative, Monad, MonadState (RDF rdfImpl))
type RdfState rdfImpl m a = StateT (RDF rdfImpl) m a

mkRdf
  :: (RDF.Rdf rdfImpl, Monad m)
  => RDF.Triples
  -> Maybe RDF.BaseUrl
  -> RDF.PrefixMappings
  -> RdfState rdfImpl m ()
mkRdf triples baseUrlMaybe mappings =
  put $ RDF.mkRdf triples baseUrlMaybe mappings

addTriple :: (RDF.Rdf rdfImpl, Monad m) => RDF.Triple -> RdfState rdfImpl m ()
addTriple triple = do
  graph <- get
  put $ RDF.addTriple graph triple

removeTriple
  :: (RDF.Rdf rdfImpl, Monad m) => RDF.Triple -> RdfState rdfImpl m ()
removeTriple triple = do
  graph <- get
  put $ RDF.removeTriple graph triple

addPrefixMappings
  :: (RDF.Rdf rdfImpl, Monad m)
  => RDF.PrefixMappings
  -> Bool
  -> RdfState rdfImpl m ()
addPrefixMappings mappings replaceOldMapping = do
  graph <- get
  put $ RDF.addPrefixMappings graph mappings replaceOldMapping

empty :: (RDF.Rdf rdfImpl, Monad m) => RdfState rdfImpl m ()
empty = put RDF.empty

query
  :: (RDF.Rdf rdfImpl, Monad m)
  => Maybe RDF.Node
  -> Maybe RDF.Node
  -> Maybe RDF.Node
  -> RdfState rdfImpl m RDF.Triples
query s p o = do
  graph <- get
  return $ RDF.query graph s p o
