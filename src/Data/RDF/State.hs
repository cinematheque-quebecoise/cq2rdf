{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.RDF.State where

import Import
import Data.RDF (RDF)
import qualified Data.RDF as RDF
import Control.Monad.State

-- newtype RdfState rdfImpl m a = RdfState { unRdfState :: StateT (RDF rdfImpl) m a }
--   deriving (Functor, Applicative, Monad, MonadState (RDF rdfImpl))
type RdfState rdfImpl m a = StateT (RDF rdfImpl) m a

mkRdf :: (RDF.Rdf rdfImpl, Monad m)
      => RDF.Triples
      -> Maybe RDF.BaseUrl
      -> RDF.PrefixMappings
      -> RdfState rdfImpl m ()
mkRdf triples baseUrlMaybe mappings =
  put $ RDF.mkRdf triples baseUrlMaybe mappings

addTriple :: (RDF.Rdf rdfImpl, Monad m)
          => RDF.Triple
          -> RdfState rdfImpl m ()
addTriple triple = do
  graph <- get
  put $ RDF.addTriple graph triple

removeTriple :: (RDF.Rdf rdfImpl, Monad m)
             => RDF.Triple
             -> RdfState rdfImpl m ()
removeTriple triple = do
  graph <- get
  put $ RDF.removeTriple graph triple

addPrefixMappings :: (RDF.Rdf rdfImpl, Monad m)
                  => RDF.PrefixMappings
                  -> Bool
                  -> RdfState rdfImpl m ()
addPrefixMappings mappings replaceOldMapping = do
  graph <- get
  put $ RDF.addPrefixMappings graph mappings replaceOldMapping

empty :: (RDF.Rdf rdfImpl, Monad m)
      => RdfState rdfImpl m ()
empty = put $ RDF.empty

