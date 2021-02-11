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
module Data.RDF.Vocabulary.QUDT where

import           Import             hiding ((^.))

import           Data.RDF.Namespace (Namespace)
import qualified Data.RDF.Namespace as RDF

unit :: Namespace
unit = RDF.mkPrefixedNS' "unit" "http://qudt.org/vocab/unit/"

unitSEC :: Text
unitSEC = RDF.mkUri unit "SEC"

unitCAD :: Text
unitCAD = RDF.mkUri unit "CAD"
