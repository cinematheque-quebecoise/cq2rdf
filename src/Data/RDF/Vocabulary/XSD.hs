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
module Data.RDF.Vocabulary.XSD where

import           Import             hiding ((^.))

import qualified Data.RDF.Namespace as RDF

xsdDate :: Text
xsdDate = RDF.mkUri RDF.xsd "date"

xsdDateTime :: Text
xsdDateTime = RDF.mkUri RDF.xsd "dateTime"

xsdDouble :: Text
xsdDouble = RDF.mkUri RDF.xsd "double"

xsdInteger :: Text
xsdInteger = RDF.mkUri RDF.xsd "integer"
