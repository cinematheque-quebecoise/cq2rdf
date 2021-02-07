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
module Data.RDF.Vocabulary.Wikidata where

import           Import             hiding ((^.))

import qualified Data.RDF.Namespace as RDF

wd :: RDF.Namespace
wd = RDF.mkPrefixedNS' "wd" "http://www.wikidata.org/entity/"

wdt :: RDF.Namespace
wdt = RDF.mkPrefixedNS' "wdt" "http://www.wikidata.org/prop/direct/"

wdQ483394 :: Text
wdQ483394 = RDF.mkUri wd "Q483394"

wdtP136 :: Text
wdtP136 = RDF.mkUri wdt "P136"

wdtP407 :: Text
wdtP407 = RDF.mkUri wdt "P407"
