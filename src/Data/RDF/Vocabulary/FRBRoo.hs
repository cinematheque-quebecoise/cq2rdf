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
module Data.RDF.Vocabulary.FRBRoo where

import           Import             hiding ((^.))
import           Namespaces

import qualified Data.RDF.Namespace as RDF

frbrooF1 :: Text
frbrooF1 = RDF.mkUri frbroo "F1_Work"

frbrooF3 :: Text
frbrooF3 = RDF.mkUri frbroo "F3_Manifestation_Product_Type"

frbrooF21 :: Text
frbrooF21 = RDF.mkUri frbroo "F21_Recording_Work"

frbrooF24 :: Text
frbrooF24 = RDF.mkUri frbroo "F24_Publication_Expression"

frbrooF26 :: Text
frbrooF26 = RDF.mkUri frbroo "F26_Recording"

frbrooF29 :: Text
frbrooF29 = RDF.mkUri frbroo "F29_Recording_Event"

frbrooF30 :: Text
frbrooF30 = RDF.mkUri frbroo "F30_Publicatio_Event"

frbrooR2 :: Text
frbrooR2 = RDF.mkUri frbroo "R2_is_derivative_of"

frbrooR2i :: Text
frbrooR2i = RDF.mkUri frbroo "R2i_has_derivative"

frbrooR16 :: Text
frbrooR16 = RDF.mkUri frbroo "R16_initiated"

frbrooR16i :: Text
frbrooR16i = RDF.mkUri frbroo "R16i_was_initiated_by"

frbrooR21 :: Text
frbrooR21 = RDF.mkUri frbroo "R21_created"

frbrooR22 :: Text
frbrooR22 = RDF.mkUri frbroo "R22_created_a_realization_of"

frbrooR22i :: Text
frbrooR22i = RDF.mkUri frbroo "R22i_was_realised_through"

frbrooR24 :: Text
frbrooR24 = RDF.mkUri frbroo "R24_created"

frbrooCLR6 :: Text
frbrooCLR6 = RDF.mkUri frbroo "CLR6_should_carry"
