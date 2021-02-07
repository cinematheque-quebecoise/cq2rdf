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
module Data.RDF.Vocabulary.CRM where

import           Import             hiding ((^.))
import           Namespaces

import qualified Data.RDF.Namespace as RDF

crmE7 :: Text
crmE7 = RDF.mkUri crm "E7_Activity"

crmE21 :: Text
crmE21 = RDF.mkUri crm "E21_Person"

crmE33 :: Text
crmE33 = RDF.mkUri crm "E33_Linguistic_Object"

crmE35 :: Text
crmE35 = RDF.mkUri crm "E35_Title"

crmE40 :: Text
crmE40 = RDF.mkUri crm "E40_Legal_Body"

crmE41 :: Text
crmE41 = RDF.mkUri crm "E41_Appellation"

crmE42 :: Text
crmE42 = RDF.mkUri crm "E42_Identifier"

crmE53 :: Text
crmE53 = RDF.mkUri crm "E53_Place"

crmE54 :: Text
crmE54 = RDF.mkUri crm "E54_Dimension"

crmE55 :: Text
crmE55 = RDF.mkUri crm "E55_Type"

crmE56 :: Text
crmE56 = RDF.mkUri crm "E56_Language"

crmE58 :: Text
crmE58 = RDF.mkUri crm "E58_Measurement_Unit"

crmE98 :: Text
crmE98 = RDF.mkUri crm "E98_Currency"

crmP01 :: Text
crmP01 = RDF.mkUri crm "P01_has_domain"

crmP02 :: Text
crmP02 = RDF.mkUri crm "P02_has_range"

crmP1 :: Text
crmP1 = RDF.mkUri crm "P1_is_identified_by"

crmP2 :: Text
crmP2 = RDF.mkUri crm "P2_has_type"

crmP4 :: Text
crmP4 = RDF.mkUri crm "P4_has_time-span"

crmP4i :: Text
crmP4i = RDF.mkUri crm "P4i_is_time-span_of"

crmP7 :: Text
crmP7 = RDF.mkUri crm "P7_took_place_at"

crmP7i :: Text
crmP7i = RDF.mkUri crm "P7i_witnessed"

crmP9 :: Text
crmP9 = RDF.mkUri crm "P9_consists_of"

crmP9i :: Text
crmP9i = RDF.mkUri crm "P9i_forms_part_of"

crmP14 :: Text
crmP14 = RDF.mkUri crm "P14_carried_out_by"

crmP14_1 :: Text
crmP14_1 = RDF.mkUri crm "P14.1_in_the_role_of"

crmP14i :: Text
crmP14i = RDF.mkUri crm "P14i_performed"

crmP16 :: Text
crmP16 = RDF.mkUri crm "P16_used_specific_object"

crmP43 :: Text
crmP43 = RDF.mkUri crm "P43_has_dimension"

crmP48 :: Text
crmP48 = RDF.mkUri crm "P48_has_preferred_identifier"

crmP67 :: Text
crmP67 = RDF.mkUri crm "P67_refers_to"

crmP72 :: Text
crmP72 = RDF.mkUri crm "P72_has_language"

crmP73 :: Text
crmP73 = RDF.mkUri crm "P73_has_translation"

crmP79 :: Text
crmP79 = RDF.mkUri crm "P79_beginning_is_qualified_by"

crmP80 :: Text
crmP80 = RDF.mkUri crm "P80_end_is_qualified_by"

crmP82a :: Text
crmP82a = RDF.mkUri crm "P82a_begin_of_the_begin"

crmP82b :: Text
crmP82b = RDF.mkUri crm "P82b_end_of_the_end"

crmP90 :: Text
crmP90 = RDF.mkUri crm "P90_has_value"

crmP91 :: Text
crmP91 = RDF.mkUri crm "P91_has_unit"

crmP102 :: Text
crmP102 = RDF.mkUri crm "P102_has_title"

crmP165 :: Text
crmP165 = RDF.mkUri crm "P165_incorporates"

crmP180 :: Text
crmP180 = RDF.mkUri crm "P180_has_currency"

crmP181 :: Text
crmP181 = RDF.mkUri crm "P181_has_amount"

crmP183 :: Text
crmP183 = RDF.mkUri crm "P183_ends_before_the_start_of"

crmP190 :: Text
crmP190 = RDF.mkUri crm "P190_has_symbolic_content"
