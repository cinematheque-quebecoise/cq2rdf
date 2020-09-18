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
module Data.RDF.Types.ExtendedSpec
  ( spec
  )
where

import           Data.Maybe              (fromJust)
import           Data.RDF                (LValue (..), Triple (..), bnode,
                                          lnode, unode)
import           Data.RDF.Types.Extended (mkTriple, mkTripleLit)
import           Import
import           Test.Hspec

spec :: Spec
spec = do
  describe "mkTriple" $ do
    it "should create triple from SPO URIs" $ do
      let maybeTriple = mkTriple
            "http://example.com/vocab#Concept123"
            "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
            "http://example.com/vocab#Concept"

      maybeTriple `shouldNotBe` Nothing

      let (Triple subject predicate object) = fromJust maybeTriple

      subject `shouldBe` unode "http://example.com/vocab#Concept123"
      predicate
        `shouldBe` unode "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
      object `shouldBe` unode "http://example.com/vocab#Concept"

    it "should detect invalid URIs in SPO" $ do
      let maybeTriple1 = mkTriple
            "invalid subject URI"
            "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
            "http://example.com/vocab#Concept"

      maybeTriple1 `shouldBe` Nothing

      let maybeTriple2 = mkTriple "http://example.com/vocab#Concept123"
                                  "invalid predicate URI"
                                  "http://example.com/vocab#Concept"

      maybeTriple2 `shouldBe` Nothing

      let maybeTriple3 = mkTriple
            "http://example.com/vocab#Concept123"
            "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
            "invalid subject URI"

      maybeTriple3 `shouldBe` Nothing

    it "should accept relative URIs in SPO" $ do
      let maybeTriple = mkTriple "/resource/Concept123" "#type" "Concept"

      maybeTriple `shouldNotBe` Nothing

      let (Triple subject predicate object) = fromJust maybeTriple

      subject `shouldBe` unode "/resource/Concept123"
      predicate `shouldBe` unode "#type"
      object `shouldBe` unode "Concept"

    it "should accept blank URIs in SPO" $ do
      let maybeTriple = mkTriple "_:b1" "_:b2" "_:b3"

      maybeTriple `shouldNotBe` Nothing

      let (Triple subject predicate object) = fromJust maybeTriple

      subject `shouldBe` bnode "b1"
      predicate `shouldBe` bnode "b2"
      object `shouldBe` bnode "b3"

  describe "mkTripleLit" $ do
    it "should treat object in SPO as a literal" $ do
      let maybeTriple = mkTripleLit
            "http://example.com/vocab#Concept123"
            "http://www.w3.org/2000/01/rdf-schema#label"
            "Concept123"

      maybeTriple `shouldNotBe` Nothing

      let (Triple subject predicate object) = fromJust maybeTriple

      subject `shouldBe` unode "http://example.com/vocab#Concept123"
      predicate `shouldBe` unode "http://www.w3.org/2000/01/rdf-schema#label"
      object `shouldBe` lnode (PlainL "Concept123")

    it "should accept multi word literal in object" $ do
      let maybeTriple = mkTripleLit
            "http://example.com/vocab#Concept123"
            "http://www.w3.org/2000/01/rdf-schema#label"
            "Concept 123"

      maybeTriple `shouldNotBe` Nothing

      let (Triple subject predicate object) = fromJust maybeTriple

      subject `shouldBe` unode "http://example.com/vocab#Concept123"
      predicate `shouldBe` unode "http://www.w3.org/2000/01/rdf-schema#label"
      object `shouldBe` lnode (PlainL "Concept 123")

    it "should accept words with any characters expect @ and ^" $ do
      let maybeTriple = mkTripleLit
            "http://example.com/vocab#Concept123"
            "http://www.w3.org/2000/01/rdf-schema#label"
            "Concept's 123"

      maybeTriple `shouldNotBe` Nothing

      let (Triple subject predicate object) = fromJust maybeTriple

      subject `shouldBe` unode "http://example.com/vocab#Concept123"
      predicate `shouldBe` unode "http://www.w3.org/2000/01/rdf-schema#label"
      object `shouldBe` lnode (PlainL "Concept's 123")

    it "should parse language in literal" $ do
      let maybeTriple = mkTripleLit
            "http://example.com/vocab#Concept123"
            "http://www.w3.org/2000/01/rdf-schema#label"
            "Concept123@fr"

      maybeTriple `shouldNotBe` Nothing

      let (Triple subject predicate object) = fromJust maybeTriple

      subject `shouldBe` unode "http://example.com/vocab#Concept123"
      predicate `shouldBe` unode "http://www.w3.org/2000/01/rdf-schema#label"
      object `shouldBe` lnode (PlainLL "Concept123" "fr")

    it "should parse type in literal" $ do
      let maybeTriple = mkTripleLit
            "http://example.com/vocab#Concept123"
            "http://www.w3.org/2000/01/rdf-schema#label"
            "Concept123^^xsd:string"

      maybeTriple `shouldNotBe` Nothing

      let (Triple subject predicate object) = fromJust maybeTriple

      subject `shouldBe` unode "http://example.com/vocab#Concept123"
      predicate `shouldBe` unode "http://www.w3.org/2000/01/rdf-schema#label"
      object `shouldBe` lnode (TypedL "Concept123" "xsd:string")
