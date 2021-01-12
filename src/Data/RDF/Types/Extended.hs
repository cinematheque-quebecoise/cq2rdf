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

module Data.RDF.Types.Extended
  ( mkTriple
  , mkTripleLit
  , mkNode
  , mkNodeLit
  , subject
  , predicate
  , object
  , bnodeGen
  , getUNode
  )
where

import           Data.RDF
import           Data.Text        (pack, unpack)
import           Import
import           Network.URI      (isURIReference)
import           Text.Parsec      (Parsec)
import qualified Text.Parsec      as P (many, parse, sepBy, try)
import qualified Text.Parsec.Char as P (alphaNum, anyChar, char, noneOf, space,
                                        string)

-- |Get subject of triple
subject :: Triple -> Node
subject (Triple s _ _) = s

-- |Get predicate of triple
predicate :: Triple -> Node
predicate (Triple _ p _) = p

-- |Get object of triple
object :: Triple -> Node
object (Triple _ _ o) = o

getUNode :: Node -> Maybe Text
getUNode (UNode uri) = Just uri
getUNode _ = Nothing

bnodeGen :: Int -> Node
bnodeGen h = bnode $ "_:" <> pack (show $ abs h)

mkTriple :: Text -> Text -> Text -> Maybe Triple
mkTriple subject predicate object = do
  subjectNode   <- mkNode subject
  predicateNode <- mkNode predicate
  objectNode    <- mkNode object
  return $ Triple subjectNode predicateNode objectNode

mkTripleLit :: Text -> Text -> LValue -> Maybe Triple
mkTripleLit subject predicate literal = do
  subjectNode   <- mkNode subject
  predicateNode <- mkNode predicate
  let objectNode = lnode literal
  return $ Triple subjectNode predicateNode objectNode

mkNode :: Text -> Maybe Node
mkNode r = parseUriNode r <|> parseBlankNodeId r

mkNodeLit :: Text -> Maybe Node
mkNodeLit = parsePlainLiteralNode

parseUriNode :: Text -> Maybe Node
parseUriNode uri = if isValidUri uri then Just $ unode uri else Nothing
  where isValidUri = isURIReference . unpack

parseBlankNodeId :: Text -> Maybe Node
parseBlankNodeId blankUri = case P.parse blankNodeParser "" blankUri of
  Left  _ -> Nothing
  Right v -> Just $ bnode $ pack v

blankNodeParser :: Parsec Text () String
blankNodeParser = P.string "_:" *> P.many P.alphaNum

parsePlainLiteralNode :: Text -> Maybe Node
parsePlainLiteralNode l = case P.parse literalValuePlainParser "" l of
  Left  _ -> Nothing
  Right v -> Just $ lnode v

parseLangLiteralNode :: Text -> Maybe Node
parseLangLiteralNode l = case P.parse literalValuePlainLangParser "" l of
  Left  _ -> Nothing
  Right v -> Just $ lnode v

literalValueParser :: Parsec Text () LValue
literalValueParser =
  P.try literalValuePlainLangParser
    <|> P.try literalValueTypedParser
    <|> literalValuePlainParser

literalValuePlainParser :: Parsec Text () LValue
literalValuePlainParser = PlainL . pack <$> labelParser
 where
  labelParser = unwords <$> P.sepBy wordParser P.space
  wordParser  = P.many P.anyChar

literalValuePlainLangParser :: Parsec Text () LValue
literalValuePlainLangParser = do
  literal <- labelParser
  _       <- P.char '@'
  lang    <- P.many P.alphaNum
  return $ PlainLL (pack literal) (pack lang)

literalValueTypedParser :: Parsec Text () LValue
literalValueTypedParser = do
  literal <- labelParser
  _       <- P.string "^^"
  xsdType <- P.many P.anyChar
  return $ TypedL (pack literal) (pack xsdType)

labelParser :: Parsec Text () String
labelParser = unwords <$> P.sepBy wordParser P.space
  where wordParser = P.many $ P.noneOf "@"
  -- where wordParser = P.many $ P.noneOf "@^"
