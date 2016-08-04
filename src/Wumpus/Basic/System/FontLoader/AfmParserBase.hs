{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.System.FontLoader.AfmParserBase
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Common parsers for AFM files.
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.System.FontLoader.AfmParserBase
  ( 

    afmFileParser
  , runQuery
  , textQuery

  , getFontBBox
  , getEncodingScheme
  , getCapHeight
  
  , charBBox
  , metric
  , keyStringPair
  , versionNumber  
  , startCharMetrics

  , keyName
  , newlineOrEOF
  , name
  , name1
  , semi
  , uptoNewline
  , number
  , cint
  , hexInt
  , octInt

  , lexeme
  , symbol
  , integer
  , int
  , double

  ) where

import Wumpus.Basic.System.FontLoader.Datatypes

import Wumpus.Basic.Utils.ParserCombinators
import qualified Wumpus.Basic.Utils.TokenParsers as P

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative

import Data.Char
import qualified Data.Map               as Map


afmFileParser :: CharParser AfmGlyphMetrics -> CharParser AfmFile
afmFileParser pgm = do 
    info <- (versionNumber    *> globalInfo) 
    cms  <- (startCharMetrics *> many pgm)
    return $ AfmFile 
              { afm_encoding            = getEncodingScheme info
              , afm_letter_bbox         = getFontBBox       info
              , afm_cap_height          = getCapHeight      info
              , afm_descender           = getDescender      info
              , afm_underline_position  = getUlPosition     info
              , afm_underline_thickness = getUlThickness    info
              , afm_glyph_metrics       = cms
              }

globalInfo :: CharParser GlobalInfo
globalInfo = (foldr (\(k,v) a -> Map.insert k v a) Map.empty) 
               <$> manyTill keyStringPair (peek startCharMetrics)


 
runQuery :: String -> CharParser a -> GlobalInfo -> Maybe a
runQuery field_name p table = 
    Map.lookup field_name table >>= extr . runParser p
  where
    extr (Okay a _) = Just a
    extr _          = Nothing

textQuery :: String -> GlobalInfo -> Maybe String
textQuery = Map.lookup


-- | Strictly speaking a fontBBox is measured in integer units.
--
getFontBBox            :: GlobalInfo -> Maybe AfmBoundingBox
getFontBBox            = runQuery "FontBBox" fontBBox

getEncodingScheme      :: GlobalInfo -> Maybe String
getEncodingScheme      = textQuery "EncodingScheme"

getCapHeight           :: GlobalInfo -> Maybe AfmUnit
getCapHeight           = runQuery "CapHeight" number

getDescender           :: GlobalInfo -> Maybe AfmUnit
getDescender           = runQuery "Descender" number


-- | Expected to be negative...
--
getUlPosition          :: GlobalInfo -> Maybe AfmUnit
getUlPosition          = runQuery "UnderlinePosition" number

getUlThickness         :: GlobalInfo -> Maybe AfmUnit
getUlThickness         = runQuery "UnderlineThickness" number


charBBox :: CharParser AfmBoundingBox
charBBox = symbol "B" *> fontBBox <* semi

fontBBox :: CharParser AfmBoundingBox
fontBBox = (\llx lly urx ury -> boundingBox (P2 llx lly) (P2 urx ury))
              <$> number <*> number <*> number <*> number



metric :: String -> a -> CharParser a -> CharParser a
metric iden dfault p = option dfault go
  where
    go = symbol iden *> p <* semi



keyStringPair :: CharParser (AfmKey,String)
keyStringPair = (,) <$> keyName <*> uptoNewline <* newlineOrEOF 
             <?> "key-value line"

versionNumber :: CharParser String
versionNumber = 
    symbol "StartFontMetrics" *> many1 (digit <|> char '.') <* newlineOrEOF
      <?> "StartFontMetrics"


startCharMetrics :: CharParser Int
startCharMetrics = symbol "StartCharMetrics" *> int <* newlineOrEOF
                <?> "StartCharMetrics failed"



--------------------------------------------------------------------------------


keyName :: CharParser AfmKey
keyName = lexeme (many1 $ satisfy isAlphaNum) 


newlineOrEOF :: CharParser ()
newlineOrEOF = skipOne (lexeme newline) <|> eof


uptoNewline :: CharParser String
uptoNewline = many1 (noneOf ['\n'])


name :: CharParser String
name = lexeme $ many (noneOf ";\n")

name1 :: CharParser String
name1 = lexeme $ many (noneOf "; \t\n")



semi :: CharParser Char
semi = lexeme $ char ';'




number :: CharParser AfmUnit
number = liftA realToFrac double


cint :: CharParser Int
cint = hexInt <|> octInt <|> int


hexInt :: CharParser Int
hexInt = lexeme $ between (char '<') (char '>') P.hexBase


octInt :: CharParser Int
octInt = lexeme $ char '\\' *> P.octBase



--------------------------------------------------------------------------------

-- no newline in whitespace


lp :: P.LexemeParser
lp = P.commentLineLexemeParser "Comment" [' ', '\t']


lexeme          :: CharParser a -> CharParser a
lexeme          = P.lexeme lp

symbol          :: String -> CharParser String
symbol          = lexeme . string

-- whiteSpace      :: CharParser ()
-- whiteSpace      = P.whiteSpace lp


integer         :: CharParser Integer
integer         = lexeme P.integer

int             :: CharParser Int
int             = fromIntegral <$> integer

double          :: CharParser Double
double          = lexeme P.double
