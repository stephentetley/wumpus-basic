{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.System.FontLoader.AfmV4Dot1Parser
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- AFM file parser for Version 4.1.
--
-- Adobe distributes font metrics for the /Core 14/ fonts as
-- AFM Version 4.1 files.  
--
--------------------------------------------------------------------------------

module Wumpus.Basic.System.FontLoader.AfmV4Dot1Parser
  ( 
    
    afmV4Dot1Parser  
  ) where

import Wumpus.Basic.System.FontLoader.AfmParserBase
import Wumpus.Basic.System.FontLoader.Datatypes
import Wumpus.Basic.Utils.ParserCombinators

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative


afmV4Dot1Parser :: CharParser AfmFile
afmV4Dot1Parser = afmFileParser charMetricsV4Dot1


charMetricsV4Dot1 :: CharParser AfmGlyphMetrics
charMetricsV4Dot1 = AfmGlyphMetrics <$>
        characterCode
    <*> widthVector
    <*> metric "N" "" name1
    <*  charBBox
    <*  many (symbol "L" *> ligature_body <* semi)
    <*  newlineOrEOF
  where
    ligature_body = ((,) <$> name <*> name)


-- Note - there are many variants for width vectors in in 4.1.
-- Wumpus needs some thought about what to do for them, it also
-- needs some facility to tell how successful the parse has been.
    
widthVector :: CharParser (Vec2 AfmUnit)
widthVector =  (symbol "WX" *> ((\w -> vec w 0) <$> number) <* semi)
           <|> (symbol "W"  *> (vec <$> number <*> number)  <* semi)


-- V4.1 allows C int or CH \<hex\>
--
characterCode :: CharParser Int
characterCode = metric "CH" (-1) hexInt <|> metric "C" (-1) cint  