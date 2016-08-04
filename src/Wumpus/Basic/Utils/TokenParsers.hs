{-# LANGUAGE RankNTypes                 #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Utils.TokenParsers
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Generally you should expect to import this module qualified 
-- and define versions to consume trailing white-space.
--
-- > lexDef   :: P.LexemeParser
-- > lexDef   = P.commentLineLexemeParser "Comment" [' ', '\t']
-- >
-- > lexeme   :: CharParser a -> CharParser a
-- > lexeme   = P.lexeme lexDef
-- >
-- > integer  :: CharParser Int
-- > integer  = lexeme P.integer
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Utils.TokenParsers
  (

    LexemeParser
  , spaceLexemeParser
  , spaceCharLexemeParser
  , commentLexemeParser
  , commentLineLexemeParser
  , commentMultiLexemeParser

  , lexeme
  , whiteSpace

  , octBase
  , octHask
  
  , hexBase

  , natural
  , integer
  , double

  ) where

import Wumpus.Basic.Utils.ParserCombinators

import Control.Applicative

-- | Opaque type representing a parser that consumes arbitrary 
-- space.
--
-- Unlike Parsec\'s lexeme parser, this can be customized so that 
-- e.g. newlines are not consumed as white space.
--
newtype LexemeParser = LexemeParser { getLexemeParser :: CharParser () }

-- | Build a lexeme parser that handles /space/.
--
-- /Space/ is zero or more elements matching the @isSpace@
-- predicate from @Data.Char@.
--
spaceLexemeParser :: LexemeParser
spaceLexemeParser = LexemeParser go 
  where
    go = () <$ many space


-- | Build a lexeme parser that handles arbitrary /space/.
--
-- /space/ is parametric, for instance this can manufacture a 
-- lexeme parser that consumes space and tab chars but not 
-- newline.
--
spaceCharLexemeParser :: [Char] -> LexemeParser
spaceCharLexemeParser cs = LexemeParser go 
  where
    go = skipMany (oneOf cs)


-- | Build a lexeme parser that handles start-and-end delimited
-- comments and arbitrary /space/.
--
commentLexemeParser :: String -> (String,String) -> [Char] -> LexemeParser
commentLexemeParser line (start,end) cs = LexemeParser go
  where
    go        = skipMany (whiteChar <|> lineComment line
                                    <|> spanComment start end)
    whiteChar = skipOne (oneOf cs)


-- | Build a lexeme parser that handles line spanning comments 
-- an arbitrary /space/.
--
commentLineLexemeParser :: String -> [Char] -> LexemeParser
commentLineLexemeParser start cs = LexemeParser go
  where
    go        = skipMany (whiteChar <|> lineComment start)
    whiteChar = skipOne (oneOf cs)


-- | Build a lexeme parser that handles start-and-end delimited
-- comments, line comments and arbitrary /space/.
--
commentMultiLexemeParser :: String -> String -> [Char] -> LexemeParser
commentMultiLexemeParser start end cs = LexemeParser go
  where
    go        = skipMany (whiteChar <|> spanComment start end)
    whiteChar = skipOne (oneOf cs)

lineComment :: String -> CharParser ()
lineComment start  = 
    skipOne (string start *> manyTill anyChar endLine)

spanComment :: String -> String -> CharParser ()
spanComment start end = 
    string start *> manyTill anyChar (string end) *> return ()


endLine :: CharParser ()
endLine = skipOne (char '\n') <|> eof


-- | Wrap a CharParser with a lexeme parser, the CharParser will
-- consume trailing space according to the strategy of the 
-- LexemeParser.
--
lexeme :: LexemeParser -> CharParser a -> CharParser a
lexeme trail p = p <* getLexemeParser trail

whiteSpace :: LexemeParser -> CharParser ()
whiteSpace = getLexemeParser

--------------------------------------------------------------------------------




octBase :: CharParser Int
octBase = (\cs -> read $ '0':'o':cs) <$> many1 octDigit

octHask :: CharParser Int
octHask = (string "0o" <|> string "0O") *> octBase


hexBase :: CharParser Int
hexBase = (\xs -> read $ '0':'x':xs) <$> many1 hexDigit



--------------------------------------------------------------------------------

natural :: CharParser Integer
natural = liftA read (many1 digit)


integer :: CharParser Integer
integer = ($) <$> psign <*> natural
  where
    psign = option id (negate <$ char '-')


double :: CharParser Double
double = (\signf intpart fracpart -> signf $ intpart + fracpart)
                  <$> psign <*> onatural <*> ofrac 
  where
    psign     = option id (negate <$ char '-')
    onatural  = option 0  (fromIntegral <$> natural)
    ofrac     = option 0  ((\xs -> read $ '.':xs) <$> (char '.' *> (many1 digit)))
