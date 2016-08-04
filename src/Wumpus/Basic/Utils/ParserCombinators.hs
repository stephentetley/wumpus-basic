{-# LANGUAGE RankNTypes                 #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Utils.ParserCombinators
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Two continuation parser combinators.
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Utils.ParserCombinators
  (
    Parser
  , Result(..)
  , CharParser
  , CharResult
  , ParseError

  , runParser
  , runParserEither
  , apply
  , failure
  , throwError
  , (<?>)
  , lookahead
  , peek
  , eof
  , equals
  , satisfy
  , oneOf
  , noneOf

  , chainl1
  , chainr1
  , chainl
  , chainr
  , choice
  , count
  , between
  , option
  , optionMaybe
  , optionUnit
  , skipOne
  , skipMany
  , skipMany1
  , many1
  , sepBy
  , sepBy1
  , sepEndBy
  , sepEndBy1
  , manyTill  
  , manyTill1

  -- * Char parsers
  , char
  , string
  , anyChar
  , upper
  , lower
  , letter
  , alphaNum
  , digit
  , hexDigit
  , octDigit
  , newline
  , tab
  , space



  ) where

import Control.Applicative
import Control.Monad
import Data.Char


data Result s ans = Fail String [s] | Okay ans [s]
  deriving (Eq,Ord,Show)


type SK s r ans = r -> FK s ans -> [s] -> Result s ans
type FK s ans   = Result s ans


newtype Parser s r = Parser { 
          getParser :: forall ans. SK s r ans -> FK s ans -> [s] -> Result s ans }


type CharParser a = Parser Char a
type CharResult a = Result Char a
type ParseError   = String

runParser :: Parser s a -> [s] -> Result s a
runParser p = getParser p skZero fkZero
  where
    skZero = \ans _ ss -> Okay ans ss
    fkZero = Fail "" []


runParserEither :: Show s => Parser s a -> [s] -> Either ParseError a
runParserEither p = post . runParser p
  where    
    post (Okay a _)    = Right a
    post (Fail err []) = Left $ err ++ "\nUnexpected EOF"
    post (Fail err ss) = Left $ err ++ "\n" ++ (take 20 $ show ss)


-- @return@ of Monad, @pure@ of Applicative
--
yield  :: a -> Parser s a
yield a = Parser $ \sk fk ss -> sk a fk ss

-- mplus of MonadPlus, (<|>) of Applicative.
--
alt :: Parser s a -> Parser s a -> Parser s a
alt p1 p2 = Parser $ \sk fk ss -> getParser p1 sk (getParser p2 sk fk ss) ss

infixl 5 `apply`

apply :: Functor f => f a -> (a -> b) -> f b
apply = flip fmap


failure :: Parser s a
failure = Parser $ \_ fk _ -> fk


-- eager-many / zero-or-more (many of Applicative)
--
eagerMany :: Parser s a -> Parser s [a]
eagerMany p = (p >>= \r -> eagerMany p `apply` \rs -> (r:rs)) <|> return [] 

-- eager-some / one-or-more (some of Applicative)
--
eagerSome :: Parser s a -> Parser s [a]
eagerSome p = p >>= \r -> eagerMany p `apply` \rs -> (r:rs)


instance Functor (Parser s) where
  fmap f mf = Parser $ \sk -> getParser mf $ \a -> (sk . f) a
     


instance Applicative (Parser s) where
  pure = yield
  (<*>) = ap

instance Alternative (Parser s) where
  empty = failure
  (<|>) = alt
  many  = eagerMany
  some  = eagerSome
 
instance Monad (Parser s) where
  return  = yield
  m >>= k = Parser $ \sk -> getParser m $ \a -> getParser (k a) sk

instance MonadPlus (Parser s) where
  mzero = failure
  mplus = alt



--------------------------------------------------------------------------------
-- Combinators


throwError :: String -> Parser s a
throwError err_msg = Parser $ \_ _ ss -> Fail err_msg ss


infixr 0 <?>

(<?>) :: Parser s a -> String -> Parser s a
p <?> err_msg = Parser $ \sk fk ss -> getParser p sk (swapMsg fk) ss
  where
    swapMsg (Fail _ ss) = Fail err_msg ss
    swapMsg okay        = okay

-- | This one is from Chris Okasaki\'s \"Even Higher-Order 
-- Functions for Parsing\".
--
lookahead :: Parser s a -> (a -> Parser s b) -> Parser s b 
lookahead p mf  = Parser $ \sk fk -> 
    getParser p (\a fk2 -> getParser (mf a) sk fk2) fk


-- | Peek tries the supplied parse, but does not consume input 
-- \*\* even when the parse succeeds \*\*.
--
peek :: Parser s a -> Parser s a
peek p = Parser $ \sk fk ss -> 
    getParser p (\a fk2 _ -> sk a fk2 ss) fk ss


eof :: Parser s ()
eof = Parser go
  where
    go sk fk [] = sk () fk []
    go _  fk _  = fk

equals :: Eq s => s -> Parser s s
equals sym = Parser go
  where
    go sk fk (s:ss) | s == sym = sk s fk ss
    go _  fk _                 = fk


satisfy :: (s -> Bool) -> Parser s s
satisfy test = Parser go
  where
    go sk fk (s:ss) | test s = sk s fk ss 
    go _  fk _               = fk


oneOf           :: Eq s => [s] -> Parser s s
oneOf cs        = satisfy (`elem` cs)

noneOf          :: Eq s => [s] -> Parser s s
noneOf cs       = satisfy (`notElem` cs)



-- Note - the type sigs of the chain parsers can be generalized 
-- to any MonadPlus.
--

chainl1 :: MonadPlus m => m a -> m (a -> a -> a) -> m a
chainl1 p op = p >>= rest 
  where 
    rest x = mplus (op >>= \f -> p >>= \a -> rest (f x a)) (return x) 
               

chainr1 :: MonadPlus m => m a -> m (a -> a -> a) -> m a
chainr1 p op = scan 
   where 
     scan   = p >>= rest 
     rest x = mplus (op >>= \f -> scan >>= \a -> rest (f x a)) (return x) 

chainl :: MonadPlus m => m a -> m (a -> a -> a) -> a -> m a
chainl p op v = mplus (chainl1 p op) (return v)

chainr :: MonadPlus m => m a -> m (a -> a -> a) -> a -> m a
chainr p op v = mplus (chainr1 p op) (return v)


infixr 5 <:> 
 
-- | Applicative cons.
--
(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) p1 p2 = (:) <$> p1 <*> p2


choice :: Alternative f => [f a] -> f a
choice = foldr (<|>) empty 
   
count :: Applicative f => Int -> f a -> f [a]
count i p | i <= 0    = pure []
          | otherwise = p <:> count (i-1) p 
          
between :: Applicative f => f open -> f close -> f a -> f a
between o c a = o *> a <* c

          
option :: Alternative f => a -> f a -> f a
option x p          = p <|> pure x

optionMaybe :: Alternative f => f a -> f (Maybe a)
optionMaybe = optional

-- aka Parsecs /optional/
optionUnit :: Alternative f => f a -> f ()
optionUnit p = () <$ p <|> pure ()

skipOne :: Applicative f => f a -> f ()
skipOne p = p *> pure ()

skipMany :: Alternative f => f a -> f ()
skipMany p = many_p
  where 
    many_p = some_p <|> pure ()
    some_p = p       *> many_p

skipMany1 :: Alternative f => f a -> f ()
skipMany1 p = p *> skipMany p

-- | 'many1' an alias for Control.Applicative 'some'. 
--
many1 :: Alternative f => f a -> f [a]
many1 = some

sepBy :: Alternative f => f a -> f b -> f [a]
sepBy p sep = sepBy1 p sep <|> pure []

sepBy1 :: Alternative f => f a -> f b -> f [a]
sepBy1 p sep = p <:> step 
  where 
    step = (sep *> p) <:> step <|> pure []

sepEndBy :: Alternative f => f a -> f b -> f [a]
sepEndBy p sep = sepEndBy1 p sep <|> pure []

sepEndBy1 :: Alternative f => f a -> f b -> f [a]
sepEndBy1 p sep = (p <* sep) <:> step 
  where
    step = (p <* sep) <:> step <|> pure []
    
manyTill :: Alternative f => f a -> f b -> f [a]
manyTill p end = step <|> pure [] 
  where
    step  = p <:> (final <|> step)
    final = [] <$ end 

manyTill1 :: Alternative f => f a -> f b -> f [a]
manyTill1 p end = p <:> step 
  where
    step  = final <|> (p <:> step)
    final = [] <$ end


--------------------------------------------------------------------------------
-- Char parsers


char :: Char -> CharParser Char
char ch = satisfy (==ch)

string :: String -> CharParser String
string ss = mapM char ss

anyChar :: CharParser Char
anyChar = Parser go
  where
   go sk fk (s:ss) = sk s fk ss
   go _  fk _      = fk


upper       :: CharParser Char
upper       = satisfy isUpper

lower       :: CharParser Char
lower       = satisfy isLower

letter      :: CharParser Char
letter      = satisfy isAlpha

alphaNum    :: CharParser Char
alphaNum    = satisfy isAlphaNum

digit       :: CharParser Char
digit       = satisfy isDigit 

hexDigit    :: CharParser Char
hexDigit    = satisfy isHexDigit

octDigit    :: CharParser Char
octDigit    = satisfy isOctDigit

newline     :: CharParser Char
newline     = equals '\n'

tab         :: CharParser Char
tab         = equals '\t'

space       :: CharParser Char
space       = satisfy isSpace

