{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Utils.HList
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Hughes list, ...
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Utils.HList
  (

  -- * Hughes list
    H
  , emptyH
  , wrapH
  , consH
  , snocH
  , appendH
  , unfoldrH
  , veloH
  , concatH

  , toListH
  , prefixListH
  , fromListH


  ) where



--------------------------------------------------------------------------------
-- Hughes list

type H a = [a] -> [a]

emptyH :: H a
emptyH = id

wrapH :: a -> H a
wrapH a = consH a id 

consH :: a -> H a -> H a
consH a f = (a:) . f

snocH :: H a -> a -> H a
snocH  f a = f . (a:)

appendH :: H a -> H a -> H a
appendH f g = f . g



unfoldrH :: (b -> Maybe (a,b)) -> b -> H a
unfoldrH phi = step
  where step b = case phi b of
                  Nothing -> emptyH
                  Just (a,s) -> a `consH` step s


-- | velo consumes the list as per map, but builds it back
-- as a Hughes list - so items can be dropped
-- replaced, repeated, etc...
-- 
veloH :: (a -> H b) -> [a] -> H b
veloH f = foldr step id 
  where step a hf = f a . hf

concatH :: [H a] -> H a
concatH = foldr (.) id


toListH :: H a -> [a]
toListH = ($ [])

prefixListH :: H a -> [a] -> [a]
prefixListH hf xs = hf $ xs

fromListH :: [a] -> H a
fromListH xs = (xs++)


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

