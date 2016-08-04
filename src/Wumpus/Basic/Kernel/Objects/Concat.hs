{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.Concat
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Classes for concatenation.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.Concat
  (

    ZConcat(..)
  , cat
  , Concat(..)
  , hcat
  , vcat

  , CatSpace(..)
  , hsep
  , vsep

  , Align(..)
  , alignRow
  , alignColumn

  , AlignSpace(..)
  , alignRowSep
  , alignColumnSep
  
  
  ) where

import Wumpus.Basic.Kernel.Base.BaseDefs

import Wumpus.Core                              -- package: wumpus-core

import Data.Monoid

infixr 6 `superior`, `anterior`

-- | Minimal defintion is 'superior', 'anterior' is usually 
-- @flip superior@.
--
-- > `superior` (infixr 6)
--
-- > `anterior` (infixr 6)
-- 
-- 
class ZConcat o where
  anterior :: o -> o -> o 
  superior :: o -> o -> o

  anterior = flip superior


cat :: (Monoid o, ZConcat o) => [o] -> o
cat [] = mempty
cat (x:xs) = go x xs
  where
    go acc []     = acc
    go acc (a:as) = go (acc `superior` a) as
  


infixr 6 `hconcat`
infixr 5 `vconcat`


-- | Concatenation with /movement/ - the second object is moved
-- next to the first.
--
-- > hconcat is equivalent to @(<>)@ in WL-PPrint.
-- > (infixr 6)
-- 
-- > vconcat is equivalent to @(<$>)@ in WL_PPrint.
-- > (infixr 5)
--
class Concat o where
  hconcat :: o -> o -> o
  vconcat :: o -> o -> o

-- | Horizontally concatenate a list of objects.
-- 
-- Note - the first argument is an /alternative/ - this is drawn 
-- if the list is empty, otherwise it is not drawn.
--
hcat :: (Monoid o, Concat o) => [o] -> o
hcat []     = mempty
hcat (x:xs) = go x xs
  where
    go acc []     = acc
    go acc (a:as) = go (acc `hconcat` a) as
  

-- | Vertically concatenate a list of objects.
-- 
-- Note - the first argument is an /alternative/ - this is drawn 
-- if the list is empty, otherwise it is not drawn.
--
vcat :: (Monoid o, Concat o) => [o] -> o
vcat []     = mempty
vcat (x:xs) = go x xs
  where
    go acc []     = acc
    go acc (a:as) = go (acc `vconcat` a) as
  

class CatSpace o where
   hspace :: u ~ DUnit o => u -> o -> o -> o
   vspace :: u ~ DUnit o => u -> o -> o -> o

hsep :: (Monoid o, CatSpace o, u ~ DUnit o) => u -> [o] -> o
hsep _  []     = mempty
hsep dx (x:xs) = go x xs
  where
    op            = hspace dx
    go acc []     = acc
    go acc (a:as) = go (acc `op` a) as
  

vsep :: (Monoid o, CatSpace o, u ~ DUnit o) => u -> [o] -> o
vsep _  []     = mempty
vsep dx (x:xs) = go x xs
  where
    op            = vspace dx
    go acc []     = acc
    go acc (a:as) = go (acc `op` a) as


class Align o where
  halign :: HAlign -> o -> o -> o 
  valign :: VAlign -> o -> o -> o


alignRow :: (Monoid o, Align o) => HAlign -> [o] -> o
alignRow _ []     = mempty
alignRow ha (x:xs) = go x xs
  where
    op            = halign ha 
    go acc []     = acc
    go acc (a:as) = go (acc `op` a) as


alignColumn :: (Monoid o, Align o) => VAlign -> [o] -> o
alignColumn _ []     = mempty
alignColumn va (x:xs) = go x xs
  where
    op            = valign va 
    go acc []     = acc
    go acc (a:as) = go (acc `op` a) as



class AlignSpace o where
  halignSpace :: u ~ DUnit o => HAlign -> u -> o -> o -> o 
  valignSpace :: u ~ DUnit o => VAlign -> u -> o -> o -> o



alignRowSep :: (Monoid o, AlignSpace o, u ~ DUnit o) 
            => HAlign -> u -> [o] -> o
alignRowSep _  _  []     = mempty
alignRowSep ha dx (x:xs) = go x xs
  where
    op            = halignSpace ha dx
    go acc []     = acc
    go acc (a:as) = go (acc `op` a) as


alignColumnSep :: (Monoid o, AlignSpace o, u ~ DUnit o) 
            => VAlign -> u -> [o] -> o
alignColumnSep _  _  []     = mempty
alignColumnSep va dx (x:xs) = go x xs
  where
    op            = valignSpace va dx
    go acc []     = acc
    go acc (a:as) = go (acc `op` a) as


