{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Base.WrappedPrimitive
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Wrapped versions of the @Primitive@ type from Wumpus-Core.
--
-- This file is essentially /internal/ to Wumpus-Basic.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Base.WrappedPrimitive
  (


  -- * Primitives
    CatPrim
  , prim1
  , cpmap
  , cpmove

  , HPrim
  , hprimToList
  , singleH

  ) where

import Wumpus.Basic.Utils.HList

import Wumpus.Core                      -- package: wumpus-core


import Data.Monoid



-- | A wrapped version of 'Primitive' from Wumpus-Core that 
-- supports Monoid.
-- 
-- Note that CatPrim provides a /single-object/ that can be
-- hyperlinked or whatever. 
--
-- It is different to 'HPrim' which is intended as a list type 
-- with efficient concatenation to support building of multiple
-- Primitives in a frame.
--
-- This type is essentially internal to Wumpus-Basic.
--
data CatPrim = CZero
             | Cat1 Primitive

type instance DUnit CatPrim = Double



instance Monoid CatPrim where
  mempty                  = CZero
  CZero  `mappend` b      = b
  a      `mappend` CZero  = a
  Cat1 a `mappend` Cat1 b = Cat1 $ a `primCat` b

  mconcat []      = mempty
  mconcat (a:as)  = step a as
    where
      step ac []     = ac
      step ac (x:xs) = step (ac `mappend` x) xs




--------------------------------------------------------------------------------

instance Rotate CatPrim where
  rotate _   CZero              = CZero
  rotate ang (Cat1 a)           = Cat1 $ rotate ang a

instance RotateAbout CatPrim where
  rotateAbout _   _  CZero      = CZero
  rotateAbout ang pt (Cat1 a)   = Cat1 $ rotateAbout ang pt a

instance Scale CatPrim where
  scale _  _  CZero             = CZero
  scale sx sy (Cat1 a)          = Cat1 $ scale sx sy a

instance Translate CatPrim where
  translate _  _  CZero         = CZero
  translate dx dy (Cat1 a)      = Cat1 $ translate dx dy a


--------------------------------------------------------------------------------

prim1 :: Primitive -> CatPrim 
prim1 = Cat1


-- | Map 
cpmap :: (Primitive -> Primitive) -> CatPrim -> CatPrim
cpmap _ CZero    = CZero
cpmap f (Cat1 a) = Cat1 $ f a


cpmove :: Vec2 Double -> CatPrim -> CatPrim
cpmove (V2 x y) = translate x y


--------------------------------------------------------------------------------
-- Lists of primitives...


-- | Collected primitives - this type is effectively an analogue
-- to a @Frame@ in Wumpus-Core.
--
-- This type is essentially internal to Wumpus-Basic.
-- 
newtype HPrim u = HPrim { getHPrim :: H Primitive }

-- Note - only a Monoid instance for HPrim - they cannot be 
-- shown, fmapped etc.

instance Monoid (HPrim u) where
  mempty          = HPrim emptyH
  ha `mappend` hb = HPrim $ getHPrim ha `appendH` getHPrim hb

  mconcat []      = mempty
  mconcat (a:as)  = step a as
    where
      step ac []     = ac
      step ac (x:xs) = step (ac `mappend` x) xs


-- | Extract the internal list of 'Primitive' from a 'HPrim'.
-- 
-- The expectation is that this Primitive list will be rendered
-- by Wumpus-Core as a @frame@.
--
hprimToList :: HPrim u -> [Primitive]
hprimToList = toListH . getHPrim


-- | Form a 'HPrim' from a 'CatPrim'.
--
singleH :: CatPrim -> HPrim u
singleH CZero    = HPrim emptyH
singleH (Cat1 a) = HPrim $ wrapH a






