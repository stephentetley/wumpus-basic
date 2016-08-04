{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.AdvObject
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Extended Graphic object - an AdvanceGraphic is a Graphic 
-- twinned with and advance vector.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.AdvObject
  (


  -- * Advance vector
    AdvanceVec
  , advanceH
  , advanceV


  -- * Advance-vector object and graphic
  , AdvObject
  , DAdvObject

  , AdvGraphic
  , DAdvGraphic
  
  , runAdvObject

  , makeAdvObject
  , emptyAdvObject
  , blankAdvObject
  

  -- * Composition
  , advance
  , advances
  , advspace
  , evenspace

  , advrepeat
  , punctuate
  , advfill

  ) where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Base.WrappedPrimitive
import Wumpus.Basic.Kernel.Objects.Basis
import Wumpus.Basic.Kernel.Objects.Image
import Wumpus.Basic.Kernel.Objects.LocImage

import Wumpus.Core                              -- package: wumpus-core

import Data.VectorSpace                         -- package: vector-space





--------------------------------------------------------------------------------

-- | Advance vectors provide an idiom for drawing consecutive
-- graphics. PostScript uses them to draw left-to-right text - 
-- each character has an advance vector for the width and 
-- as characters are drawn they successively displace the start
-- point for the next character with their advance vector.
--
-- Type alias for Vec2.
--
type AdvanceVec u = Vec2 u


-- | Extract the horizontal component of an advance vector.
--
-- For left-to-right latin text, the vertical component of an
-- advance vector is expected to be 0. Ingoring it seems 
-- permissible when drawing text.
--
advanceH :: AdvanceVec u -> u
advanceH (V2 w _)  = w

-- | Extract the verticall component of an advance vector.
--
advanceV :: AdvanceVec u -> u
advanceV (V2 _ h)  = h


--------------------------------------------------------------------------------
-- AdvObject

-- | Internal newtype wrapper so we can have a monoid instance 
-- with vector plus (^+^) for mappend.
--
newtype DAV = DAV { getDAV :: AdvanceVec Double }


instance Monoid DAV where
  mempty = DAV $ V2 0 0
  DAV v1 `mappend` DAV v2 = DAV $ v1 ^+^ v2

-- | /Advance vector/ graphic - this partially models the 
-- PostScript @show@ command which moves the /current point/ by the
-- advance (width) vector as each character is drawn.
--
newtype AdvObject u a = AdvObject 
          { getAdvObject :: DrawingContext -> DPoint2 -> (a, DAV, CatPrim) }

type instance DUnit (AdvObject u a) = u

type DAdvObject a   = AdvObject Double a


type AdvGraphic u = AdvObject u (UNil u)
type DAdvGraphic  = AdvGraphic Double


instance Functor (AdvObject u) where
  fmap f mf = AdvObject $ \ctx pt -> 
              let (a,v1,w1) = getAdvObject mf ctx pt in (f a,v1,w1)


instance Applicative (AdvObject u) where
  pure a    = AdvObject $ \_   _  -> (a,mempty,mempty)
  mf <*> ma = AdvObject $ \ctx pt -> 
              let (f,v1,w1) = getAdvObject mf ctx pt
                  (a,v2,w2) = getAdvObject ma ctx pt
              in (f a, v1 `mappend` v2, w1 `mappend` w2)



instance Monad (AdvObject u) where
  return a  = AdvObject $ \_   _  -> (a,mempty,mempty)
  mf >>= k  = AdvObject $ \ctx pt -> 
              let (a,v1,w1) = getAdvObject mf ctx pt
                  (b,v2,w2) = getAdvObject (k a) ctx pt
              in (b, v1 `mappend` v2, w1 `mappend` w2)



instance DrawingCtxM (AdvObject u) where
  askDC           = AdvObject $ \ctx _ -> (ctx, mempty, mempty)
  asksDC fn       = AdvObject $ \ctx _ -> (fn ctx, mempty, mempty)
  localize upd ma = AdvObject $ \ctx pt -> getAdvObject ma (upd ctx) pt




instance (Monoid a) => Monoid (AdvObject u a) where
  mempty = AdvObject $ \_ _ -> (mempty, mempty, mempty)
  ma `mappend` mb = AdvObject $ \ctx pt -> 
                    let (a,v1,w1) = getAdvObject ma ctx pt
                        (b,v2,w2) = getAdvObject mb ctx pt
                        w2r       = cpmove (getDAV v1) w2
                    in (a `mappend` b, v1 `mappend` v2, w1 `mappend` w2r)



-- | Running an AdvObject produces a LocImage.
--
runAdvObject :: InterpretUnit u 
             => AdvObject u a -> LocImage u a
runAdvObject ma = promoteLoc $ \ot -> 
    askDC >>= \ctx -> 
    let dot      = normalizeF (dc_font_size ctx) ot
        (a,_,ca) = getAdvObject ma ctx dot
    in replaceAns a $ primGraphic ca



--------------------------------------------------------------------------------


-- | 'makeAdvObject' : @ loc_context_function * image -> AdvObject @
--
-- Build an 'AdvObject' from a context function ('CF') that 
-- generates the answer displacement vector and a 'LocGraphic' 
-- that draws the 'AdvObject'.
--
makeAdvObject :: InterpretUnit u 
              => Query u (Vec2 u) -> LocImage u a -> AdvObject u a
makeAdvObject ma gf = AdvObject $ \ctx pt -> 
    let v1    = runQuery ctx ma
        dav1  = DAV $ normalizeF (dc_font_size ctx) v1
        upt   = dinterpF (dc_font_size ctx) pt
        (a,w) = runLocImage ctx upt gf
    in (a,dav1,w)



-- | 'emptyAdvObjectAU' : @ AdvObject @
--
-- Build an empty 'AdvObject'.
-- 
-- The 'emptyAdvObject' is treated as a /null primitive/ by 
-- @Wumpus-Core@ and is not drawn, the answer vector generated is
-- the zero vector @(V2 0 0)@.
-- 
emptyAdvObject :: Monoid a => AdvObject u a
emptyAdvObject = mempty



blankAdvObject :: (Monoid a, InterpretUnit u) 
               => Vec2 u -> AdvObject u a
blankAdvObject v1 = AdvObject $ \ctx _ ->
                    let dav1  = DAV $ normalizeF (dc_font_size ctx) v1
                    in (mempty, dav1, mempty)





--------------------------------------------------------------------------------
-- Combining AdvObjects



-- Helper for list concatenation.
-- 
listcat :: Monoid a
        => (AdvObject u a -> AdvObject u a -> AdvObject u a)
        -> [AdvObject u a] -> AdvObject u a
listcat _ []     = mempty
listcat op (x:xs) = go x xs
  where
    go acc []     = acc
    go acc (b:bs) = go (acc `op` b) bs



-- AdvObject does not have the same ability to be concatenated
-- as PosObject - all the advance vector says is \"where to go 
-- next\". Nothing in the AdvObject tracks the boundary so we
-- cannot implement the Concat classes.

infixr 6 `advance`


-- | Draw the first AdvObject and use the advance vector to 
-- displace the second AdvObject.
--
-- The final answer is the sum of both advance vectors.
--
advance :: Monoid a 
        => AdvObject u a -> AdvObject u a -> AdvObject u a
advance = mappend
  

-- | Concatenate the list of AdvObjects with 'advance'.
--
advances :: Monoid a
         => [AdvObject u a] -> AdvObject u a
advances = mconcat


-- | Combine the AdvObjects using the answer vector of the 
-- first object plus the separator to move the start of the second
-- object. 
--
advspace :: (Monoid a, InterpretUnit u) 
         => Vec2 u -> AdvObject u a -> AdvObject u a -> AdvObject u a
advspace sep a b = a `mappend` blank `mappend` b
  where
    blank = blankAdvObject sep


-- | List version of 'nextSpace'.
--
evenspace :: (Monoid a, InterpretUnit u) 
          => Vec2 u -> [AdvObject u a] -> AdvObject u a
evenspace v = listcat (advspace v)



-- | Repeat the AdvObject @n@ times, moving each time with 
-- 'advance'.
--
advrepeat :: Monoid a
          => Int -> AdvObject u a -> AdvObject u a
advrepeat n = advances . replicate n


-- | Concatenate the list of AdvObjects, going next and adding
-- the separator at each step.
--
punctuate :: Monoid a
          => AdvObject u a -> [AdvObject u a] -> AdvObject u a
punctuate sep =  listcat (\a b -> a `advance` sep `advance` b)



-- | Render the supplied AdvObject, but swap the result advance
-- for the supplied vector. This function has behaviour analogue 
-- to @fill@ in the @wl-pprint@ library.
-- 
advfill :: InterpretUnit u 
        => Vec2 u -> AdvObject u a -> AdvObject u a
advfill sv mf = AdvObject $ \ctx pt -> 
    let (a,_,ca) = getAdvObject mf ctx pt
        dav1     = DAV $ normalizeF (dc_font_size ctx) sv 
    in (a,dav1,ca)


