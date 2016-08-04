{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Drawing.LocTrace
-- Copyright   :  (c) Stephen Tetley 2011-2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Writer monad with imperative /turtle/ style movement to build 
-- LocGraphics.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Drawing.LocTrace
  (

  -- * GenLocTrace monad
    GenLocTrace
  , LocTrace

  , runGenLocTrace
  , evalGenLocTrace
  , execGenLocTrace
  , stripGenLocTrace

  , runLocTrace
  , runLocTrace_ 

  )

  where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Base.WrappedPrimitive
import Wumpus.Basic.Kernel.Drawing.Basis
import Wumpus.Basic.Kernel.Objects.Basis
import Wumpus.Basic.Kernel.Objects.Image
import Wumpus.Basic.Kernel.Objects.LocImage


import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

import Control.Applicative
import Data.Monoid




-- | GenLocTrace is a reader-writer-state monad.
--
-- The writer accumulates a graphical trace and the state is 
-- the current point.
--
newtype GenLocTrace st u a = GenLocTrace { 
    getGenLocTrace :: DrawingContext -> DPoint2 -> st 
                   -> (a, DPoint2, st, CatPrim)}

type instance DUnit  (GenLocTrace st u a) = u
type instance UState (GenLocTrace st u a) = st

type LocTrace u a = GenLocTrace () u a


-- Functor

instance Functor (GenLocTrace st u) where
  fmap f ma = GenLocTrace $ \ctx pt s -> 
    let (a,p1,s1,o) = getGenLocTrace ma ctx pt s in (f a, p1, s1, o)


-- Applicative

instance Applicative (GenLocTrace st u) where
  pure a    = GenLocTrace $ \_   pt s -> (a, pt, s, mempty)
  mf <*> ma = GenLocTrace $ \ctx pt s -> 
                let (f,p1,s1,o1) = getGenLocTrace mf ctx pt s
                    (a,p2,s2,o2) = getGenLocTrace ma ctx p1 s1
                in (f a, p2, s2, o1 `mappend` o2)



-- Monad

instance Monad (GenLocTrace st u) where
  return a  = GenLocTrace $ \_   pt s -> (a, pt, s, mempty)
  ma >>= k  = GenLocTrace $ \ctx pt s -> 
                let (a,p1,s1,o1) = getGenLocTrace ma ctx pt s
                    (b,p2,s2,o2) = (getGenLocTrace . k) a ctx p1 s1
                in (b, p2, s2, o1 `mappend` o2)



-- DrawingCtxM

instance DrawingCtxM (GenLocTrace st u) where
  askDC           = GenLocTrace $ \ctx pt s -> (ctx, pt, s, mempty)
  asksDC fn       = GenLocTrace $ \ctx pt s -> (fn ctx, pt, s, mempty)
  localize upd ma = GenLocTrace $ \ctx pt s -> getGenLocTrace ma (upd ctx) pt s



-- UserStateM 

instance UserStateM (GenLocTrace st u) where
  getState        = GenLocTrace $ \_ pt s -> (s, pt, s, mempty)
  setState s      = GenLocTrace $ \_ pt _ -> ((), pt, s, mempty)
  updateState upd = GenLocTrace $ \_ pt s -> ((), pt, upd s, mempty)


-- Monoid

instance Monoid a => Monoid (GenLocTrace st u a) where
  mempty           = GenLocTrace $ \_   pt s -> (mempty, pt, s, mempty)
  ma `mappend` mb  = GenLocTrace $ \ctx pt s -> 
                       let (a,p1,s1,w1) = getGenLocTrace ma ctx pt s
                           (b,p2,s2,w2) = getGenLocTrace mb ctx p1 s1
                       in (a `mappend` b, p2, s2, w1 `mappend` w2)


-- LocationM

instance InterpretUnit u => LocationM (GenLocTrace st u) where
  location = GenLocTrace $ \ctx pt s ->
      let upt = dinterpF (dc_font_size ctx) pt in (upt, pt, s, mempty) 


-- CursorM 

instance InterpretUnit u => InsertlM (GenLocTrace st u) where
  insertl   = insertlImpl

instance InterpretUnit u => CursorM (GenLocTrace st u) where
  moveby    = movebyImpl


insertlImpl :: InterpretUnit u => LocImage u a -> GenLocTrace st u a
insertlImpl gf = GenLocTrace $ \ctx pt s ->
    let upt    = dinterpF (dc_font_size ctx) pt 
        (a,w1) = runLocImage ctx upt gf
    in (a,pt,s,w1) 


movebyImpl :: InterpretUnit u => Vec2 u -> GenLocTrace st u ()
movebyImpl v1 = GenLocTrace $ \ctx pt s ->
    let dv1 = normalizeF (dc_font_size ctx) v1 
    in ((), pt .+^ dv1, s, mempty) 



-- BranchCursorM 

instance InterpretUnit u => BranchCursorM (GenLocTrace st u) where
  branchCursor ma = GenLocTrace $ \ctx pt s ->
    let (a,_,s1,w1) = getGenLocTrace ma ctx pt s
    in (a, pt, s1, w1)  -- re-instate pt          



--------------------------------------------------------------------------------
-- Run functions


runGenLocTrace :: InterpretUnit u 
               => st -> GenLocTrace st u a -> LocImage u (a,st)
runGenLocTrace st ma = promoteLoc $ \pt -> 
    askDC >>= \ctx ->
    let dpt         = normalizeF (dc_font_size ctx) pt
        (a,_,s1,w1) = getGenLocTrace ma ctx dpt st
    in replaceAns (a,s1) $ primGraphic w1




-- | Forget the user state LocImage, just return the /answer/.
--
evalGenLocTrace :: InterpretUnit u 
                => st -> GenLocTrace st u a -> LocImage u a
evalGenLocTrace st ma = fmap fst $ runGenLocTrace st ma


-- | Forget the /answer/, just return the user state.
--
execGenLocTrace :: InterpretUnit u 
                => st -> GenLocTrace st u a -> LocImage u st 
execGenLocTrace st ma = fmap snd $ runGenLocTrace st ma


stripGenLocTrace :: InterpretUnit u 
                 => st -> GenLocTrace st u a -> LocQuery u (a,st)
stripGenLocTrace st ma = stripLocImage $ runGenLocTrace st ma


-- | Simple version of 'runGenLocTrace' - run a 'LocTrace' without
-- user state.
--
runLocTrace :: InterpretUnit u 
            => LocTrace u a -> LocImage u a
runLocTrace ma = evalGenLocTrace () ma


runLocTrace_ :: InterpretUnit u 
             => LocTrace u a -> LocGraphic u 
runLocTrace_ ma = ignoreAns $ runLocTrace ma

