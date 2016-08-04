{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Drawing.LocDrawing
-- Copyright   :  (c) Stephen Tetley 2011-2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Drawing monad with immutable start point.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Drawing.LocDrawing
  (

  -- * GenLocDrawing monad
    GenLocDrawing
  , LocDrawing

  , LocDrawM(..)

  , runGenLocDrawing
  , evalGenLocDrawing
  , execGenLocDrawing
  , stripGenLocDrawing

  , runLocDrawing
  , runLocDrawing_ 

  )

  where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Base.WrappedPrimitive
import Wumpus.Basic.Kernel.Drawing.Basis
import Wumpus.Basic.Kernel.Objects.Anchors
import Wumpus.Basic.Kernel.Objects.Basis
import Wumpus.Basic.Kernel.Objects.Connector
import Wumpus.Basic.Kernel.Objects.Image
import Wumpus.Basic.Kernel.Objects.LocImage


import Wumpus.Core                              -- package: wumpus-core


-- | 'GenLocDrawing' is a reader-writer-state monad, unlike 
-- 'GenLocTrace' there is no updateable current point, instead 
-- the start point is supplied when the drawing is run and it 
-- is translated by the components of the start point.
--
-- The writer accumulates a graphical trace.
--
-- Essentially, 'GenLocDrawing' is an 'Image' object extended 
-- with user state.
--
newtype GenLocDrawing st u a = GenLocDrawing { 
    getGenLocDrawing :: DrawingContext -> st -> (a, st, CatPrim)}

type instance DUnit  (GenLocDrawing st u a) = u
type instance UState (GenLocDrawing st u a) = st

type LocDrawing u a = GenLocDrawing () u a


-- Functor

instance Functor (GenLocDrawing st u) where
  fmap f ma = GenLocDrawing $ \ctx s -> 
    let (a,s1,o) = getGenLocDrawing ma ctx s in (f a, s1, o)


-- Applicative

instance Applicative (GenLocDrawing st u) where
  pure a    = GenLocDrawing $ \_   s -> (a, s, mempty)
  mf <*> ma = GenLocDrawing $ \ctx s -> 
                let (f,s1,o1) = getGenLocDrawing mf ctx s
                    (a,s2,o2) = getGenLocDrawing ma ctx s1
                in (f a, s2, o1 `mappend` o2)



-- Monad

instance Monad (GenLocDrawing st u) where
  return a  = GenLocDrawing $ \_   s -> (a, s, mempty)
  ma >>= k  = GenLocDrawing $ \ctx s -> 
                let (a,s1,o1) = getGenLocDrawing ma ctx s
                    (b,s2,o2) = (getGenLocDrawing . k) a ctx s1
                in (b, s2, o1 `mappend` o2)



-- DrawingCtxM

instance DrawingCtxM (GenLocDrawing st u) where
  askDC           = GenLocDrawing $ \ctx s -> (ctx, s, mempty)
  asksDC fn       = GenLocDrawing $ \ctx s -> (fn ctx, s, mempty)
  localize upd ma = GenLocDrawing $ \ctx s -> 
                      getGenLocDrawing ma (upd ctx) s



-- UserStateM 

instance UserStateM (GenLocDrawing st u) where
  getState        = GenLocDrawing $ \_ s -> (s, s, mempty)
  setState s      = GenLocDrawing $ \_ _ -> ((), s, mempty)
  updateState upd = GenLocDrawing $ \_ s -> ((), upd s, mempty)


-- Monoid

instance Monoid a => Monoid (GenLocDrawing st u a) where
  mempty           = GenLocDrawing $ \_   s -> (mempty, s, mempty)
  ma `mappend` mb  = GenLocDrawing $ \ctx s -> 
                       let (a,s1,w1) = getGenLocDrawing ma ctx s
                           (b,s2,w2) = getGenLocDrawing mb ctx s1
                       in (a `mappend` b, s2, w1 `mappend` w2)



--------------------------------------------------------------------------------

class Monad m => LocDrawM (m :: * -> *) where
  inserti   :: u ~ DUnit (m ()) => Image u a -> m a
  inserti_  :: u ~ DUnit (m ()) => Image u a -> m ()
  insertli  :: u ~ DUnit (m ()) => Anchor u -> LocImage u a -> m a
  insertli_ :: u ~ DUnit (m ()) => Anchor u -> LocImage u a -> m ()
  insertci  :: u ~ DUnit (m ()) => 
               Anchor u -> Anchor u -> ConnectorImage u a -> m a
  insertci_ :: u ~ DUnit (m ()) => 
               Anchor u -> Anchor u -> ConnectorImage u a -> m ()

  inserti_  gf       = inserti gf >> return ()
  insertli_ pt gf    = insertli pt gf >> return ()
  insertci_ p1 p2 gf = insertci p1 p2 gf >> return ()


instance InterpretUnit u => LocDrawM (GenLocDrawing st u) where
  inserti  = insertiImpl
  insertli = insertliImpl
  insertci = insertciImpl

 
--------------------------------------------------------------------------------
-- Run functions


runGenLocDrawing :: (Translate a, InterpretUnit u, u ~ DUnit a) 
                 => st -> GenLocDrawing st u a -> LocImage u (a,st)
runGenLocDrawing st ma = promoteLoc $ \(P2 x y) -> 
    askDC >>= \ctx ->
    let (a,s1,w1) = getGenLocDrawing ma ctx st
        ans       = translate x y a 
        dv1       = normalizeF (dc_font_size ctx) (V2 x y)
    in replaceAns (ans,s1) $ primGraphic $ cpmove dv1 w1




-- | Forget the user state LocImage, just return the /answer/.
--
evalGenLocDrawing :: (Translate a, InterpretUnit u, u ~ DUnit a) 
                  => st -> GenLocDrawing st u a -> LocImage u a
evalGenLocDrawing st ma = fmap fst $ runGenLocDrawing st ma


-- | Forget the /answer/, just return the user state.
--
execGenLocDrawing :: (Translate a, InterpretUnit u, u ~ DUnit a) 
                  => st -> GenLocDrawing st u a -> LocImage u st 
execGenLocDrawing st ma = fmap snd $ runGenLocDrawing st ma


stripGenLocDrawing :: (Translate a, InterpretUnit u, u ~ DUnit a) 
                   => st -> GenLocDrawing st u a -> LocQuery u (a,st)
stripGenLocDrawing st ma = stripLocImage $ runGenLocDrawing st ma


-- | Simple version of 'runGenLocDrawing' - run a 'LocDrawing' without
-- user state.
--
runLocDrawing :: (Translate a, InterpretUnit u, u ~ DUnit a) 
              => LocDrawing u a -> LocImage u a
runLocDrawing ma = evalGenLocDrawing () ma


runLocDrawing_ :: (Translate a, InterpretUnit u, u ~ DUnit a) 
               => LocDrawing u a -> LocGraphic u 
runLocDrawing_ ma = ignoreAns $ runLocDrawing ma



--------------------------------------------------------------------------------

insertiImpl :: Image u a -> GenLocDrawing st u a
insertiImpl gf = GenLocDrawing $ \ctx s -> 
    let (a,w1)   = runImage ctx gf in (a,s,w1) 





insertliImpl :: InterpretUnit u
             => Anchor u -> LocImage u a -> GenLocDrawing st u a
insertliImpl p1 gf = GenLocDrawing $ \ctx s -> 
    let (a,w1) = runLocImage ctx p1 gf in (a,s,w1) 



-- This is not right - if I\'ve taken an anchor from an object
-- within the relative coord system, the anchor points are already
-- translated respective to the origin. This implementation of 
-- @insertci@ adds the translation a second time.


insertciImpl :: InterpretUnit u 
             => Anchor u -> Anchor u -> ConnectorImage u a 
             -> GenLocDrawing st u a
insertciImpl p1 p2 gf = GenLocDrawing $ \ctx s -> 
    let (a,w1) = runConnectorImage ctx p1 p2 gf in (a,s,w1) 
