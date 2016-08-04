{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.LocThetaImage
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- LocThetaImage and LocThetaGraphic types - these are functional 
-- types from the DrawingContext, start point and angle of 
-- inclination to a graphic /primitive/.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.LocThetaImage
   (
     LocThetaGraphic
   , LocThetaImage

   , DLocThetaGraphic
   , DLocThetaImage

   , LocThetaQuery

   , runLocThetaImage
   , runLocThetaQuery

   , stripLocThetaImage
   , liftLocThetaQuery
   
   , promoteLocTheta
   , applyLocTheta
   , supplyLocTheta
   , qpromoteLocTheta
   , qapplyLocTheta

   , emptyLocThetaImage

   , incline
   , atIncline
   , supplyIncline
   
   )

   where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Base.QueryDC
import Wumpus.Basic.Kernel.Objects.Basis
import Wumpus.Basic.Kernel.Objects.Image
import Wumpus.Basic.Kernel.Objects.LocImage

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative
import Data.Monoid

-- | 'LocThetaImage' - function from start point, inclination and
-- DrawingContext to a polymorphic /answer/ and a graphic 
-- /primitive/ (PrimW).
--
newtype LocThetaImage u a = LocThetaImage { 
          getLocThetaImage :: DPoint2 -> Radian -> Image u a }

type instance DUnit (LocThetaImage u a) = u

type LocThetaGraphic u = LocThetaImage u (UNil u)


-- | Type specialized version of 'LocThetaImage'.
--
type DLocThetaImage a        = LocThetaImage Double a

-- | Type specialized version of 'LocThetaGraphic'.
--
type DLocThetaGraphic        = LocThetaGraphic Double 


newtype LocThetaQuery u a = LocThetaQuery { 
          getLocThetaQuery :: DPoint2 -> Radian -> Query u a }

-- Functor

instance Functor (LocThetaImage u) where
  fmap f ma = LocThetaImage $ \pt ang -> 
                fmap f $ getLocThetaImage ma pt ang

instance Functor (LocThetaQuery u) where
  fmap f ma = LocThetaQuery $ \pt ang -> 
                fmap f $ getLocThetaQuery ma pt ang


-- Applicative

instance Applicative (LocThetaImage u) where
  pure a    = LocThetaImage $ \_  _   -> pure a
  mf <*> ma = LocThetaImage $ \pt ang -> 
                getLocThetaImage mf pt ang <*> getLocThetaImage ma pt ang

instance Applicative (LocThetaQuery u) where
  pure a    = LocThetaQuery $ \_  _   -> pure a
  mf <*> ma = LocThetaQuery $ \pt ang -> 
                getLocThetaQuery mf pt ang <*> getLocThetaQuery ma pt ang


-- Monad 

instance Monad (LocThetaImage u) where
  return a  = LocThetaImage $ \_  _   -> return a
  ma >>= k  = LocThetaImage $ \pt ang -> 
                getLocThetaImage ma pt ang >>= \ans -> 
                getLocThetaImage (k ans) pt ang


instance Monad (LocThetaQuery u) where
  return a  = LocThetaQuery $ \_  _   -> return a
  ma >>= k  = LocThetaQuery $ \pt ang -> 
                getLocThetaQuery ma pt ang >>= \ans -> 
                getLocThetaQuery (k ans) pt ang

-- Monoid

instance Monoid a => Monoid (LocThetaImage u a) where
  mempty          = pure mempty
  ma `mappend` mb = LocThetaImage $ \pt ang -> 
                      getLocThetaImage ma pt ang 
                         `mappend` getLocThetaImage mb pt ang



instance Monoid a => Monoid (LocThetaQuery u a) where
  mempty          = pure mempty
  ma `mappend` mb = LocThetaQuery $ \pt ang -> 
                      getLocThetaQuery ma pt ang 
                         `mappend` getLocThetaQuery mb pt ang

-- DrawingCtxM

instance DrawingCtxM (LocThetaImage u) where
  askDC           = LocThetaImage $ \_  _   -> askDC
  asksDC fn       = LocThetaImage $ \_  _   -> asksDC fn
  localize upd ma = LocThetaImage $ \pt ang -> 
                      localize upd (getLocThetaImage ma pt ang)

instance DrawingCtxM (LocThetaQuery u) where
  askDC           = LocThetaQuery $ \_  _   -> askDC
  asksDC fn       = LocThetaQuery $ \_  _   -> asksDC fn
  localize upd ma = LocThetaQuery $ \pt ang -> 
                      localize upd (getLocThetaQuery ma pt ang)

--

instance Decorate LocThetaImage where
  decorate zo ma mz     = LocThetaImage $ \pt ang -> 
    decorate zo (getLocThetaImage ma pt ang) (getLocThetaImage mz pt ang)

  elaborate zo ma f     = LocThetaImage $ \pt ang -> 
    elaborate zo (getLocThetaImage ma pt ang) 
                 (\a -> getLocThetaImage (f a) pt ang)

  obliterate ma         = LocThetaImage $ \pt ang -> 
    obliterate $ getLocThetaImage ma pt ang

  hyperlink xl ma       = LocThetaImage $ \pt ang -> 
    hyperlink xl $ getLocThetaImage ma pt ang

  svgId ss ma           = LocThetaImage $ \pt ang -> 
    svgId ss $ getLocThetaImage ma pt ang

  svgAnnotate attrs ma  = LocThetaImage $ \pt ang -> 
    svgAnnotate attrs $ getLocThetaImage ma pt ang
           



runLocThetaImage :: InterpretUnit u 
                 => DrawingContext -> Point2 u -> Radian
                 -> LocThetaImage u a 
                 -> PrimResult u a
runLocThetaImage ctx pt incl ma = 
    let dpt = normalizeF (dc_font_size ctx) pt 
    in runImage ctx $ getLocThetaImage ma dpt incl


runLocThetaQuery :: InterpretUnit u 
                 => DrawingContext -> Point2 u -> Radian  
                 -> LocThetaQuery u a 
                 -> a
runLocThetaQuery ctx pt incl ma = 
    let dpt = normalizeF (dc_font_size ctx) pt 
    in runQuery ctx $ getLocThetaQuery ma dpt incl



stripLocThetaImage :: LocThetaImage u a -> LocThetaQuery u a
stripLocThetaImage ma = LocThetaQuery $ \pt ang -> 
    stripImage $ getLocThetaImage ma pt ang


liftLocThetaQuery :: LocThetaQuery u a -> LocThetaImage u a
liftLocThetaQuery ma = LocThetaImage $ \pt ang -> 
    liftQuery $ getLocThetaQuery ma pt ang




promoteLocTheta ::  InterpretUnit u 
                => (Point2 u -> Radian -> Image u a) -> LocThetaImage u a
promoteLocTheta k = LocThetaImage $ \pt ang -> 
    dinterpCtxF pt >>= \upt -> k upt ang

applyLocTheta :: InterpretUnit u 
              => LocThetaImage u a -> Point2 u -> Radian -> Image u a
applyLocTheta ma pt ang = 
    normalizeCtxF pt >>= \dpt -> getLocThetaImage ma dpt ang

supplyLocTheta :: InterpretUnit u 
               => Point2 u -> Radian -> LocThetaImage u a -> Image u a
supplyLocTheta pt ang ma = applyLocTheta ma pt ang


qpromoteLocTheta :: InterpretUnit u 
                 => (Point2 u -> Radian -> Query u a) -> LocThetaQuery u a
qpromoteLocTheta k = LocThetaQuery $ \pt ang ->
    dinterpCtxF pt >>= \upt -> k upt ang


qapplyLocTheta :: InterpretUnit u
               => LocThetaQuery u a -> Point2 u -> Radian -> Query u a
qapplyLocTheta ma pt ang = 
    normalizeCtxF pt >>= \dpt -> getLocThetaQuery ma dpt ang




instance UConvert LocThetaImage where
  uconvF = uconvLocThetaImageF
  uconvZ = uconvLocThetaImageZ


-- | Use this to convert 'LocThetaThetaGraphic' or 'LocThetaThetaImage' 
-- with Functor answer.
--
uconvLocThetaImageF :: (InterpretUnit u, InterpretUnit u1, Functor t) 
                    => LocThetaImage u (t u) -> LocThetaImage u1 (t u1)
uconvLocThetaImageF ma = LocThetaImage $ \pt ang -> 
    uconvF $ getLocThetaImage ma pt ang




-- | Use this to convert 'LocThetaImage' with unit-less answer.
--
uconvLocThetaImageZ :: (InterpretUnit u, InterpretUnit u1) 
                    => LocThetaImage u a -> LocThetaImage u1 a
uconvLocThetaImageZ ma = LocThetaImage $ \pt ang -> 
    uconvZ $ getLocThetaImage ma pt ang



-- | Having /empty/ at the specific 'LocThetaImage' type is useful.
-- 
emptyLocThetaImage :: Monoid a => LocThetaImage u a
emptyLocThetaImage = mempty





infixr 1 `incline`


-- | Downcast a 'LocThetaImage' function by applying it to the 
-- supplied angle, making a 'LocImage'. 
-- 
incline :: InterpretUnit u => LocThetaImage u a -> Radian -> LocImage u a
incline ma incl = promoteLoc $ \pt -> 
    normalizeCtxF pt >>= \dpt -> getLocThetaImage ma dpt incl

atIncline :: InterpretUnit u 
          => LocThetaImage u a -> Point2 u -> Radian -> Image u a
atIncline = applyLocTheta


-- | Flipped version of 'incline'
--
supplyIncline :: InterpretUnit u => Radian -> LocThetaImage u a -> LocImage u a
supplyIncline = flip incline