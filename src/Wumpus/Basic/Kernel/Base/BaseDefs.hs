{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernal.Base.BaseDefs
-- Copyright   :  (c) Stephen Tetley 2010-2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- The elementary base types and classes.
-- 
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Base.BaseDefs
  (

  -- * Constants
    quarter_pi
  , half_pi
  , two_pi

  , ang180
  , ang150
  , ang120
  , ang90
  , ang60
  , ang45
  , ang30
  , ang15

  -- * Unit phantom type
  , UNil(..)
  , ureturn
  , uvoid

  -- * Non-contextual unit conversion.
  , ScalarUnit(..)

  -- * Unit interpretation with respect to the current Point size
  , InterpretUnit(..)
  , dinterpF
  , normalizeF
  , uconvert1
  , uconvertF

  , intraMapPoint
  , intraMapFunctor


  -- * KernChar
  , KernChar

  -- * Drawing paths and shapes (closed paths)
  , PathMode(..)
  , DrawMode(..)
  , closedMode

  -- * Drawing layers
  , ZOrder(..)  

  -- * Alignment
  , HAlign(..)
  , VAlign(..)  

  -- * Text height
  , TextHeight(..)

  -- * Cardinal (compass) positions
  , Cardinal(..)

  -- * Direction enumeration
  , Direction(..)
  , ClockDirection(..)  
  , clockDirection

  , HDirection(..)
  , horizontalDirection
  , VDirection(..)
  , verticalDirection

  -- * Quadrant enumeration
  , Quadrant(..)
  , quadrant

  -- * Beziers

  , bezierArcPoints  
  , bezierMinorArc
  

  -- * Misc

  , both
  , monPreRepeatPost

  ) where

import Wumpus.Core                              -- package: wumpus-core


import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace

import Control.Applicative
import Data.Monoid






quarter_pi      :: Radian
quarter_pi      = 0.25 * pi

half_pi         :: Radian
half_pi         = 0.5 * pi

two_pi          :: Radian
two_pi          = 2.0 * pi


ang180          :: Radian
ang180          = pi

ang150          :: Radian
ang150          = 5 * ang30

ang120          :: Radian 
ang120          = 2 * ang60

ang90           :: Radian
ang90           = pi / 2

ang60           :: Radian
ang60           = pi / 3

ang45           :: Radian 
ang45           = pi / 4

ang30           :: Radian
ang30           = pi / 6

ang15           :: Radian
ang15           = pi / 12



--------------------------------------------------------------------------------
-- Simple objects wrapped with unit phatom type 


-- | The empty data type - i.e. @()@ - wrapped with a phantom unit 
-- parameter.
--
data UNil   u = UNil          deriving (Eq,Ord,Read,Show)

type instance DUnit (UNil u) = u

instance Functor UNil where
  fmap _ UNil= UNil


instance Monoid (UNil u) where
  mempty        = UNil
  _ `mappend` _ = UNil




instance Rotate (UNil u) where
  rotate _              = id

instance RotateAbout (UNil u) where
  rotateAbout _ _       = id

instance Scale (UNil u) where
  scale _ _             = id

instance Translate (UNil u) where
  translate _ _         = id

-- | Return a 'UNil' rather than @()@ at the end of sequence of
-- monadic commands.
--
-- Many Wumpus objects are usefully constructed in the 
-- @do-notation@, but because Wumpus has to expose the type of 
-- the @unit@ to the type checker we must finish the do-block 
-- with:
--
-- > ureturn
-- 
-- or:
-- 
-- > return UNil
--
-- rather than:
--
-- > return ()
--
--
ureturn :: Monad m => m (UNil u)
ureturn = return UNil

-- | 'uvoid' runs a monadic computation and returns @UNil@.
--
uvoid :: Monad m => m a -> m (UNil u)
uvoid ma = ma >> return UNil

--------------------------------------------------------------------------------
-- Non-contextual units

class ScalarUnit a where
  fromPsPoint :: Double -> a 
  toPsPoint   :: a -> Double

instance ScalarUnit Double where
  fromPsPoint = id
  toPsPoint   = id 



--------------------------------------------------------------------------------
-- Interpreting units 

-- Units may or may not depend on current font size
--

class (Eq u, Num u) => InterpretUnit u where
  normalize :: FontSize -> u -> Double
  dinterp   :: FontSize -> Double -> u

instance InterpretUnit Double where
  normalize _ = id
  dinterp   _ = id 

instance InterpretUnit AfmUnit where
  normalize sz = afmValue sz 
  dinterp   sz = afmUnit sz


-- | 'dinterp' an object that gives access to its unit at the 
-- functor position.
--
dinterpF :: (Functor t, InterpretUnit u) => FontSize -> t Double -> t u
dinterpF sz = fmap (dinterp sz)


-- | 'normalize' an object that gives access to its unit at the 
-- functor position.
--
normalizeF :: (Functor t, InterpretUnit u) => FontSize -> t u -> t Double
normalizeF sz = fmap (normalize sz)


-- | Convert a scalar value from one unit to another.
--
uconvert1 :: (InterpretUnit u, InterpretUnit u1) => FontSize -> u -> u1
uconvert1 sz = dinterp sz . normalize sz

-- | Unit convert an object that gives access to its unit at the
-- Functor position.
--
-- In practive this will be \*all\* Image answers.
--
uconvertF :: (Functor t, InterpretUnit u, InterpretUnit u1) 
          => FontSize -> t u -> t u1
uconvertF sz = fmap (uconvert1 sz)



-- Helper for defining Affine instances. This function allows 
-- scaling etc to be applied on a Point coerced to a Double then
-- converted back to the original unit. Thus transformations can 
-- work in contextual units.
--
intraMapPoint :: InterpretUnit u 
              => FontSize -> (DPoint2 -> DPoint2) -> Point2 u -> Point2 u
intraMapPoint sz fn (P2 x y) = 
    let P2 x' y' = fn $ P2 (normalize sz x) (normalize sz y)
    in  P2 (dinterp sz x') (dinterp sz y')



-- Helper for defining Affine instances. This function allows 
-- scaling etc to be applied on a Point coerced to a Double then
-- converted back to the original unit. Thus transformations can 
-- work in contextual units.
--
intraMapFunctor :: (Functor f, InterpretUnit u)
                => FontSize -> (f Double -> f Double) -> f u -> f u
intraMapFunctor sz fn ma = dinterpF sz $ fn $ normalizeF sz ma


--------------------------------------------------------------------------------
-- KernChar


-- | Unit parametric version of KerningChar from Wumpus-Core.
--
type KernChar u = (u,EscapedChar)


--------------------------------------------------------------------------------
-- Drawing closed paths


-- | Draw closed paths. 
-- 
-- > OSTROKE - open and stroked
--
-- > CSTROKE - closed and stroke
--
-- > CFILL - closed and filled
--
-- > CFILL_STROKE - closed, the path is filled, its edge is stroked.
--
data PathMode = OSTROKE | CSTROKE | CFILL | CFILL_STROKE
  deriving (Bounded,Enum,Eq,Ord,Show)



-- | Draw closed paths and shapes. 
-- 
-- > DRAW_STROKE - closed and stroked
--
-- > DRAW_FILL - closed and filled
--
-- > DRAW_FILL_STROKE - the path is filled, its edge is stroked.
--
data DrawMode = DRAW_STROKE | DRAW_FILL | DRAW_FILL_STROKE
  deriving (Bounded,Enum,Eq,Ord,Show)


-- | Interpret a 'DrawMode' for a closed path.
--
closedMode :: DrawMode -> PathMode
closedMode DRAW_STROKE      = CSTROKE 
closedMode DRAW_FILL        = CFILL 
closedMode DRAW_FILL_STROKE = CFILL_STROKE





-- | Enumerated type for drawing with respect to the z-order.
--
data ZOrder = ZBELOW | ZABOVE
  deriving (Bounded,Enum,Eq,Ord,Show)


--------------------------------------------------------------------------------

-- Alignment

-- | Horizontal alignment - align to the top, center or bottom.
--
data HAlign = HALIGN_TOP | HALIGN_CENTER | HALIGN_BASE
  deriving (Enum,Eq,Ord,Show)

-- | Vertical alignment - align to the left, center or bottom.
--
data VAlign = VALIGN_LEFT | VALIGN_CENTER | VALIGN_RIGHT
  deriving (Enum,Eq,Ord,Show)


--------------------------------------------------------------------------------
-- Text height


-- | Wumpus distinguishes two use-cases for displaying vertically 
-- centered text.
-- 
-- Arbitrary text that is expected to contain lower case letters 
-- with descenders, show take the vertical center as the mid-point 
-- between the cap height and the descender depth.
--
-- Unfortunately, including the descender depth can produce 
-- unbalanced results for text which is not expected to have 
-- descenders (e.g. numbers within a bordered box), visually this 
-- makes the center too high.
-- 
data TextHeight = JUST_CAP_HEIGHT | CAP_HEIGHT_PLUS_DESCENDER
  deriving (Enum,Eq,Ord,Show)



--------------------------------------------------------------------------------

-- Compass positions

-- | An enumeratied type representing the compass positions.
--
data Cardinal = NORTH | NORTH_EAST | EAST | SOUTH_EAST 
              | SOUTH | SOUTH_WEST | WEST | NORTH_WEST
   deriving (Enum,Eq,Ord,Show) 


-- | An enumerated type representing horizontal and vertical 
-- directions.
--
data Direction = UP | DOWN | LEFT | RIGHT
   deriving (Enum,Eq,Ord,Show) 


-- | An enumerated type representing horizontal direction.
--
data HDirection = LEFTWARDS | RIGHTWARDS
   deriving (Enum,Eq,Ord,Show) 


horizontalDirection :: Radian -> HDirection
horizontalDirection = fn . circularModulo
  where
    fn a | a <= 0.5*pi || a > 1.5*pi = RIGHTWARDS
         | otherwise                 = LEFTWARDS

-- | An enumerated type representing vertical direction.
--
data VDirection = UPWARDS | DOWNWARDS
   deriving (Enum,Eq,Ord,Show) 


verticalDirection :: Radian -> VDirection
verticalDirection = fn . circularModulo
  where
    fn a | a <= pi   = UPWARDS
         | otherwise = DOWNWARDS
  

-- | An enumerated type representing /clock/ directions.
--
data ClockDirection = CW | CCW
   deriving (Enum,Eq,Ord,Show) 



-- | Note - behaviour at the continuity (0 deg, 180 deg, ...) is
-- unspecified.
--
clockDirection :: (Real u, Floating u) 
               => Vec2 u -> Vec2 u -> ClockDirection
clockDirection v1 v2 = if a1 < asum then CW else CCW
  where
    a1   = r2d $ vdirection v1
    asum = r2d $ vdirection (v1 ^+^ v2)



-- | An enumerated type representing quadrants.
-- 
data Quadrant = QUAD_NE | QUAD_NW | QUAD_SW | QUAD_SE
  deriving (Enum,Eq,Ord,Show)

-- | 'quadrant' : @ ang -> Quadrant @
--
-- Get the quadrant of an angle.
--
quadrant :: Radian -> Quadrant
quadrant = fn . circularModulo
  where
    fn a | a < 0.5*pi   = QUAD_NE
         | a < pi       = QUAD_NW
         | a < 1.5*pi   = QUAD_SW
         | otherwise    = QUAD_SE



--------------------------------------------------------------------------------
-- Beziers

kappa :: Floating u => u
kappa = 4 * ((sqrt 2 - 1) / 3)



-- | 'bezierArcPoints' : @ apex_angle * radius * inclination * center -> [Point] @
--
-- > ang should be in the range 0 < ang < 360deg.
--
-- > if   0 < ang <=  90 returns 4 points
-- > if  90 < ang <= 180 returns 7 points
-- > if 180 < ang <= 270 returns 10 points
-- > if 270 < ang <  360 returns 13 points
--
bezierArcPoints ::  Floating u 
                => Radian -> u -> Radian -> Point2 u -> [Point2 u]
bezierArcPoints ang radius theta pt = go (circularModulo ang)
  where
    go a | a <= half_pi = wedge1 a
         | a <= pi      = wedge2 (a/2)
         | a <= 1.5*pi  = wedge3 (a/3)
         | otherwise    = wedge4 (a/4)
    
    wedge1 a = 
      let (p0,p1,p2,p3) = bezierMinorArc a radius theta pt
      in [p0,p1,p2,p3]

    wedge2 a = 
      let (p0,p1,p2,p3) = bezierMinorArc a radius theta pt
          (_ ,p4,p5,p6) = bezierMinorArc a radius (theta+a) pt
      in [ p0,p1,p2,p3, p4,p5,p6 ] 

    wedge3 a = 
      let (p0,p1,p2,p3) = bezierMinorArc a radius theta pt
          (_ ,p4,p5,p6) = bezierMinorArc a radius (theta+a) pt
          (_ ,p7,p8,p9) = bezierMinorArc a radius (theta+a+a) pt
      in [ p0,p1,p2,p3, p4,p5,p6, p7, p8, p9 ] 
  
    wedge4 a = 
      let (p0,p1,p2,p3)    = bezierMinorArc a radius theta pt
          (_ ,p4,p5,p6)    = bezierMinorArc a radius (theta+a) pt
          (_ ,p7,p8,p9)    = bezierMinorArc a radius (theta+a+a) pt
          (_ ,p10,p11,p12) = bezierMinorArc a radius (theta+a+a+a) pt
      in [ p0,p1,p2,p3, p4,p5,p6, p7,p8,p9, p10,p11, p12 ] 


-- | 'bezierMinorArc' : @ apex_angle * radius * rotation * center -> BezierCurve @
--
-- > ang should be in the range 0 < ang <= 90deg.
--
bezierMinorArc :: Floating u 
               => Radian -> u -> Radian -> Point2 u 
               -> (Point2 u, Point2 u, Point2 u, Point2 u)
bezierMinorArc ang radius theta pt = (p0,p1,p2,p3)
  where
    kfactor = fromRadian $ ang / (0.5*pi)
    rl      = kfactor * radius * kappa
    totang  = circularModulo $ ang + theta

    p0      = pt .+^ orthoVec radius 0 theta
    p1      = p0 .+^ orthoVec 0 rl theta
    p2      = p3 .+^ orthoVec 0 (-rl) totang
    p3      = pt .+^ orthoVec radius 0 totang


--------------------------------------------------------------------------------


-- | Applicative /both/ - run both computations return the pair
-- of the the answers.
--
both :: Applicative f => f a -> f b -> f (a,b)
both fa fb = (,) <$> fa <*> fb


-- | Monodial scheme - prefix, repeat body n times, suffix.
--
monPreRepeatPost :: Monoid a => a -> (Int, a) -> a -> a
monPreRepeatPost pre (n,body1) post = step pre n
  where
    step ac i | i < 1     = ac `mappend` post
              | otherwise = step (ac `mappend` body1) (i - 1) 
    