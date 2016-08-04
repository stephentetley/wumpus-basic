{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RankNTypes                 #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Drawing.CtxPicture
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- A Picture-with-implicit-context object. 
-- 
-- This is the corresponding type to Picture in the Wumpus-Core.
-- 
-- Note - many of the composition functions are in 
-- /destructor form/. As Wumpus cannot make a Picture from an 
-- empty list of Pictures, /destructor form/ decomposes the 
-- list into the @head@ and @rest@ as arguments in the function 
-- signature, rather than take a possibly empty list and have to 
-- throw an error.
-- 
-- TODO - PosImage no longer supports composition operators, so 
-- better names are up for grabs...
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Drawing.CtxPicture
  (

    CtxPicture
  , runCtxPicture
  , runCtxPictureU
  , drawTracing
  , udrawTracing

  , mapCtxPicture

  -- * Composition

  , uniteCenter
  
  , centeredAt

  ) where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Drawing.TraceDrawing
import Wumpus.Basic.Kernel.Objects.Anchors
import Wumpus.Basic.Kernel.Objects.Concat

import Wumpus.Core                              -- package: wumpus-core

import Data.AdditiveGroup                       -- package: vector-space
import Data.AffineSpace




-- | A /Contextual/ Picture.
-- 
-- > CtxPicture = DrawingContext -> Maybe Picture
-- 
-- This type corresponds to the 'Picture' type in Wumpus-Core, but
-- it is embedded with a 'DrawingContext' (for font properties, 
-- fill colour etc.). The DrawingContext is embedded so that font
-- metrics - loaded in @IO@ can be passed into the pure world of
-- 'TraceDrawing'.
--
-- Internally a /context picture/ is a function from 
-- 'DrawingContext' to @(Maybe Picture)@. The @Maybe@ represents
-- that it is possible to construct empty Pictures, even though
-- @Wumpus-Core@ cannot render them. Just as the DrawingContext
-- pushes font-metrics from the IO to the pure world, the Maybe
-- lifts the problem of unrenderable Pictures into the API where
-- client code must deal with it explicitly. 
--
-- (In practice, it is very unlikely a program will create empty 
-- pictures and @runCtxPictureU@ can be used without worry).
-- 
-- 
-- Note - pictures are fixed to the unit @Double@ (representing 
-- PostScript points). Pictures are intentionally unsophisticated,
-- any fine grained control of units should be delegated to the 
-- elements that build the picture (Graphics, LocGraphics, etc.). 
--
newtype CtxPicture = CtxPicture { 
          getCtxPicture :: DrawingContext -> Maybe Picture }

type instance DUnit CtxPicture = Double




-- | 'runCtxPicture' : @ drawing_ctx * ctx_picture -> Maybe Picture @
--
-- Run a 'CtxPicture' with the supplied 'DrawingContext' 
-- producing a 'Picture'.
--
-- The resulting Picture may be empty. Wumpus-Core cannot 
-- generate empty pictures as they have no bounding box, so the 
-- result is wrapped within a Maybe. This delegates reponsibility 
-- for handling empty pictures to client code.
--
runCtxPicture :: DrawingContext -> CtxPicture -> Maybe Picture
runCtxPicture ctx drw = getCtxPicture drw ctx


-- | 'runCtxPictureU' : @ drawing_ctx * ctx_picture -> Picture @
--
-- /Unsafe/ version of 'runCtxPicture'.
--
-- This function throws a runtime error when supplied with an
-- empty CtxPicture.
--
runCtxPictureU :: DrawingContext -> CtxPicture -> Picture
runCtxPictureU ctx df = maybe fk id $ runCtxPicture ctx df
  where
    fk = error "runCtxPictureU - empty CtxPicture."   


-- | 'drawTracing' : @ trace_drawing  -> CtxPicture @
--
-- Transform a 'TraceDrawing' into a 'CtxPicture'.
--
drawTracing :: TraceDrawing u a -> CtxPicture
drawTracing ma = 
    CtxPicture $ \ctx -> liftToPictureMb $ execTraceDrawing ctx ma

-- | 'udrawTracing' : @ scalar_unit_value * trace_drawing  -> CtxPicture @
--
-- Variant of 'drawTracing' with a phantom first argument - the 
-- phantom identifies the unit type of the 'TraceDrawing'. It is 
-- not scurtinized at the value level.
--
--
udrawTracing :: u -> TraceDrawing u a -> CtxPicture
udrawTracing _ ma = 
    CtxPicture $ \ctx -> liftToPictureMb $ execTraceDrawing ctx ma


-- Note need Gen versions with user state...


-- | 'mapCtxPicture' : @ trafo * ctx_picture -> CtxPicture @
--
-- Apply a picture transformation function to the 'Picture'
-- warpped in a 'CtxPicture'.
--
mapCtxPicture :: (Picture -> Picture) -> CtxPicture -> CtxPicture
mapCtxPicture pf pic1 = CtxPicture $ \ctx -> fmap pf $ getCtxPicture pic1 ctx


--------------------------------------------------------------------------------
-- Affine instances


instance Rotate CtxPicture where 
  rotate ang            = mapCtxPicture (rotate ang)

instance RotateAbout CtxPicture where
  rotateAbout ang pt    = mapCtxPicture (rotateAbout ang pt)

instance Scale CtxPicture where
  scale sx sy           = mapCtxPicture (scale sx sy)

instance Translate CtxPicture where
  translate dx dy       = mapCtxPicture (translate dx dy)



--------------------------------------------------------------------------------
-- Monoid

-- | Avoid initial mempty for mconcat.
--
instance Monoid CtxPicture where
  mempty  = CtxPicture $ \_ -> Nothing
  mappend = moveSnd $ \_ _ -> V2 0 0

  mconcat []      = mempty
  mconcat (a:as)  = step a as
    where
      step ac []     = ac
      step ac (x:xs) = step (ac `mappend` x) xs


--------------------------------------------------------------------------------
-- Extract /planes/.


leftEdge        :: BoundingBox Double -> Double
leftEdge        = point_x . ll_corner

rightEdge       :: BoundingBox Double -> Double
rightEdge       = point_x . ur_corner

bottomEdge      :: BoundingBox Double -> Double
bottomEdge      = point_y . ll_corner


topEdge         :: BoundingBox Double -> Double
topEdge         = point_y . ur_corner





--------------------------------------------------------------------------------
-- Composition operators

-- Naming convention - Wumpus-Core already prefixes operations
-- on Pictures with pic. As the picture operators here work on a
-- different type, they merit a different naming scheme.
--
-- Unfortunately the @cxp_@ prefix is rather ugly...
--
-- Directional names seem better than positional ones (less 
-- ambiguous as when used as binary operators).
--



combineP2 :: (Picture -> Picture -> Picture) 
          -> CtxPicture -> CtxPicture -> CtxPicture
combineP2 op mf mg = 
    CtxPicture $ \ctx -> fn (getCtxPicture mf ctx) (getCtxPicture mg ctx)
  where
    fn (Just a) (Just b) = Just $ a `op` b
    fn a        Nothing  = a
    fn Nothing  b        = b


-- Note - the megaCombR operator is in some way an
-- /anti-combinator/. It seems easier to think about composing 
-- drawings if we do work on the result Pictures directly rather 
-- than build combinators to manipulate CtxPictures.
--
-- The idea of combining pre- and post- operating combinators
-- makes me worry about circular programs even though I know 
-- lazy evaluation allows me to write them (in some cicumstances).
--


moveSnd :: (DBoundingBox -> DBoundingBox -> DVec2) 
          -> CtxPicture -> CtxPicture
          -> CtxPicture
moveSnd mkV = combineP2 fn
  where
    fn pl pr = let v1  = mkV (boundary pl) (boundary pr)
               in pl `picOver` (picMoveBy pr v1)


instance ZConcat CtxPicture where
  superior = mappend
  anterior = flip mappend

--------------------------------------------------------------------------------
-- Composition


infixr 6 `uniteCenter`




-- | Draw @a@, move @b@ so its center is at the same center as 
-- @a@, @b@ is drawn over underneath in the zorder.
--
-- > a `cxpUniteCenter` b 
--

uniteCenter :: CtxPicture -> CtxPicture -> CtxPicture
uniteCenter = moveSnd $ \a b -> center a .-. center b
--
-- Are combinator names less ambiguous if they name direction
-- rather than position?
--

instance Concat CtxPicture where
  hconcat = cxpRight
  vconcat = cxpBelow


-- | > a `cxpRight` b
-- 
-- Horizontal composition - position picture @b@ to the right of 
-- picture @a@.
-- 
cxpRight :: CtxPicture -> CtxPicture -> CtxPicture
cxpRight = moveSnd $ \a b -> hvec $ rightEdge a - leftEdge b


-- | > a `cxpBelow` b
--
-- Vertical composition - position picture @b@ /down/ from picture
-- @a@.
--
cxpBelow :: CtxPicture -> CtxPicture -> CtxPicture
cxpBelow = moveSnd $ \a b -> vvec $ bottomEdge a - topEdge b


-- | Center the picture at the supplied point.
--
centeredAt :: CtxPicture -> DPoint2 -> CtxPicture
centeredAt pic (P2 x y) = mapCtxPicture fn pic
  where
    fn p = let bb = boundary p
               dx = x - (boundaryWidth  bb * 0.5)
               dy = y - (boundaryHeight bb * 0.5)
           in p `picMoveBy` vec dx dy




--------------------------------------------------------------------------------

instance CatSpace CtxPicture where
  hspace = cxpRightSep
  vspace = cxpDownSep


-- | > cxpRightSep n a b
--
-- Horizontal composition - move @b@, placing it to the right 
-- of @a@ with a horizontal gap of @n@ separating the pictures.
--
cxpRightSep :: Double -> CtxPicture -> CtxPicture -> CtxPicture
cxpRightSep n = moveSnd $ \a b -> hvec $ n + (rightEdge a - leftEdge b)



-- | > cxpDownSep n a b
--
-- Vertical composition - move @b@, placing it below @a@ with a
-- vertical gap of @n@ separating the pictures.
--
cxpDownSep :: Double  -> CtxPicture -> CtxPicture -> CtxPicture
cxpDownSep n = moveSnd $ \a b -> vvec $ bottomEdge a - (topEdge b + n)


--------------------------------------------------------------------------------
-- Aligning pictures


instance Align CtxPicture where
  halign = cxpAlignH
  valign = cxpAlignV

-- | > cxpAlignH align a b
-- 
-- Horizontal composition - move @b@, placing it to the right 
-- of @a@ and align it with the top, center or bottom of @a@.
-- 
cxpAlignH :: HAlign -> CtxPicture -> CtxPicture -> CtxPicture
cxpAlignH HALIGN_TOP     = moveSnd $ \a b -> northeast a .-. northwest b
cxpAlignH HALIGN_CENTER  = moveSnd $ \a b -> east a .-. west b
cxpAlignH HALIGN_BASE    = moveSnd $ \a b -> southeast a .-. southwest b


-- | > cxpAlignV align a b
-- 
-- Vertical composition - move @b@, placing it below @a@ 
-- and align it with the left, center or right of @a@.
-- 
cxpAlignV :: VAlign -> CtxPicture -> CtxPicture -> CtxPicture
cxpAlignV VALIGN_LEFT    = moveSnd $ \a b -> southwest a .-. northwest b
cxpAlignV VALIGN_CENTER  = moveSnd $ \a b -> south a .-. north b
cxpAlignV VALIGN_RIGHT   = moveSnd $ \a b -> southeast a .-. northeast b



instance AlignSpace CtxPicture where
  halignSpace = cxpAlignSpaceH
  valignSpace = cxpAlignSpaceV

-- | > cxpAlignSpaceH align sep a b
-- 
-- Spacing version of 'cxpAlignH' - move @b@ to the right of @a@ 
-- separated by @sep@ units, align @b@ according to @align@.
-- 
cxpAlignSpaceH :: HAlign -> Double -> CtxPicture -> CtxPicture -> CtxPicture
cxpAlignSpaceH align dx = go align
  where
    mv f g           = moveSnd $ \a b -> hvec dx ^+^ (f a .-. g b)
    go HALIGN_TOP    = mv northeast northwest
    go HALIGN_CENTER = mv east west 
    go HALIGN_BASE   = mv southeast southwest


-- | > cxpAlignSpaceV align sep a b
-- 
-- Spacing version of alignV - move @b@ below @a@ 
-- separated by @sep@ units, align @b@ according to @align@.
-- 
cxpAlignSpaceV :: VAlign -> Double -> CtxPicture -> CtxPicture -> CtxPicture
cxpAlignSpaceV align dy = go align
  where
    mv f g           = moveSnd $ \a b -> vvec (-dy) ^+^ (f a .-. g b)
    go VALIGN_LEFT   = mv southwest northwest 
    go VALIGN_CENTER = mv south north  
    go VALIGN_RIGHT  = mv southeast northeast 

