{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.Bounded
-- Copyright   :  (c) Stephen Tetley 2010-2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Helpers for working with Images and LocImages that produce
-- bounding boxes.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.Bounded
  (


  -- * Type synonyms
    BoundedGraphic
  , BoundedLocGraphic
  , BoundedLocThetaGraphic

  -- * Bounding box graphic helpers
  , centerOrthoBBox


  , emptyBoundedLocGraphic
  , emptyBoundedLocThetaGraphic


  , illustrateBoundedGraphic
  , illustrateBoundedLocGraphic
  , illustrateBoundedLocThetaGraphic

  , bbrectangle

  , boundedRect

  ) where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Base.UpdateDC
import Wumpus.Basic.Kernel.Objects.Basis
import Wumpus.Basic.Kernel.Objects.DrawingPrimitives
import Wumpus.Basic.Kernel.Objects.Image
import Wumpus.Basic.Kernel.Objects.LocImage
import Wumpus.Basic.Kernel.Objects.LocThetaImage

import Wumpus.Core                              -- package: wumpus-core


--------------------------------------------------------------------------------


type BoundedGraphic u           = Image u (BoundingBox u)
type BoundedLocGraphic u        = LocImage u (BoundingBox u)
type BoundedLocThetaGraphic u   = LocThetaImage u (BoundingBox u)




-- | 'centerOrthoBBox' : @ theta * bbox -> BBox @
-- 
-- Rotate a bounding box by @theta@ about its center. Take the 
-- new bounding box.
--
-- Remember that bounding boxes are always orthonormal rectangles,
-- so the dimensions as well as the positions may change under 
-- rotation. 
--

centerOrthoBBox :: (Real u, Floating u)
                => Radian -> BoundingBox u -> BoundingBox u
centerOrthoBBox theta bb = traceBoundary $ map (rotateAbout theta ctr) ps
  where
    ctr = boundaryCenter bb
    ps  = boundaryCornerList bb



-- | Build an empty 'LocGraphic' returning a bounding box.
-- 
-- The 'emptyBoundedLocGraphic' is treated as a /null primitive/ 
-- by @Wumpus-Core@ and is not drawn, although it does generate
-- the minimum bounding box with both the bottom-left and 
-- upper-right corners at the implicit start point.
--
emptyBoundedLocGraphic :: InterpretUnit u => BoundedLocGraphic u 
emptyBoundedLocGraphic = promoteLoc $ \pt -> 
    replaceAns (BBox pt pt) $ primGraphic mempty




-- | Build an empty 'LocThetaGraphic' returning a bounding box.
-- 
-- The 'emptyBoundedLocThetaGraphic' is treated as a 
-- /null primitive/  by @Wumpus-Core@ and is not drawn, although 
-- it does generate the minimum bounding box with both the 
-- bottom-left and upper-right corners at the implicit start point 
--
emptyBoundedLocThetaGraphic :: InterpretUnit u 
                            => BoundedLocThetaGraphic u
emptyBoundedLocThetaGraphic = promoteLocTheta $ \pt _ -> 
    replaceAns (BBox pt pt) $ primGraphic mempty


--
-- NOTE - CONCATENATION
--
-- Because there is no tangible relation between the start point 
-- and answer BoundingBox, LocBoundedGraphic supports exactly the
-- same concatenation as LocImage.
--
-- PosImage and AdvGraphic are the objects that support more 
-- sophisticated concatenation.
--
 


--------------------------------------------------------------------------------
-- 


-- | Draw a BoundedGraphic, illustrating the bounding box.
--
illustrateBoundedGraphic :: InterpretUnit u
                         => Image u (BoundingBox u) -> Image u (BoundingBox u)
illustrateBoundedGraphic gf = elaborateBelow gf bbrectangle



-- | Draw a BoundedLocGraphic, illustrating the bounding box.
--
illustrateBoundedLocGraphic :: InterpretUnit u
                            => LocImage u (BoundingBox u) 
                            -> LocImage u (BoundingBox u)
illustrateBoundedLocGraphic gf = elaborateBelow gf fn
  where
    fn bb = promoteLoc $ \_ -> bbrectangle bb




-- | Draw a BoundedLocThetaGraphic, illustrating the bounding box.
--
illustrateBoundedLocThetaGraphic :: InterpretUnit u
                                 => LocThetaImage u (BoundingBox u)
                                 -> LocThetaImage u (BoundingBox u)
illustrateBoundedLocThetaGraphic gf = elaborateBelow gf fn
  where
    fn bb = promoteLocTheta $ \_ _ -> bbrectangle bb



-- | Draw a bounding box as a stroked rectangle with 
-- dotted lines.
--
bbrectangle :: InterpretUnit u => BoundingBox u -> Graphic u
bbrectangle (BBox p1@(P2 llx lly) p2@(P2 urx ury))
    | llx == urx && lly == ury = mempty `at` p1
    | otherwise                = 
        localize drawing_props $ rect1 `mappend` cross
  where
    drawing_props = cap_round . dotted_line
    rect1         = dcRectangle DRAW_STROKE (urx-llx) (ury-lly) `at` p1
    cross         = straightLine p1 p2 
                      `mappend` straightLine (P2 llx ury) (P2 urx lly)



-- | 'boundedRect' : @ style * width * height -> LocGraphic @
--
-- Create a stroked rectangle - the implicit start point is 
-- /bottom-left/, return the bounding box of the rectangle as the 
-- answer.
-- 
-- The line properties (colour, pen thickness, etc.) are taken 
-- from the implicit 'DrawingContext'.
-- 
boundedRect :: InterpretUnit u 
            => DrawMode -> u -> u -> BoundedLocGraphic u 
boundedRect style w h = promoteLoc $ \pt@(P2 x y) -> 
    let bb = BBox pt (P2 (x + w) (y + h))
    in replaceAns bb $ applyLoc (dcRectangle style w h) pt

 