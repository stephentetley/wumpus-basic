{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.Anchors
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Anchor points on shapes, bounding boxes, etc.
--
-- Anchors are addressable positions, an examplary use is taking
-- anchors on node shapes to get the in-bound and out-bound points 
-- for connectors in a network (graph) diagram.
-- 
-- \*\* WARNING \*\* - the API here probably needs some more 
-- thought.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.Anchors
  ( 

  -- * Anchors
    Anchor

  -- * Anchor classes
  , CenterAnchor(..)
  , ApexAnchor(..)
  , CardinalAnchor(..)
  , CardinalAnchor2(..)
  , RadialAnchor(..)
  , TopCornerAnchor(..)
  , BottomCornerAnchor(..)
  , SideMidpointAnchor(..)


  -- * Extended anchor points
  , projectAnchor

  , radialConnectorPoints

  ) where


import Wumpus.Core                      -- package: wumpus-core

import Data.AffineSpace                 -- package: vector-space


-- | Note an Anchor is just a Point2.
--
type Anchor u = Point2 u


-- | Center of an object.
--
class CenterAnchor a where
  center :: u ~ DUnit a => a -> Anchor u


-- | Apex of an object.
--
class ApexAnchor a where
  apex :: u ~ DUnit a => a -> Anchor u


-- | Cardinal (compass) positions on an object. 
-- 
-- Cardinal anchors should be at their equivalent radial position.
-- However, some shapes may not be able to easily define radial 
-- positions or may be able to provide more efficient definitions 
-- for the cardinal anchors. Hence the redundancy seems justified. 
--
class CardinalAnchor a where
  north :: u ~ DUnit a => a -> Anchor u
  south :: u ~ DUnit a => a -> Anchor u
  east  :: u ~ DUnit a => a -> Anchor u
  west  :: u ~ DUnit a => a -> Anchor u

--
-- Note - a design change is probably in order where the cardinals 
-- should /always/ represent their true cardinal position.
--
-- If this change is made, it is worthwhile having cardinals as
-- classes (rather than making them derived operations on 
-- RadialAnchor) as classes allow for more efficient 
-- implementations usually by trigonometry.
-- 


-- | Secondary group of cardinal (compass) positions on an object
-- for the diagonal positions. 
-- 
-- It seems possible that for some objects defining the primary
-- compass points (north, south,...) will be straight-forward 
-- whereas defining the secondary compass points may be 
-- problematic, hence the compass points are split into two 
-- classes.
--
class CardinalAnchor2 a where
  northeast :: u ~ DUnit a => a -> Anchor u
  southeast :: u ~ DUnit a => a -> Anchor u
  southwest :: u ~ DUnit a => a -> Anchor u
  northwest :: u ~ DUnit a => a -> Anchor u


-- | Anchor on a border that can be addressed by an angle.
--
-- The angle is counter-clockwise from the right-horizontal, i.e.
-- 0 is /east/.
--
class RadialAnchor a where
  radialAnchor :: Radian -> u ~ DUnit a => a -> Anchor u


-- | Anchors at the top left and right corners of a shape.
--
-- For some shapes (Rectangle) the TikZ convention appears to be
-- have cardinals as the corner anchors, but this doesn\'t seem
-- to be uniform. Wumpus will need to reconsider anchors at some 
-- point...
--
class TopCornerAnchor a where
  topLeftCorner  :: u ~ DUnit a => a -> Anchor u
  topRightCorner :: u ~ DUnit a => a -> Anchor u


-- | Anchors at the bottom left and right corners of a shape.
--
class BottomCornerAnchor a where
  bottomLeftCorner  :: u ~ DUnit a => a -> Anchor u
  bottomRightCorner :: u ~ DUnit a => a -> Anchor u


-- | Anchors in the center of a side.
-- 
-- Sides are addressable by index. Following TikZ, side 1 is 
-- expected to be the top of the shape. If the shape has an apex 
-- instead of a side then side 1 is expected to be the first side 
-- left of the apex.
-- 
-- Implementations are also expected to modulo the side number, 
-- rather than throw an out-of-bounds error.
--
class SideMidpointAnchor a where
  sideMidpoint :: Int -> u ~ DUnit a => a -> Anchor u



--------------------------------------------------------------------------------

-- | 'projectAnchor' : @ extract_func * dist * object -> Point @
-- 
-- Derive a anchor by projecting a line from the center of an 
-- object through the intermediate anchor (produced by the 
-- extraction function). The final answer point is located along
-- the projected line at the supplied distance @dist@.
-- 
-- E.g. take the north of a rectangle and project it 10 units 
-- further on:
--  
-- > projectAnchor north 10 my_rect
--
-- If the distance is zero the answer with be whatever point the 
-- the extraction function produces.
--
-- If the distance is negative the answer will be along the 
-- projection line, between the center and the intermediate anchor.
--
-- If the distance is positive the anchor will be extend outwards 
-- from the intermediate anchor.
--
projectAnchor :: (Real u, Floating u, CenterAnchor a, u ~ DUnit a) 
              => (a -> Anchor u) -> u -> a -> Anchor u
projectAnchor fn d a = p1 .+^ (avec (vdirection v) d)
  where
    p1  = fn a 
    v   = pvec (center a) p1 
     


--------------------------------------------------------------------------------

-- | 'radialConnectorPoints' : @ object_a * object_b -> (Point_a, Point_b) @
--
-- Find the radial connectors points for objects @a@ and @b@ along
-- the line joining their centers.
--
radialConnectorPoints :: ( Real u, Floating u
                         , CenterAnchor a, RadialAnchor a
                         , CenterAnchor b, RadialAnchor b
                         , u ~ DUnit a, u ~ DUnit b) 
                      => a -> b -> (Point2 u, Point2 u) 
radialConnectorPoints a b = (radialAnchor ang a, radialAnchor (ang+pi) b)
  where
    ang = vdirection $ pvec (center a) (center b)
     
    

--------------------------------------------------------------------------------
-- Instances 

instance Fractional u => CenterAnchor (BoundingBox u) where
  center (BBox (P2 xl ylo) (P2 xr yhi)) = P2 x y 
     where
       x = xl+0.5*(xr-xl)
       y = ylo+0.5*(yhi-ylo)
       

instance Fractional u => CardinalAnchor (BoundingBox u) where
  north (BBox (P2 xl _  ) (P2 xr yhi)) = P2 (xl+0.5*(xr-xl)) yhi
  south (BBox (P2 xl ylo) (P2 xr _  )) = P2 (xl+0.5*(xr-xl)) ylo
  east  (BBox (P2 _  ylo) (P2 xr yhi)) = P2 xr (ylo+0.5*(yhi-ylo))
  west  (BBox (P2 xl ylo) (P2 _  yhi)) = P2 xl (ylo+0.5*(yhi-ylo))


instance CardinalAnchor2 (BoundingBox u) where
  northeast (BBox _ ur)                 = ur
  southeast (BBox (P2 _ ylo) (P2 xr _)) = P2 xr ylo
  southwest (BBox ll _)                 = ll
  northwest (BBox (P2 xl _) (P2 _ yhi)) = P2 xl yhi 

