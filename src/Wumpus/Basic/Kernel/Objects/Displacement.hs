{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.Displacement
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Displacing points - often start points. 
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.Displacement
  (


  -- * Displacement
    PointDisplace
  , ThetaPointDisplace
  , displace

  , dispParallel
  , dispPerpendicular
  , dispOrtho

  , dispDirectionTheta
  , dispCardinalTheta

  -- * Named vector constructors

  , go_up
  , go_down
  , go_left
  , go_right

  , go_north
  , go_south
  , go_east
  , go_west
  , go_north_east
  , go_north_west
  , go_south_east
  , go_south_west
  
  , go_up_left
  , go_up_right
  , go_down_left
  , go_down_right

  , theta_up
  , theta_down
  , theta_left
  , theta_right

  , theta_north
  , theta_south
  , theta_east
  , theta_west
  , theta_north_east
  , theta_north_west
  , theta_south_east
  , theta_south_west

  , theta_up_left
  , theta_up_right
  , theta_down_left
  , theta_down_right

  , theta_adj_grazing
  , theta_bkwd_adj_grazing


  ) where


import Wumpus.Basic.Kernel.Base.BaseDefs

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space




-- | 'PointDisplace' is a type representing functions 
-- @from Point to Point@.
--
-- It is especially useful for building composite graphics where 
-- one part of the graphic is drawn from a different start point 
-- to the other part.
--
type PointDisplace u = Point2 u -> Point2 u




-- | 'ThetaPointDisplace' is a type representing functions 
-- @from Radian * Point to Point@.
--
-- It is useful for building arrowheads which are constructed 
-- with an implicit angle representing the direction of the line 
-- at the arrow tip.
--
type ThetaPointDisplace u = Radian -> Point2 u -> Point2 u



-- | 'displace' : @ Vec2 -> PointDisplace @
--
-- Alias for @.+^@ from @Data.AffineSpace@.
--
displace :: Num u => Vec2 u -> PointDisplace u
displace (V2 dx dy) (P2 x y) = P2 (x+dx) (y+dy)








--------------------------------------------------------------------------------
-- ThetaPointDisplace functions


-- | 'dispParallel' : @ dist -> ThetaPointDisplace @
-- 
-- Build a combinator to move @Points@ in parallel to the 
-- direction of the implicit angle by the supplied distance 
-- @dist@. 
--
dispParallel :: Floating u => u -> ThetaPointDisplace u
dispParallel d = \theta pt -> pt .+^ avec (circularModulo theta) d


-- | 'dispParallel' : @ dist -> ThetaPointDisplace @
-- 
-- Build a combinator to move @Points@ perpendicular to the 
-- inclnation of the implicit angle by the supplied distance 
-- @dist@. 
--
dispPerpendicular :: Floating u => u -> ThetaPointDisplace u
dispPerpendicular d = 
    \theta pt -> pt .+^ avec (circularModulo $ theta + (0.5*pi)) d



-- | 'dispOrtho' : @ vec -> ThetaPointDisplace @
-- 
-- This is a combination of @displaceParallel@ and 
-- @displacePerpendicular@, with the x component of the vector
-- displaced in parallel and the y component displaced
-- perpendicular. 
-- 
dispOrtho :: Floating u => u -> u -> ThetaPointDisplace u
dispOrtho x y = \theta -> dispParallel x theta . dispPerpendicular y theta




-- | /Angular/ version of 'dispDirection'. 
--
-- The displacement direction is with respect to implicit angle
-- of inclination, so:
--
-- > up    == perpendicular
-- > down  == perdendicular . negate
-- > left  == parallel . negate
-- > right == parallel
-- 
dispDirectionTheta :: Floating u => Direction -> u -> ThetaPointDisplace u
dispDirectionTheta UP      = dispPerpendicular
dispDirectionTheta DOWN    = dispPerpendicular . negate
dispDirectionTheta LEFT    = dispParallel . negate
dispDirectionTheta RIGHT   = dispParallel


-- | /Angular/ version of 'dispCardinal'.
--
-- The displacement direction is with respect to implicit angle
-- of inclination, so:
--
-- > north == perpendicular
-- > east  == parallel
-- > south == perdendicular . negate
-- > etc.
-- 
dispCardinalTheta :: Floating u => Cardinal -> u -> ThetaPointDisplace u
dispCardinalTheta NORTH      = dispPerpendicular
dispCardinalTheta NORTH_EAST = \d ang -> displace (avec (ang + (0.25*pi)) d)
dispCardinalTheta EAST       = dispParallel
dispCardinalTheta SOUTH_EAST = \d ang -> displace (avec (ang + (1.75*pi)) d)
dispCardinalTheta SOUTH      = dispPerpendicular . negate
dispCardinalTheta SOUTH_WEST = \d ang -> displace (avec (ang + (1.25*pi)) d)
dispCardinalTheta WEST       = dispParallel . negate
dispCardinalTheta NORTH_WEST = \d ang -> displace (avec (ang + (0.75*pi)) d)


--------------------------------------------------------------------------------
-- Named vectors


go_up :: Num u => u -> Vec2 u
go_up d = V2 0 d

go_down :: Num u => u -> Vec2 u
go_down d = V2 0 (-d)

go_left :: Num u => u -> Vec2 u
go_left d = V2 (-d) 0

go_right :: Num u => u -> Vec2 u
go_right d = V2 d 0


go_north :: Num u => u -> Vec2 u
go_north = go_up

go_south :: Num u => u -> Vec2 u
go_south = go_down

go_east :: Num u => u -> Vec2 u
go_east = go_right

go_west :: Num u => u -> Vec2 u
go_west = go_left


go_north_east :: Floating u => u -> Vec2 u
go_north_east = avec (0.25*pi)

go_north_west :: Floating u => u -> Vec2 u
go_north_west = avec (0.75*pi)

go_south_east :: Floating u => u -> Vec2 u
go_south_east = avec (1.75*pi)

go_south_west :: Floating u => u -> Vec2 u
go_south_west = avec (1.25*pi)


go_up_left :: Num u => u -> Vec2 u
go_up_left d = V2 (-d) d

go_up_right :: u -> Vec2 u
go_up_right d = V2 d d

go_down_left :: Num u => u -> Vec2 u
go_down_left d = V2 (-d) (-d)

go_down_right :: Num u => u -> Vec2 u
go_down_right d = V2 d (-d)


--------------------------------------------------------------------------------



theta_up :: Floating u => u -> Radian -> Vec2 u
theta_up d ang = avec (ang + half_pi) d

theta_down :: Floating u => u -> Radian -> Vec2 u
theta_down d ang = avec (ang - half_pi) d


-- | Parallel (reverse)
--
theta_left :: Floating u => u -> Radian -> Vec2 u
theta_left d ang = avec (ang + pi) d

-- | Parallel (forward)
--
theta_right :: Floating u => u -> Radian -> Vec2 u
theta_right d ang = avec ang d



theta_north :: Floating u => u -> Radian -> Vec2 u
theta_north = theta_up

theta_south :: Floating u => u -> Radian -> Vec2 u
theta_south = theta_down

theta_east :: Floating u => u -> Radian -> Vec2 u
theta_east = theta_right

theta_west :: Floating u => u -> Radian -> Vec2 u
theta_west = theta_left


theta_north_east :: Floating u => u -> Radian -> Vec2 u
theta_north_east d ang = avec (ang + quarter_pi) d

theta_north_west :: Floating u => u -> Radian -> Vec2 u
theta_north_west d ang = avec (ang + 0.75*pi) d

theta_south_east :: Floating u => u -> Radian -> Vec2 u
theta_south_east d ang = avec (ang - quarter_pi) d

theta_south_west :: Floating u => u -> Radian -> Vec2 u
theta_south_west d ang = avec (ang + 1.25*pi) d



theta_up_left :: Floating u => u -> Radian -> Vec2 u
theta_up_left d = orthoVec (-d) d

theta_up_right :: Floating u => u -> Radian -> Vec2 u
theta_up_right d = orthoVec d d

theta_down_left :: Floating u => u -> Radian -> Vec2 u
theta_down_left d = orthoVec (-d) (-d)

theta_down_right :: Floating u => u -> Radian -> Vec2 u
theta_down_right d = orthoVec d (-d)




-- | Return @a-o@ when supplied length of @b-o@ and the grazing 
-- angle @boa@:
--
-- >    a
-- >    .\
-- >    . \
-- >  ..b..o
--
-- This is useful for building arrowhead vectors.
--
theta_adj_grazing :: Floating u => u -> Radian -> Radian -> Vec2 u 
theta_adj_grazing adj_len ang theta = orthoVec adj_len (-opp) theta
  where
    opp = adj_len * (fromRadian $ tan ang)


-- | Return @o-c@ when supplied length of @b-o@ and the grazing 
-- angle @boc@:
--
--
-- >  ..b..o
-- >    . /
-- >    ./
-- >    c
--
-- This is useful for building arrowhead vectors.
--
theta_bkwd_adj_grazing :: Floating u => u -> Radian -> Radian -> Vec2 u 
theta_bkwd_adj_grazing adj_len ang theta = orthoVec (-adj_len) (-opp) theta
  where
    opp = adj_len * (fromRadian $ tan ang)


