{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.Orientation
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Graphic objects RectAddress and Orientation to model 
-- rectangular positioning.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.Orientation
  (


  -- * Components
    RectAddress(..)
  , Orientation(..)

  , vtoRectAddress
  , vtoOrigin
  , orientationBounds
  , orientationWidth
  , orientationHeight

  , rotateOrientation
 
  , extendOrientation
  , extendOLeft
  , extendORight
  , extendODown
  , extendOUp

  , fillHEven
  , fillXMinor
  , fillXMajor
  , fillVEven
  , fillYMajor
  , fillYMinor

  , spineRight
  , spineBelow

  , halignBottomO
  , halignCenterO
  , halignTopO
  , valignLeftO
  , valignCenterO
  , valignRightO

  , spinemoveH
  , spinemoveV
  , binmoveHBottom
  , binmoveHCenter
  , binmoveHTop
  , binmoveVLeft
  , binmoveVCenter
  , binmoveVRight

  ) where



import Wumpus.Core                              -- package: wumpus-core

import Data.VectorSpace                         -- package: vector-space

import Data.Monoid

-- | Datatype enumerating the addressable positions of a rectangle 
-- that can be derived for a 'PosObject'.  
--
-- The positions are the compass points, plus the geometric 
-- center, origin and the baseline positions: 
-- 
-- > BLL - baseline-left
--
-- > BLC - baseline-center 
-- 
-- > BLR - baseline-right
--
data RectAddress = CENTER | ORIGIN
                 | NN | SS | EE | WW | NE | NW | SE | SW 
                 | BLL | BLC | BLR
  deriving (Enum,Eq,Ord,Show)




-- | Utility datatype representing orientation within a 
-- rectangular /frame/. RectPos is useful for graphics such as 
-- text where the start point is not necessarily at the center 
-- (or bottom left).
--
-- > x_minor is the horizontal distance from the left to the start point
-- >
-- > x_major is the horizontal distance from the start point to the right
-- >
-- > y_minor is the vertical distance from the bottom to the start point
-- >
-- > y_major is the vertical distance from the start point to the top
--
-- Values should be not be negative!
--
-- 
data Orientation u = Orientation
      { or_x_minor      :: !u
      , or_x_major      :: !u
      , or_y_minor      :: !u
      , or_y_major      :: !u
      }
  deriving (Eq,Ord,Show)




--------------------------------------------------------------------------------

instance Functor Orientation where
  fmap f (Orientation xmin xmaj ymin ymaj) = 
    Orientation (f xmin) (f xmaj) (f ymin) (f ymaj)


-- | Concatenation coalesces the origins.
--
instance (Fractional u, Ord u) => Monoid (Orientation u) where
  mempty  = Orientation 0 0 0 0
  a `mappend` b = 
     Orientation { or_x_minor = max (or_x_minor a) (or_x_minor b)
                 , or_x_major = max (or_x_major a) (or_x_major b)
                 , or_y_minor = max (or_y_minor a) (or_y_minor b)
                 , or_y_major = max (or_y_major a) (or_y_major b)
                 }


-- Helper types for calculating vector from Origin 
-- (not exported).

data HDist = HCENTER | HLEFT | HRIGHT
  deriving (Eq,Ord,Show)

data VDist = VCENTER | VBASE | VTOP
  deriving (Eq,Ord,Show)



-- | The vector from a origin ro a 'RectAddress'.
--
vtoRectAddress :: (Fractional u, Ord u) 
               => Orientation u -> RectAddress -> Vec2 u
vtoRectAddress (Orientation xmin xmaj ymin ymaj) = go
  where
    hw        = 0.5  * (xmin + xmaj)
    hh        = 0.5  * (ymin + ymaj)
   
    -- CENTER, NN, SS, EE, WW all go to bottomleft then add back 
    -- the minors.

    go CENTER = V2 (hdist HCENTER) (vdist VCENTER)
    go ORIGIN = zeroVec
    go NN     = V2 (hdist HCENTER) (vdist VTOP)
    go SS     = V2 (hdist HCENTER) (vdist VBASE)
    go EE     = V2 (hdist HRIGHT)  (vdist VCENTER)
    go WW     = V2 (hdist HLEFT)   (vdist VCENTER)
    go NE     = V2 (hdist HRIGHT)  (vdist VTOP)
    go SE     = V2 (hdist HRIGHT)  (vdist VBASE)
    go SW     = V2 (hdist HLEFT)   (vdist VBASE)
    go NW     = V2 (hdist HLEFT)   (vdist VTOP)
    go BLL    = V2 (hdist HLEFT)   0
    go BLC    = V2 (hdist HCENTER) 0
    go BLR    = V2 (hdist HRIGHT)  0 

    -- > [..o..^.....]  , o -> ^
    --
    hdist HCENTER = if xmin < xmaj then hw - xmin else negate (xmin - hw)

    -- > [..o..^.....]  , o -> [
    --
    hdist HLEFT   = negate xmin
    
    -- > [..o..^.....]  , o -> ]
    --
    hdist HRIGHT  = xmaj

    vdist VCENTER = if ymin < ymaj then hh - ymin else negate (ymin - hh)
    vdist VBASE   = negate ymin
    vdist VTOP    = ymaj


vtoOrigin :: (Fractional u, Ord u) 
          => RectAddress -> Orientation u -> Vec2 u
vtoOrigin addr ortt = negateV $ vtoRectAddress ortt addr

-- | Calculate the bounding box formed by locating the 'Orientation'
-- at the supplied point.
-- 
orientationBounds :: Num u 
                  => Orientation u -> Point2 u -> BoundingBox u
orientationBounds (Orientation xmin xmaj ymin ymaj) (P2 x y) = BBox llc urc
  where
    llc   = P2 (x-xmin) (y-ymin)
    urc   = P2 (x+xmaj) (y+ymaj)


-- | Height of the orientation.
--
orientationWidth :: Num u => Orientation u -> u
orientationWidth (Orientation xmin xmaj _ _) = xmin + xmaj

-- | Height of the orientation.
--
orientationHeight :: Num u => Orientation u -> u
orientationHeight (Orientation _ _ ymin ymaj) = ymin + ymaj

--------------------------------------------------------------------------------
-- Rotation

-- | Rotate an Orientation about its origin (locus).
--
rotateOrientation :: (Real u, Floating u, Ord u) 
               => Radian -> Orientation u -> Orientation u
rotateOrientation ang (Orientation { or_x_minor = xmin
                                   , or_x_major = xmaj
                                   , or_y_minor = ymin
                                   , or_y_major = ymaj }) = 
    orthoOrientation bl br tl tr  
  where
    bl  = rotateAbout ang zeroPt $ P2 (-xmin) (-ymin)
    br  = rotateAbout ang zeroPt $ P2   xmaj  (-ymin)
    tr  = rotateAbout ang zeroPt $ P2   xmaj    ymaj
    tl  = rotateAbout ang zeroPt $ P2 (-xmin)   ymaj
  

orthoOrientation :: (Num u, Ord u)
                 => Point2 u -> Point2 u -> Point2 u -> Point2 u 
                 -> Orientation u
orthoOrientation (P2 x0 y0) (P2 x1 y1) (P2 x2 y2) (P2 x3 y3) = 
    Orientation { or_x_minor = abs $ min4 x0 x1 x2 x3
                , or_x_major = max4 x0 x1 x2 x3
                , or_y_minor = abs $ min4 y0 y1 y2 y3
                , or_y_major = max4 y0 y1 y2 y3
                }


min4 :: Ord u => u -> u -> u -> u -> u
min4 a b c d = min (min a b) (min c d)

max4 :: Ord u => u -> u -> u -> u -> u
max4 a b c d = max (max a b) (max c d)


--------------------------------------------------------------------------------
-- Extending an arm of the orientation

extendOrientation :: Num u 
                  => u -> u -> u -> u -> Orientation u -> Orientation u
extendOrientation dxl dxr dyd dyu (Orientation xmin xmaj ymin ymaj) = 
    Orientation (xmin+dxl) (xmaj+dxr) (ymin+dyd) (ymaj+dyu)

extendOLeft :: Num u => u -> Orientation u -> Orientation u
extendOLeft u (Orientation xmin xmaj ymin ymaj) = 
    Orientation (u+xmin) xmaj ymin ymaj


extendORight :: Num u => u -> Orientation u -> Orientation u
extendORight u (Orientation xmin xmaj ymin ymaj) = 
    Orientation xmin (u+xmaj) ymin ymaj

extendODown :: Num u => u -> Orientation u -> Orientation u
extendODown u (Orientation xmin xmaj ymin ymaj) = 
    Orientation xmin xmaj (u+ymin) ymaj

extendOUp :: Num u => u -> Orientation u -> Orientation u
extendOUp u (Orientation xmin xmaj ymin ymaj) = 
    Orientation xmin xmaj ymin (u+ymaj)


--------------------------------------------------------------------------------
-- Note these are fills not pads...


fillHEven :: (Fractional u, Ord u) 
          => u -> Orientation u -> Orientation u
fillHEven w ortt@(Orientation xmin xmaj _ _) = 
    if w > ow then ortt { or_x_minor = xmin + hdx
                        , or_x_major = xmaj + hdx } 
              else ortt
  where
    ow = xmin + xmaj
    hdx = 0.5 * (w - ow)


fillXMinor :: (Num u, Ord u) 
          => u -> Orientation u -> Orientation u
fillXMinor w ortt@(Orientation xmin xmaj _ _) = 
    if w > ow then ortt { or_x_minor = xmin + dx } else ortt
  where
    ow = xmin + xmaj
    dx = w - ow

fillXMajor :: (Num u, Ord u)
         => u -> Orientation u -> Orientation u
fillXMajor w ortt@(Orientation xmin xmaj _ _) = 
    if w > ow then ortt { or_x_major = xmaj + dx } else ortt
  where
    ow = xmin + xmaj
    dx = w - ow

fillVEven :: (Fractional u, Ord u) 
          => u -> Orientation u -> Orientation u
fillVEven h ortt@(Orientation _ _ ymin ymaj) = 
    if h > oh then ortt { or_y_minor = ymin + hdy
                        , or_y_major = ymaj + hdy } 
              else ortt
  where
    oh = ymin + ymaj
    hdy = 0.5 * (h - oh)

fillYMinor :: (Num u, Ord u) 
         => u -> Orientation u -> Orientation u
fillYMinor h ortt@(Orientation _ _ ymin ymaj) = 
    if h > oh then ortt { or_y_minor = ymin + dy } else ortt
  where
    oh = ymin + ymaj
    dy = h - oh


fillYMajor :: (Num u, Ord u) 
       => u -> Orientation u -> Orientation u
fillYMajor h ortt@(Orientation _ _ ymin ymaj) = 
    if h > oh then ortt { or_y_major = ymaj + dy } else ortt
  where
    oh = ymin + ymaj
    dy = h - oh


--------------------------------------------------------------------------------
-- Combining Orientation

-- Note - there are lots of concatenations (due to alignment) 
-- we need a consistent name scheme...


-- | Second Orientation is moved /to the right/ of the first along
-- the /spine/ i.e the baseline.
--
spineRight :: (Num u, Ord u) 
            => Orientation u -> Orientation u -> Orientation u
spineRight (Orientation xmin0 xmaj0 ymin0 ymaj0) 
           (Orientation xmin1 xmaj1 ymin1 ymaj1) = 
    Orientation { or_x_minor = xmin0
                , or_x_major = xmaj0 + xmin1 + xmaj1 
                , or_y_minor = max ymin0 ymin1
                , or_y_major = max ymaj0 ymaj1
                }


-- | Second Orientation is moved /below/ the first along the spine
-- i.e. the vertical point between the left minor and right major
-- (not the same as the horizontal center).
--
spineBelow :: (Num u, Ord u) 
           => Orientation u -> Orientation u -> Orientation u
spineBelow (Orientation xmin0 xmaj0 ymin0 ymaj0) 
           (Orientation xmin1 xmaj1 ymin1 ymaj1) = 
    Orientation { or_x_minor = max xmin0 xmin1
                , or_x_major = max xmaj0 xmaj1
                , or_y_minor = ymin0 + ymaj1 + ymin1
                , or_y_major = ymaj0
                }


-- | xmin and xmaj same as left.
--
halignBottomO :: (Num u, Ord u) 
            => Orientation u -> Orientation u -> Orientation u
halignBottomO (Orientation xmin0 xmaj0 ymin0 ymaj0) 
              (Orientation xmin1 xmaj1 ymin1 ymaj1) = 
    let hr = ymin1 + ymaj1
    in Orientation { or_x_minor = xmin0
                   , or_x_major = xmaj0 + xmin1 + xmaj1
                   , or_y_minor = ymin0
                   , or_y_major = max ymaj0 (hr - ymin0)
                   }





-- | xmin same as left.
--
halignCenterO :: (Fractional u, Ord u) 
              => Orientation u -> Orientation u -> Orientation u
halignCenterO (Orientation xmin0 xmaj0 ymin0 ymaj0) 
             (Orientation xmin1 xmaj1 ymin1 ymaj1) = 
    let hl         = ymin0 + ymaj0
        hr         = ymin1 + ymaj1
        half_diff  = 0.5 * (hr - hl)
    in Orientation 
          { or_x_minor = xmin0
          , or_x_major = xmaj0 + xmin1 + xmaj1
          , or_y_minor = if hl >= hr then ymin0 else (ymin0 + half_diff)
          , or_y_major = if hl >= hr then ymaj0 else (ymaj0 + half_diff)
          }



-- | xmin and ymaj same as left.
--
halignTopO :: (Num u, Ord u) 
           => Orientation u -> Orientation u -> Orientation u
halignTopO (Orientation xmin0 xmaj0 ymin0 ymaj0) 
           (Orientation xmin1 xmaj1 ymin1 ymaj1) = 
    let hr = ymin1 + ymaj1
    in Orientation { or_x_minor = xmin0
                   , or_x_major = xmaj0 + xmin1 + xmaj1
                   , or_y_minor = max ymin0 (hr - ymaj0)
                   , or_y_major = ymaj0
                   }

-- | Align second below - xmin and ymaj are same as left.
--
valignLeftO :: (Fractional u, Ord u) 
            => Orientation u -> Orientation u -> Orientation u
valignLeftO (Orientation xmin0 xmaj0 ymin0 ymaj0) 
            (Orientation xmin1 xmaj1 ymin1 ymaj1) = 
    let wr = xmin1 + xmaj1
    in Orientation { or_x_minor = xmin0
                   , or_x_major = max xmaj0 (wr - xmin0)
                   , or_y_minor = ymin0 + ymin1 + ymaj1
                   , or_y_major = ymaj0
                   }



-- | Align second below - ymaj same as left.
--
valignCenterO :: (Fractional u, Ord u) 
             => Orientation u -> Orientation u -> Orientation u
valignCenterO (Orientation xmin0 xmaj0 ymin0 ymaj0) 
              (Orientation xmin1 xmaj1 ymin1 ymaj1) = 
    let wl         = xmin0 + xmaj0
        wr         = xmin1 + xmaj1
        half_diff  = 0.5 * (wr - wl)
    in Orientation 
          { or_x_minor = if wl >= wr then xmin0 else (xmin0 + half_diff)
          , or_x_major = if wl >= wr then xmaj0 else (xmaj0 + half_diff)
          , or_y_minor = ymin0 + ymin1 + ymaj1
          , or_y_major = ymaj0 
          }


-- | Align second below - xmaj and ymaj are same as left.
--
valignRightO :: (Fractional u, Ord u) 
             => Orientation u -> Orientation u -> Orientation u
valignRightO (Orientation xmin0 xmaj0 ymin0 ymaj0) 
             (Orientation xmin1 xmaj1 ymin1 ymaj1) = 
    let wr = xmin1 + xmaj1
    in Orientation { or_x_minor = max xmin0 (wr - xmaj0)
                   , or_x_major = xmaj0 
                   , or_y_minor = ymin0 + ymin1 + ymaj1
                   , or_y_major = ymaj0 
                   }


--------------------------------------------------------------------------------
-- Binary start pos displacement

-- Note - these can be made a lot clearer...

upDown :: Num u => u -> u -> u
upDown u d = u - d

downUp :: Num u => u -> u -> u
downUp d u = negate d + u

-- | Move second right.
--
spinemoveH :: Num u => Orientation u -> Orientation u -> Vec2 u
spinemoveH op0 op1 = V2 hdist 0
  where
    hdist = or_x_major op0 + or_x_minor op1

-- | Move second below.
--
spinemoveV :: Num u => Orientation u -> Orientation u -> Vec2 u
spinemoveV op0 op1 = V2 0 (negate vdist)
  where
    vdist = or_y_minor op0 + or_y_major op1
   


binmoveHBottom :: Num u => Orientation u -> Orientation u -> Vec2 u
binmoveHBottom op0 op1 = V2 hdist vdist
  where
    hdist = or_x_major op0 + or_x_minor op1
    vdist = downUp (or_y_minor op0) (or_y_minor op1)
   

binmoveHCenter :: (Fractional u, Ord u) 
               => Orientation u -> Orientation u -> Vec2 u
binmoveHCenter (Orientation _     xmaj0 ymin0 ymaj0) 
               (Orientation xmin1 _     ymin1 ymaj1) = 
    V2 hdist vdist
  where
    h0        = ymin0 + ymaj0
    h1        = ymin1 + ymaj1
    half_diff = abs $ 0.5 * (h1 - h0)
    hdist     = xmaj0 + xmin1
    vdist     = if h0 >= h1 then downUp ymin0 (half_diff + ymin1)
                            else upDown (ymaj0 + half_diff) ymaj1



binmoveHTop :: Num u => Orientation u -> Orientation u -> Vec2 u
binmoveHTop op0 op1 = V2 hdist vdist
  where
    hdist = or_x_major op0 + or_x_minor op1
    vdist = upDown (or_y_major op0) (or_y_major op1)


leftRight :: Num u => u -> u -> u
leftRight l r = negate l + r


rightLeft :: Num u => u -> u -> u
rightLeft r l = r - l


binmoveVLeft :: Num u => Orientation u -> Orientation u -> Vec2 u
binmoveVLeft op0 op1 = V2 hdist vdist
  where
    hdist = leftRight (or_x_minor op0) (or_x_minor op1)
    vdist = negate $ or_y_minor op0 + or_y_major op1


binmoveVCenter :: (Fractional u, Ord u) 
               => Orientation u -> Orientation u -> Vec2 u
binmoveVCenter (Orientation xmin0 xmaj0 ymin0 _) 
               (Orientation xmin1 xmaj1 _     ymaj1) = 
    V2 hdist vdist
  where
    w0        = xmin0 + xmaj0
    w1        = xmin1 + xmaj1
    half_diff = abs $ 0.5 * (w1 - w0)
    hdist     = if w0 >= w1 then leftRight xmin0 (half_diff + xmin1)
                            else rightLeft (xmaj0 + half_diff) xmaj1
    vdist     = negate $ ymin0 + ymaj1



binmoveVRight :: Num u => Orientation u -> Orientation u -> Vec2 u
binmoveVRight op0 op1 = V2 hdist vdist
  where
    hdist = rightLeft (or_x_major op0) (or_x_major op1)
    vdist = negate $ or_y_minor op0 + or_y_major op1
   