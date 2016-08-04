{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Drawing.PosObject
-- Copyright   :  (c) Stephen Tetley 2011-2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Extended Graphic object - a rectangular /positionable/ Image.
-- 
-- This graphic object has a more flexible API for positioning 
-- than other graphic objects. Rather than a LocGraphic which 
-- supports a single method of positioning at some start-point,
-- a @PosGraphic@ can be drawn at its center or locations on its 
-- outer rectangle.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Drawing.PosObject
  (

  -- * Positionable image
    GenPosObject
  , GenPosGraphic

  , PosObject
  , DPosObject

  , PosGraphic
  , DPosGraphic

  -- * Operations
  , runGenPosObject
  , evalGenPosObject
  , execGenPosObject
  
  , runPosObject

  , runPosObjectBBox

  , makePosObject
  , emptyPosObject

  , elaboratePosObject
  , decoratePosObject

  , extendPosObject
  , mapOrientation

  , illustratePosObject

  -- * Primitive text PosObjects
  , posChar
  , posEscChar 
  , posCharUpright
  , posEscCharUpright
  
  , posCharPrim

  , posText
  , posEscText 
  , posTextUpright
  , posEscTextUpright

  , posTextPrim

  , multilinePosText
  , multilinePosEscText

  , rposText
  , rposEscText
  , rposChar
  , rposEscChar
  

  , posHKernText

  , monospaceText
  , monospaceEscText

  ) where


import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Base.QueryDC
import Wumpus.Basic.Kernel.Base.UpdateDC
import Wumpus.Basic.Kernel.Base.WrappedPrimitive
import Wumpus.Basic.Kernel.Drawing.Basis
import Wumpus.Basic.Kernel.Objects.Basis
import Wumpus.Basic.Kernel.Objects.Concat
import Wumpus.Basic.Kernel.Objects.DrawingPrimitives
import Wumpus.Basic.Kernel.Objects.Image
import Wumpus.Basic.Kernel.Objects.LocImage
import Wumpus.Basic.Kernel.Objects.LocThetaImage
import Wumpus.Basic.Kernel.Objects.Orientation

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Core.Colour ( red, blue )

import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace


--
-- Note - PosObject could be in the @Object@ rather than @Drawing@
-- namespace.
--

type DOrt = Orientation Double

-- | A positionable \"Object\".
--
newtype GenPosObject st u a = GenPosObject { 
    getGenPosObject :: DrawingContext -> DPoint2 -> st -> (a, st, DOrt, CatPrim) }

type instance DUnit   (GenPosObject st u a) = u
type instance UState  (GenPosObject st u a) = st

type GenPosGraphic st u = GenPosObject st u (UNil u)


-- | Type synonym for @GenPosObject () u a@, a PosObject without
-- user state.
--
type PosObject u a = GenPosObject () u a
    
-- | Version of PosObject specialized to Double for the unit type.
--
type DPosObject a = PosObject Double a


-- | Version of PosObject with answer specialized to UNil.
--
type PosGraphic u = PosObject u (UNil u)

-- | Version of PosGraphic specialized to Double for the unit type.
--
type DPosGraphic = PosGraphic Double




instance Functor (GenPosObject st u) where
  fmap f mf = GenPosObject $ \ctx pt s -> 
              let (a,s1,o1,w1) = getGenPosObject mf ctx pt s in (f a,s1,o1,w1)


instance Applicative (GenPosObject st u) where
  pure a    = GenPosObject $ \_   _  s -> (a,s,mempty,mempty)
  mf <*> ma = GenPosObject $ \ctx pt s -> 
              let (f,s1,o1,w1) = getGenPosObject mf ctx pt s
                  (a,s2,o2,w2) = getGenPosObject ma ctx pt s1
              in (f a, s2, o1 `mappend` o2, w1 `mappend` w2)



instance Monad (GenPosObject st u) where
  return a  = GenPosObject $ \_   _  s  -> (a, s, mempty, mempty)
  mf >>= k  = GenPosObject $ \ctx pt s -> 
              let (a,s1,o1,w1) = getGenPosObject mf ctx pt s
                  (b,s2,o2,w2) = getGenPosObject (k a) ctx pt s1
              in (b, s2, o1 `mappend` o2, w1 `mappend` w2)


instance (Monoid a) => Monoid (GenPosObject st u a) where
  mempty = GenPosObject $ \_ _ s -> (mempty, s, mempty, mempty)
  ma `mappend` mb = GenPosObject $ \ctx pt s -> 
                    let (a,s1,o1,w1) = getGenPosObject ma ctx pt s
                        (b,s2,o2,w2) = getGenPosObject mb ctx pt s1
                    in (a `mappend` b, s2, o1 `mappend` o2, w1 `mappend` w2)



-- DrawingCtxM

instance DrawingCtxM (GenPosObject st u) where
  askDC           = GenPosObject $ \ctx _  s -> (ctx, s, mempty, mempty)
  asksDC fn       = GenPosObject $ \ctx _  s -> (fn ctx, s, mempty, mempty)
  localize upd ma = GenPosObject $ \ctx pt s -> 
                      getGenPosObject ma (upd ctx) pt s

instance UserStateM (GenPosObject st u) where
  getState        = GenPosObject $ \_ _ s -> (s, s, mempty, mempty)
  setState s      = GenPosObject $ \_ _ _ -> ((), s, mempty, mempty)
  updateState upd = GenPosObject $ \_ _ s -> ((), upd s, mempty, mempty)



-- | Running an PosObject produces a LocImage.
--
runGenPosObject :: InterpretUnit u 
                => RectAddress -> st -> GenPosObject st u a -> LocImage u (a,st)
runGenPosObject addr st ma = promoteLoc $ \ot -> 
    askDC >>= \ctx -> 
    let dot          = normalizeF (dc_font_size ctx) ot
        (a,s1,o1,ca) = getGenPosObject ma ctx dot st
        v1           = vtoOrigin addr o1
    in replaceAns (a,s1) $ primGraphic $ cpmove v1 ca


evalGenPosObject :: InterpretUnit u 
                 => RectAddress -> st -> GenPosObject st u a -> LocImage u a
evalGenPosObject addr st ma = fmap fst $ runGenPosObject addr st ma

execGenPosObject :: InterpretUnit u 
                 => RectAddress -> st -> GenPosObject st u a -> LocImage u st
execGenPosObject addr st ma = fmap snd $ runGenPosObject addr st ma

runPosObject :: InterpretUnit u 
             => RectAddress -> PosObject u a -> LocImage u a
runPosObject addr ma = fmap fst $ runGenPosObject addr () ma




-- | Run a PosObject producing a LocImage (BoundingBox u).
--
runPosObjectBBox :: InterpretUnit u 
                    => RectAddress -> PosObject u a -> LocImage u (BoundingBox u)
runPosObjectBBox addr ma = promoteLoc $ \pt -> 
    askDC >>= \ctx -> 
    let sz          = dc_font_size ctx 
        dpt         = normalizeF sz pt
        (_,_,o1,w1) = getGenPosObject ma ctx dpt ()
        v1          = vtoOrigin addr o1
        bb          = dinterpF sz $ orientationBounds o1 (dpt .+^ v1)
    in replaceAns bb $ primGraphic $ cpmove v1 w1




--------------------------------------------------------------------------------


-- | 'makePosObject' : @ object_pos * loc_image -> PosObject @ 
--
-- Create a 'PosObject' from an 'Orientation' describing how it
-- is orientated within a border rectangle and a 'LocImage' that 
-- draws it.
--
-- This is the /primary/ constructor for PosObjects. Because the
-- PosObject type is considered as a specialized object it does
-- not have the range of functions of LocImage or LocThetaImage.
-- 
makePosObject :: InterpretUnit u
              => Query u (Orientation u) -> LocImage u a -> GenPosObject st u a
makePosObject ma gf = GenPosObject $ \ctx pt s -> 
    let ort1  = runQuery ctx ma
        dort1 = normalizeF (dc_font_size ctx) ort1
        upt   = dinterpF (dc_font_size ctx) pt
        (a,w) = runLocImage ctx upt gf
    in (a,s,dort1,w)


-- | 'emptyPosObject' : @ PosObject @
--
-- Build an empty 'PosGraphicObject'.
--
emptyPosObject :: Monoid a => GenPosObject st u a
emptyPosObject = mempty

    

--
-- decorate  - oblivious to /answer/.
-- elaborate - derives annotation from the /answer/ and makes a 
--             cumulative graphic.
--


elaboratePosObject :: (Fractional u, Ord u, InterpretUnit u)
                   => ZOrder -> RectAddress -> LocGraphic u 
                   -> GenPosObject st u a
                   -> GenPosObject st u a
elaboratePosObject zo raddr gf ma = decoratePosObject zo fn ma
  where
    fn ortt = moveStart (vtoRectAddress ortt raddr) gf



decoratePosObject :: InterpretUnit u 
                  => ZOrder -> (Orientation u -> LocGraphic u) 
                  -> GenPosObject st u a
                  -> GenPosObject st u a
decoratePosObject zo fn ma = GenPosObject $ \ctx pt s -> 
    let (a,s1,o1,w1) = getGenPosObject ma ctx pt s
        uortt        = dinterpF (dc_font_size ctx) o1
        upt          = dinterpF (dc_font_size ctx) pt
        (_,w2)       = runLocImage ctx upt $ fn uortt
        wout         = case zo of
                         ZABOVE -> w1 `mappend` w2
                         ZBELOW -> w2 `mappend` w1
    in (a,s1,o1,wout)




-- | Extend the orientation.
--
extendPosObject :: InterpretUnit u 
                => u -> u -> u -> u -> GenPosObject st u a 
                -> GenPosObject st u a
extendPosObject x0 x1 y0 y1 ma = GenPosObject $ \ctx pt s ->
    let (a,s1,o1,w1) = getGenPosObject ma ctx pt s
        sz           = dc_font_size ctx        
        ux0          = normalize sz x0
        ux1          = normalize sz x1
        uy0          = normalize sz y0
        uy1          = normalize sz y1
        o2           = extendOrientation ux0 ux1 uy0 uy1 o1
    in (a,s1,o2,w1)


-- | Note - this is a bad API, it would be better to have padders
-- and fillers and not expose the orientation directly.
-- 
mapOrientation :: InterpretUnit u
               => (Orientation u -> Orientation u) 
               -> GenPosObject st u a -> GenPosObject st u a
mapOrientation fn mf = GenPosObject $ \ctx pt s -> 
    let (a,s1,o1,w1) = getGenPosObject mf ctx pt s
        uort      = fn $ dinterpF (dc_font_size ctx) o1
        o2        = normalizeF (dc_font_size ctx) uort
    in (a,s1,o2,w1)


--------------------------------------------------------------------------------


-- | Illustrate a 'PosObject' by super-imposing its 'Orientation'.
--
-- This turns the 'PosObject' into a 'LocImage' drawn at the locus
-- of the PosObject.
--
illustratePosObject :: InterpretUnit u 
                    => PosObject u a -> LocGraphic u
illustratePosObject mf  = promoteLoc $ \pt ->   
    askDC >>= \ctx ->
    let dpt         = normalizeF (dc_font_size ctx) pt 
        (_,_,o1,w1) = getGenPosObject mf ctx dpt ()
        uort        = dinterpF (dc_font_size ctx) o1
    in decorateBelow (primGraphic w1) (illustrateOrientation uort `at` pt)


illustrateOrientation :: InterpretUnit u 
                      => Orientation u -> LocGraphic u
illustrateOrientation (Orientation xmin xmaj ymin ymaj) = promoteLoc $ \pt -> 
    dinterpCtx 3 >>= \radius -> 
    let upd = localize (fill_colour blue . dotted_line)
        bl  = pt .-^ V2 xmin ymin
        dot = localize (fill_colour red) $ dcDisk DRAW_FILL radius `at` pt
        hln = upd $ locStraightLine (hvec $ xmin+xmaj) `at` pt .-^ hvec xmin
        vln = upd $ locStraightLine (vvec $ ymin+ymaj) `at` pt .-^ vvec ymin
        bdr = upd $ dcRectangle DRAW_STROKE (xmin+xmaj) (ymin+ymaj) `at` bl
    in mconcat [ bdr, hln, vln, dot ]



--------------------------------------------------------------------------------
-- Char PosObjects


-- Note - because the TextHeight constructors are so long winded,
-- using them directly makes for a bad API. Instead we have two 
-- versions for each function.

-- | Note - no margins are added to the containing rectangle.
-- 
-- To get a Char with margins, use 'posText' instead:
--
-- > posText ['1']
-- 
posChar             :: InterpretUnit u 
                    => Char -> GenPosGraphic st u
posChar             = makeCharPO CAP_HEIGHT_PLUS_DESCENDER . CharLiteral

posEscChar          :: InterpretUnit u 
                    => EscapedChar -> GenPosGraphic st u
posEscChar          = makeCharPO CAP_HEIGHT_PLUS_DESCENDER

posCharUpright      :: InterpretUnit u 
                    => Char -> GenPosGraphic st u
posCharUpright      = makeCharPO JUST_CAP_HEIGHT . CharLiteral

posEscCharUpright   :: InterpretUnit u 
                    => EscapedChar -> GenPosGraphic st u
posEscCharUpright   = makeCharPO JUST_CAP_HEIGHT


-- | Primtive builder that does not add margins.
--
posCharPrim         :: InterpretUnit u 
                    => Either Char EscapedChar -> GenPosGraphic st u
posCharPrim = makeCharPO CAP_HEIGHT_PLUS_DESCENDER . either CharLiteral id


makeCharPO :: InterpretUnit u 
           => TextHeight -> EscapedChar -> GenPosGraphic st u
makeCharPO hspec esc = 
    makePosObject (charOrientation hspec esc) 
                  (dcEscapedlabel $ wrapEscChar esc)




-- | Build the Orientation of an EscapedChar.
-- 
-- The locus of the Orientation is baseline left - margins are 
-- added.
--
charOrientation :: (DrawingCtxM m, InterpretUnit u)
                => TextHeight -> EscapedChar -> m (Orientation u)
charOrientation hspec esc = 
    (\(V2 x _ ) (ymin,ymaj) -> Orientation 0 x ymin ymaj) 
      <$> escCharVector esc <*> heightSpan hspec


--------------------------------------------------------------------------------
-- Text PosObjects



posText     :: InterpretUnit u 
            => String -> GenPosGraphic st u
posText     = addMargins . makeTextPO CAP_HEIGHT_PLUS_DESCENDER . escapeString

posEscText  :: InterpretUnit u 
            => EscapedText -> GenPosGraphic st u
posEscText  = addMargins . makeTextPO CAP_HEIGHT_PLUS_DESCENDER


posTextUpright      :: InterpretUnit u 
                    => String -> GenPosGraphic st u
posTextUpright      = addMargins . makeTextPO JUST_CAP_HEIGHT . escapeString

posEscTextUpright   :: InterpretUnit u 
                    => EscapedText -> GenPosGraphic st u
posEscTextUpright   = addMargins . makeTextPO JUST_CAP_HEIGHT

-- | Primtive builder that does not add margins.
--
posTextPrim         :: InterpretUnit u 
                    => Either String EscapedText -> GenPosGraphic st u
posTextPrim = makeTextPO CAP_HEIGHT_PLUS_DESCENDER . either escapeString id


multilinePosText :: (InterpretUnit u)
                 => VAlign -> String -> PosGraphic u
multilinePosText vspec xs = 
    multilinePosEscText vspec $ map escapeString $ lines xs

multilinePosEscText :: (InterpretUnit u)
                    => VAlign -> [EscapedText] -> GenPosGraphic st u
multilinePosEscText vspec xs = addMargins $ GenPosObject $ \ctx pt s -> 
      let sep    = runQuery ctx textlineSpace
      in getGenPosObject (body sep) ctx pt s
  where
    body sp = alignColumnSep vspec sp $ 
                map (makeTextPO CAP_HEIGHT_PLUS_DESCENDER) xs



-- | Note - this does not add margins.
--
makeTextPO :: InterpretUnit u 
           => TextHeight -> EscapedText -> GenPosGraphic st u
makeTextPO hspec esc = 
    makePosObject (textOrientationZero hspec esc) (dcEscapedlabel esc)


addMargins :: InterpretUnit u => GenPosObject st u a -> GenPosObject st u a
addMargins ma = 
   textMargin >>= \(xsep,ysep) -> extendPosObject xsep xsep ysep ysep ma

-- | Build the Orientation of a single line of EscapedText - 
-- writing direction zero (left-to-right).
-- 
-- The locus of the Orientation is baseline left - margins are 
-- added.
--
textOrientationZero :: (DrawingCtxM m, InterpretUnit u )
                    => TextHeight -> EscapedText -> m (Orientation u)
textOrientationZero hspec esc = 
    (\(V2 x _ ) (ymin,ymaj) -> Orientation 0 x ymin ymaj) 
      <$> escTextVector esc <*> heightSpan hspec


--------------------------------------------------------------------------------
-- Rotated text

-- | Note - for single line text.
--
rposText        :: (Real u, Floating u, InterpretUnit u) 
                => Radian -> String -> GenPosGraphic st u
rposText ang    = addMargins . makeRotatedPO ang . escapeString

-- | Note - for single line text.
--
rposEscText     :: (Real u, Floating u, InterpretUnit u) 
                => Radian -> EscapedText -> GenPosGraphic st u
rposEscText ang = addMargins . makeRotatedPO ang


rposChar        :: (Real u, Floating u, InterpretUnit u) 
                => Radian -> Char -> GenPosGraphic st u
rposChar ang ch = rposEscText ang $ wrapEscChar $ CharLiteral ch

rposEscChar     :: (Real u, Floating u, InterpretUnit u) 
                => Radian -> EscapedChar -> GenPosGraphic st u
rposEscChar ang ch = rposEscText ang $ wrapEscChar ch




makeRotatedPO :: (Real u, Floating u, InterpretUnit u) 
              => Radian -> EscapedText -> GenPosGraphic st u
makeRotatedPO ang esc = makePosObject qry body
  where
    qry  = rotateOrientation ang <$> 
             textOrientationZero CAP_HEIGHT_PLUS_DESCENDER esc

    body = incline (dcREscapedlabel esc) ang


--------------------------------------------------------------------------------
-- Kerned text


posHKernText :: InterpretUnit u
            => [KernChar u] -> GenPosGraphic st u
posHKernText xs = makePosObject (hkernOrientationZero xs) (hkernLine xs)

-- | The query should retrieve the width of one char.
--
monospaceText :: InterpretUnit u 
              => Query u u -> String -> GenPosGraphic st u
monospaceText qry = monospaceEscText qry . escapeString


-- | The query should retrieve the width of one char.
--
monospaceEscText :: InterpretUnit u 
                 => Query u u -> EscapedText -> GenPosGraphic st u
monospaceEscText qry esc = GenPosObject $ \ctx pt s ->
    let upt    = dinterpF (dc_font_size ctx) pt
        uw     = runQuery ctx qry
        ks     = monos uw $ destrEscapedText id esc
        ortt   = runQuery ctx $ hkernOrientationZero ks
        dort   = normalizeF (dc_font_size ctx) ortt
        (_,w1) = runLocImage ctx upt $ hkernLine ks
    in (UNil, s, dort, w1)





monos :: Num u => u -> [EscapedChar] -> [KernChar u]
monos w1 (c:cs) = (0,c) : map (\ch -> (w1,ch)) cs
monos _  []     = []



-- | Note - always CAP_HEIGHT_PLUS_DESCENDER for this one.
--
hkernOrientationZero :: (DrawingCtxM m, InterpretUnit u )
                     => [KernChar u] -> m (Orientation u)
hkernOrientationZero xs = 
    (\(V2 x _ ) (ymin,ymaj) -> Orientation 0 x ymin ymaj) 
      <$> hkernVector xs <*> heightSpan CAP_HEIGHT_PLUS_DESCENDER

 

--------------------------------------------------------------------------------
-- Combining PosObject


instance (Monoid a) => ZConcat (GenPosObject st u a) where
  superior = mappend
  anterior = flip mappend


instance Monoid a => Concat (GenPosObject st u a) where
  hconcat = genMoveAlign spinemoveH spineRight
  vconcat = genMoveAlign spinemoveV spineBelow

instance (Monoid a, InterpretUnit u) => CatSpace (GenPosObject st u a) where
  hspace = genMoveSepH spinemoveH spineRight
  vspace = genMoveSepV spinemoveV spineBelow



instance Monoid a => Align (GenPosObject st u a) where
  halign HALIGN_TOP    = genMoveAlign binmoveHTop    halignTopO
  halign HALIGN_CENTER = genMoveAlign binmoveHCenter halignCenterO
  halign HALIGN_BASE   = genMoveAlign binmoveHBottom halignBottomO

  valign VALIGN_LEFT   = genMoveAlign binmoveVLeft   valignLeftO
  valign VALIGN_CENTER = genMoveAlign binmoveVCenter valignCenterO
  valign VALIGN_RIGHT  = genMoveAlign binmoveVRight  valignRightO



genMoveAlign :: Monoid a
             => (Orientation Double -> Orientation Double -> Vec2 Double) 
             -> (Orientation Double -> Orientation Double -> Orientation Double) 
             -> GenPosObject st u a -> GenPosObject st u a -> GenPosObject st u a
genMoveAlign mkV mkO ma mb = GenPosObject $ \ctx pt s -> 
    let (a,s1,o1,w1) = getGenPosObject ma ctx pt s
        (b,s2,o2,w2) = getGenPosObject mb ctx pt s1
        v1           = mkV o1 o2
        ortt         = mkO o1 o2
        w2'          = cpmove v1 w2 
    in (a `mappend` b, s2, ortt, w1 `mappend` w2')


--------------------------------------------------------------------------------
-- Sep

instance (Monoid a, InterpretUnit u) => AlignSpace (GenPosObject st u a) where
  halignSpace HALIGN_TOP    = genMoveSepH binmoveHTop    halignTopO
  halignSpace HALIGN_CENTER = genMoveSepH binmoveHCenter halignCenterO
  halignSpace HALIGN_BASE   = genMoveSepH binmoveHBottom halignBottomO

  valignSpace VALIGN_LEFT   = genMoveSepV binmoveVLeft   valignLeftO
  valignSpace VALIGN_CENTER = genMoveSepV binmoveVCenter valignCenterO
  valignSpace VALIGN_RIGHT  = genMoveSepV binmoveVRight  valignRightO


genMoveSepH :: (Monoid a, InterpretUnit u) 
            => (Orientation Double -> Orientation Double -> Vec2 Double) 
            -> (Orientation Double -> Orientation Double -> Orientation Double) 
            -> u
            -> GenPosObject st u a -> GenPosObject st u a 
            -> GenPosObject st u a
genMoveSepH mkV mkO sep ma mb  = GenPosObject $ \ctx pt s -> 
    let (a,s1,o1,w1) = getGenPosObject ma ctx pt s
        (b,s2,o2,w2) = getGenPosObject mb ctx pt s1
        dsep         = normalize (dc_font_size ctx) sep
        v1           = hvec dsep ^+^ mkV o1 o2
        ortt         = extendORight dsep $ mkO o1 o2
        w2'          = cpmove v1 w2
    in (a `mappend` b, s2, ortt, w1 `mappend` w2')



genMoveSepV :: (Monoid a, InterpretUnit u)
            => (Orientation Double -> Orientation Double -> Vec2 Double) 
            -> (Orientation Double -> Orientation Double -> Orientation Double) 
            -> u
            -> GenPosObject st u a -> GenPosObject st u a 
            -> GenPosObject st u a
genMoveSepV mkV mkO sep ma mb = GenPosObject $ \ctx pt s -> 
    let (a,s1,o1,w1) = getGenPosObject ma ctx pt s 
        (b,s2,o2,w2) = getGenPosObject mb ctx pt s1
        dsep         = normalize (dc_font_size ctx) sep
        v1           = vvec (-dsep) ^+^ mkV o1 o2
        ortt         = extendODown dsep $ mkO o1 o2
        w2'          = cpmove v1 w2
    in (a `mappend` b, s2, ortt, w1 `mappend` w2')

