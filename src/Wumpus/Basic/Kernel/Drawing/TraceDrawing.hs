{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Drawing.TraceDrawing
-- Copyright   :  (c) Stephen Tetley 2010-2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- Drawing with /trace/ - a Writer like monad collecting 
-- intermediate graphics - and /drawing context/ - a reader monad
-- of attributes - font_face, fill_colour etc.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Drawing.TraceDrawing
  (

  -- * Collect primitives (writer-like monad) 
    GenTraceDrawing
  , TraceDrawing
  , DTraceDrawing

  , runTraceDrawing
  , execTraceDrawing
  , evalTraceDrawing

  , runGenTraceDrawing


  , liftToPictureU
  , liftToPictureMb
  , mbPictureU
 
  , trace
  , fontDelta
  , evalQuery

  , draw
  , drawi
  , drawl
  , drawli

  , drawc
  , drawci

  , node
  , nodei
 
  , drawrc
  , drawrci

  ) where


import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Base.QueryDC
import Wumpus.Basic.Kernel.Base.WrappedPrimitive
import Wumpus.Basic.Kernel.Drawing.Basis
import Wumpus.Basic.Kernel.Objects.Anchors
import Wumpus.Basic.Kernel.Objects.Connector
import Wumpus.Basic.Kernel.Objects.Image
import Wumpus.Basic.Kernel.Objects.LocImage

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative
import Data.Monoid


--------------------------------------------------------------------------------




-- Note - TraceDrawing run \once\ - it is supplied with the starting
-- environment (DrawingContext) and returns a Picture.
--
-- Other Wumpus monads (e.g. Turtle) will typically be run inside
-- the TraceDrawing monad as a local effect, rather than built into a 
-- transformer stack.
--


newtype GenTraceDrawing st u a   = GenTraceDrawing { 
          getGenTraceDrawing :: DrawingContext -> st -> (a, st, HPrim u) }


type instance DUnit   (GenTraceDrawing st u a) = u
type instance UState  (GenTraceDrawing st u a) = st

type TraceDrawing u a = GenTraceDrawing () u a

type DTraceDrawing a    = TraceDrawing Double a


-- Functor

instance Functor (GenTraceDrawing st u) where
  fmap f ma = GenTraceDrawing $ \ctx s -> 
                let (a,s1,w1) = getGenTraceDrawing ma ctx s in (f a,s1,w1)


-- Applicative

instance Applicative (GenTraceDrawing st u) where
  pure a    = GenTraceDrawing $ \_   s -> (a, s, mempty)
  mf <*> ma = GenTraceDrawing $ \ctx s -> 
                let (f,s1,w1) = getGenTraceDrawing mf ctx s
                    (a,s2,w2) = getGenTraceDrawing ma ctx s1
                in (f a, s2, w1 `mappend` w2)


-- Monad

instance Monad (GenTraceDrawing st u) where
  return a  = GenTraceDrawing $ \_   s -> (a, s, mempty)
  ma >>= k  = GenTraceDrawing $ \ctx s -> 
                let (a,s1,w1) = getGenTraceDrawing ma ctx s
                    (b,s2,w2) = (getGenTraceDrawing . k) a ctx s1
                in (b,s2,w1 `mappend` w2)
                               

-- DrawingCtxM

instance DrawingCtxM (GenTraceDrawing st u) where
  askDC           = GenTraceDrawing $ \ctx s -> (ctx, s, mempty)
  asksDC f        = GenTraceDrawing $ \ctx s -> (f ctx, s, mempty)
  localize upd ma = GenTraceDrawing $ \ctx s -> 
                      getGenTraceDrawing ma (upd ctx) s


-- UserStateM 

instance UserStateM (GenTraceDrawing st u) where
  getState        = GenTraceDrawing $ \_ s -> (s, s, mempty)
  setState s      = GenTraceDrawing $ \_ _ -> ((), s, mempty)
  updateState upd = GenTraceDrawing $ \_ s -> ((), upd s, mempty)
 



runTraceDrawing :: DrawingContext -> TraceDrawing u a -> (a, HPrim u)
runTraceDrawing ctx ma = post $ getGenTraceDrawing ma ctx ()
  where
    post (a,_,w1) = (a,w1)



-- | Run the drawing returning only the output it produces, drop
-- any answer from the monadic computation.
--
execTraceDrawing :: DrawingContext -> TraceDrawing u a -> HPrim u
execTraceDrawing ctx ma = snd $ runTraceDrawing ctx ma

-- | Run the drawing ignoring the output it produces, return the 
-- answer from the monadic computation.
--
-- Note - this useful for testing, generally one would want the 
-- opposite behaviour (return the drawing, ignore than the 
-- answer).
-- 
evalTraceDrawing :: DrawingContext -> TraceDrawing u a -> a
evalTraceDrawing ctx ma = fst $ runTraceDrawing ctx ma


runGenTraceDrawing :: DrawingContext -> st -> GenTraceDrawing st u a 
                   -> (a,st,HPrim u)
runGenTraceDrawing ctx st ma = getGenTraceDrawing ma ctx st





-- | /Unsafe/ promotion of @HPrim@ to @Picture@.
--
-- If the HPrim is empty, a run-time error is thrown.
-- 
liftToPictureU :: HPrim u -> Picture
liftToPictureU hf = 
    let prims = hprimToList hf in if null prims then errK else frame prims
  where
    errK = error "toPictureU - empty prims list."

-- | /Safe/ promotion of @HPrim@ to @(Maybe Picture)@.
--
-- If the HPrim is empty, then @Nothing@ is returned.
-- 
liftToPictureMb :: HPrim u -> Maybe Picture
liftToPictureMb hf = let prims = hprimToList hf in 
    if null prims then Nothing else Just (frame prims)



-- | /Unsafe/ promotion of @(Maybe Picture)@ to @Picture@.
--
-- This is equivalent to:
--
-- > fromMaybe (error "empty") $ pic
--
-- This function is solely a convenience, using it saves one 
-- import and a few characters.
--
-- If the supplied value is @Nothing@ a run-time error is thrown.
-- 
mbPictureU :: Maybe Picture -> Picture
mbPictureU Nothing  = error "mbPictureU - empty picture."
mbPictureU (Just a) = a

-- Note - need an equivalent to Parsec\`s parseTest that provides
-- a very simple way to run graphics without concern for return 
-- type or initial drawing context.



--------------------------------------------------------------------------------



-- TraceM 
--
-- Note -  @ state `mappend` a @ means the first expression in a 
-- monadic drawing is the first element in the output file. It is
-- also \*\* at the back \*\* in the the Z-Order.
--
-- Some control over the Z-Order, possibly by adding /layers/ to 
-- the drawing model would be valuable. 
-- 

-- | Primitive operation - cf. tell in Reader monad.
--
trace     :: HPrim u -> GenTraceDrawing st u ()
trace a = GenTraceDrawing $ \_ s -> ((), s, a)



fontDelta :: GenTraceDrawing st u a -> GenTraceDrawing st u a
fontDelta mf = GenTraceDrawing $ \ctx s -> 
    let (_,font_attrs) = runQuery ctx textAttr
        (a,s1,w1)      = getGenTraceDrawing mf ctx s
        prim           = fontDeltaContext font_attrs $ primGroup $ hprimToList w1
    in (a, s1, singleH $ prim1 $ prim)

-- Note - this function is in the wrong module....
--
evalQuery :: DrawingCtxM m => Query u a -> m a
evalQuery df = askDC >>= \ctx -> return $ runQuery ctx df



-- | Draw a Graphic taking the drawing style from the 
-- /drawing context/. 
--
-- This function is the /forgetful/ version of 'drawi'. 
-- Commonly, it is used to draw 'Graphic' objects which 
-- have no /answer/.
-- 
draw :: Image u a -> GenTraceDrawing st u ()
draw gf = askDC >>= \ctx -> 
          let (_,w) = runImage ctx gf
          in trace (singleH w) >> return ()




-- | Draw an Image taking the drawing style from the 
-- /drawing context/. 
--
-- The graphic representation of the Image is drawn in the Trace 
-- monad, and the result is returned.
-- 
drawi :: Image u a -> GenTraceDrawing st u a
drawi gf = askDC >>= \ctx -> 
           let (a,w) = runImage ctx gf
           in trace (singleH w) >> return a
            


-- | Draw a LocImage at the supplied Anchor taking the drawing 
-- style from the /drawing context/. 
--
-- This function is the /forgetful/ version of 'drawli'. 
-- Commonly, it is used to draw 'LocGraphic' objects which 
-- have no /answer/.
-- 
drawl :: InterpretUnit u
      => Anchor u -> LocImage u a -> GenTraceDrawing st u ()
drawl ancr img = drawli ancr img >> return ()



-- | Draw a LocImage at the supplied Point taking the drawing 
-- style from the /drawing context/. 
--
-- The graphic representation of the Image is drawn in the Trace 
-- monad, and the result is returned.
-- 
drawli :: InterpretUnit u
       => Anchor u -> LocImage u a -> GenTraceDrawing st u a
drawli pt gf = askDC >>= \ctx -> 
               let (a,w) = runLocImage ctx pt gf
               in trace (singleH w) >> return a


-- Design note - having @drawlti@ for LocThetaImage does not seem 
-- compelling (at the moment). The thinking is that LocTheta
-- objects should be downcast to Loc objects before drawing. 
--
-- Connectors however are be different. 
-- 
-- PosImages would seem to be the same as LocThetaImages.
--



-- | Draw a ConnectorGraphic with the supplied Anchors taking the 
-- drawing style from the /drawing context/. 
--
-- This function is the /forgetful/ version of 'drawci'. 
-- Commonly, it is used to draw 'ConnectorGraphic' objects which 
-- have no /answer/.
-- 
drawc :: InterpretUnit u
      => Anchor u -> Anchor u -> ConnectorImage u a -> GenTraceDrawing st u ()
drawc an0 an1 gf = drawci an0 an1 gf >> return () 


-- | Draw a ConnectorImage with the supplied Points taking the 
-- drawing style from the /drawing context/. 
--
-- The graphic representation of the Image is drawn in the Trace 
-- monad, and the result is returned.
-- 
drawci :: InterpretUnit u 
       => Anchor u -> Anchor u -> ConnectorImage u a -> GenTraceDrawing st u a
drawci p0 p1 gf = drawi (connect gf p0 p1)








-- | Draw the object with the supplied grid coordinate. The 
-- actual position is scaled according to the 
-- @snap_grid_factors@ in the /drawing context/.
-- 
-- This function is the /forgetful/ version of 'nodei'. 
-- Commonly, it is used to draw 'LocGraphic' objects which 
-- have no /answer/.
-- 
node :: ( Fractional u, InterpretUnit u)
     => (Int,Int) -> LocImage u a -> GenTraceDrawing st u ()
node coord gf = nodei coord gf >> return ()


-- | Draw the object with the supplied grid coordinate. The 
-- actual position is scaled according to the 
-- @snap_grid_factors@ in the /drawing context/.
-- 
nodei :: (Fractional u, InterpretUnit u) 
      => (Int,Int) -> LocImage u a -> GenTraceDrawing st u a
nodei coord gf = askDC >>= \ctx -> 
                 position coord >>= \pt ->
                 let (a,w) = runLocImage ctx pt gf
                 in trace (singleH w) >> return a
 




-- | Draw a connector between two objects. The projection of the
-- connector line is drawn on the line from center to center of 
-- the objects, the actual start and end points of the drawn line
-- are the radial points on the objects borders that cross the 
-- projected line.
-- 
-- This function is the /forgetful/ version of 'drawrci'. 
-- Commonly, it is used to draw 'LocGraphic' objects which 
-- have no /answer/.
-- 
drawrc :: ( Real u, Floating u, InterpretUnit u
          , CenterAnchor a1, RadialAnchor a1
          , CenterAnchor a2, RadialAnchor a2
          , u ~ DUnit a1, u ~ DUnit a2
          ) 
       => a1 -> a2 -> ConnectorImage u a -> GenTraceDrawing st u ()
drawrc a b gf = drawrci a b gf >> return ()


-- | Draw a connector between two objects. The projection of the
-- connector line is drawn on the line from center to center of 
-- the objects, the actual start and end points of the drawn line
-- are the radial points on the objects borders that cross the 
-- projected line.
-- 
drawrci :: ( Real u, Floating u, InterpretUnit u
           , CenterAnchor a1, RadialAnchor  a1
           , CenterAnchor a2, RadialAnchor  a2
           , u ~ DUnit a1, u ~ DUnit a2
           ) 
        => a1 -> a2 -> ConnectorImage u a -> GenTraceDrawing st u a
drawrci a b gf = 
    let (p0,p1) = radialConnectorPoints a b in drawi (connect gf p0 p1)
