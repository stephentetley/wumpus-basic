{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.Image
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Common types and operations.
-- 
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.Image
  (


    Image
  , Graphic 

  , Query

  , DImage
  , DGraphic

  , runImage
  , runQuery

  , stripImage
  , liftQuery

  , emptyImage
  , primGraphic
  , clipImage


  ) where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Base.WrappedPrimitive
import Wumpus.Basic.Kernel.Objects.Basis

import Wumpus.Core                              -- package: wumpus-core



--------------------------------------------------------------------------------


newtype Image u a = Image { 
          getImage :: DrawingContext -> (a, CatPrim) }

type instance DUnit (Image u a) = u

type Graphic u = Image u (UNil u)


-- | Type specialized version of 'Image'.
--
type DImage a       = Image Double a

-- | Type specialized version of 'Graphic'.
--
type DGraphic       = Graphic Double 


newtype Query u a = Query { 
          getQuery :: DrawingContext -> a }

type instance DUnit (Query u a) = u

-- Functor

instance Functor (Image u) where
  fmap f ma = Image $ \ctx -> let (a,w1) = getImage ma ctx in (f a, w1)

instance Functor (Query u) where
  fmap f ma = Query $ \ctx -> f $ getQuery ma ctx

-- Applicative

instance Applicative (Image u) where
  pure a    = Image $ \_   -> (a,mempty)
  mf <*> ma = Image $ \ctx -> let (f,w1) = getImage mf ctx 
                                  (a,w2) = getImage ma ctx
                              in (f a, w1 `mappend` w2)

instance Applicative (Query u) where
  pure a    = Query $ \_   -> a
  mf <*> ma = Query $ \ctx -> let f = getQuery mf ctx 
                                  a = getQuery ma ctx
                              in f a


-- Monad

instance Monad (Image u) where
  return a = Image $ \_   -> (a,mempty)
  ma >>= k = Image $ \ctx -> let (a,w1) = getImage ma ctx 
                                 (b,w2) = getImage (k a) ctx
                             in (b,w1 `mappend` w2)


instance Monad (Query u) where
  return a = Query $ \_   -> a
  ma >>= k = Query $ \ctx -> let a = getQuery ma ctx in getQuery (k a) ctx

-- Monoid

instance Monoid a => Monoid (Image u a) where
  mempty          = pure mempty
  ma `mappend` mb = Image $ \ctx -> 
                      getImage ma ctx `mappend` getImage mb ctx

instance Monoid a => Monoid (Query u a) where
  mempty          = pure mempty
  ma `mappend` mb = Query $ \ctx -> 
                      getQuery ma ctx `mappend` getQuery mb ctx


-- DrawingCtxM


instance DrawingCtxM (Image u) where
  askDC           = Image $ \ctx -> (ctx, mempty)
  asksDC fn       = Image $ \ctx -> (fn ctx, mempty)
  localize upd ma = Image $ \ctx -> getImage ma (upd ctx)

instance DrawingCtxM (Query u) where
  askDC           = Query $ \ctx -> ctx
  asksDC fn       = Query $ \ctx -> (fn ctx)
  localize upd ma = Query $ \ctx -> getQuery ma (upd ctx)


runImage :: DrawingContext -> Image u a -> PrimResult u a
runImage ctx ma = getImage ma ctx

runQuery :: DrawingContext -> Query u a -> a
runQuery ctx ma = getQuery ma ctx



-- | Strip the graphic content from an 'Image' making a 'Query'.
-- 
stripImage :: Image u a -> Query u a
stripImage ma = Query $ \ctx -> fst $ getImage ma ctx


-- | Turn a 'Query' into an 'Image' without graphic content.
--
liftQuery :: Query u a -> Image u a
liftQuery ma = askDC >>= \ctx -> let a = runQuery ctx ma in return a



-- | Constructor for Primtive graphics.
--
primGraphic :: CatPrim -> Graphic u
primGraphic w = Image $ \_ -> (UNil, w)


-- | Clip an Image.
-- 
clipImage :: PrimPath -> Image u a -> Image u a
clipImage pp ma = Image $ \ctx -> 
     let (a,w) = getImage ma ctx in (a, cpmap (clipPrim pp) w)



instance UConvert Image where
  uconvZ = uconvImageZ
  uconvF = uconvImageF

uconvImageF :: (Functor t, InterpretUnit u, InterpretUnit u1) 
            => Image u (t u) -> Image u1 (t u1) 
uconvImageF ma = Image $ \ctx -> 
    let (a,w) = getImage ma ctx
        a'    = uconvertF (dc_font_size ctx) a
    in (a',w)


uconvImageZ :: Image u a -> Image u1 a
uconvImageZ ma = Image $ \ctx -> getImage ma ctx


-- | Having /empty/ at the specific 'Image' type is useful.
-- 
emptyImage :: Monoid a => Image u a
emptyImage = mempty

--------------------------------------------------------------------------------


instance Decorate Image where
  decorate      = decorateImage
  elaborate     = elaborateImage  
  obliterate    = obliterateImage
  hyperlink     = hyperlinkImage
  svgId         = svgIdImage  
  svgAnnotate   = svgAnnotateImage 

-- | Decorate Image.
--
decorateImage :: ZOrder -> Image u a -> Image u z -> Image u a
decorateImage zo ma mb = Image $ \ctx -> 
    step zo (getImage ma ctx) (getImage mb ctx)
  where
    step ZABOVE (a,w1) (_,w2) = (a, w1 `mappend` w2)
    step ZBELOW (a,w1) (_,w2) = (a, w2 `mappend` w1)


-- | Elaborate Image.
--
elaborateImage :: ZOrder -> Image u a -> (a -> Image u z) -> Image u a
elaborateImage zo ma k = Image $ \ ctx ->
    let (a,w1) = getImage ma ctx
        (_,w2) = getImage (k a) ctx 
    in case zo of
      ZABOVE -> (a, w1 `mappend` w2)
      ZBELOW -> (a, w2 `mappend` w1)


obliterateImage :: Image u a -> Image u a
obliterateImage ma = Image $ \ctx -> 
    let (a,_) = getImage ma ctx in (a,mempty)
  
hyperlinkImage :: XLink -> Image u a -> Image u a
hyperlinkImage xl ma = Image $ \ctx -> step (getImage ma ctx)
  where
    step (a,w) = (a, cpmap (xlinkPrim xl) w)

svgIdImage :: String -> Image u a -> Image u a
svgIdImage ss ma = Image $ \ctx -> step (getImage ma ctx)
  where
    step (a,w) = (a, cpmap (xidPrim ss) w)

svgAnnotateImage :: [SvgAttr] -> Image u a -> Image u a
svgAnnotateImage attrs ma = Image $ \ctx -> step (getImage ma ctx)
  where
    step (a,w) = (a, cpmap (annotateGroup attrs) w)


--------------------------------------------------------------------------------
-- Affine instances 

-- 
-- Design Note
--
-- Are PrimW instances needed as Image cannot use them?
-- 

instance Rotate a => Rotate (Image u a) where
  rotate ang ma = Image $ \ctx -> 
      let (a,w) = getImage ma ctx
      in (rotate ang a, rotate ang w)

instance (RotateAbout a, InterpretUnit u, u ~ DUnit a) => 
    RotateAbout (Image u a) where
  rotateAbout ang pt ma = Image $ \ctx -> 
      let ptu   = uconvertF (dc_font_size ctx) pt
          (a,w) = getImage ma ctx
      in (rotateAbout ang pt a, rotateAbout ang ptu w)


instance Scale a => Scale (Image u a) where
  scale sx sy ma = Image $ \ctx -> 
      let (a,w) = getImage ma ctx
      in (scale sx sy a, scale sx sy w)


instance (Translate a, InterpretUnit u, u ~ DUnit a) => 
    Translate (Image u a) where
  translate dx dy ma = Image $ \ctx -> 
      let sz    = dc_font_size ctx
          ddx   = uconvert1 sz dx
          ddy   = uconvert1 sz dy
          (a,w) = getImage ma ctx
      in (translate dx dy a, translate ddx ddy w)




