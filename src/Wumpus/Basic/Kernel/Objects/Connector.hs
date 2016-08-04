{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.Connector
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- ConnImage and ConnGraphic types - these are functional types
-- from the DrawingContext plus start point and end point to a 
-- graphic /primitive/.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.Connector
   (
     ConnectorImage
   , ConnectorGraphic

   , DConnectorImage
   , DConnectorGraphic

   , ConnectorQuery

   , runConnectorImage
   , runConnectorQuery
   , connect

   , stripConnectorImage
   , liftConnectorQuery

   , promoteConn
   , applyConn

   , qpromoteConn
   , qapplyConn

   , emptyConnectorImage

   )

   where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Base.QueryDC
import Wumpus.Basic.Kernel.Objects.Basis
import Wumpus.Basic.Kernel.Objects.Image

import Wumpus.Core                              -- package: wumpus-core



-- | ConnectorImage - function from DrawingContext and start and 
-- end points to a polymorphic /answer/ and a graphic /primitive/.
--
newtype ConnectorImage u a = ConnectorImage { 
          getConnectorImage :: DPoint2 -> DPoint2 -> Image u a }


type instance DUnit (ConnectorImage u a) = u



type ConnectorGraphic u = ConnectorImage u (UNil u)

-- | Type specialized version of 'ConnectorImage'.
--
type DConnectorImage a        = ConnectorImage Double a

-- | Type specialized version of 'ConnectorGraphic'.
--
type DConnectorGraphic        = ConnectorGraphic Double 


newtype ConnectorQuery u a = ConnectorQuery { 
          getConnectorQuery :: DPoint2 -> DPoint2 -> Query u a }


-- Functor 

instance Functor (ConnectorImage u) where
  fmap f ma = ConnectorImage $ \p0 p1 -> fmap f $ getConnectorImage ma p0 p1

instance Functor (ConnectorQuery u) where
  fmap f ma = ConnectorQuery $ \p0 p1 -> fmap f $ getConnectorQuery ma p0 p1


-- Applicative

instance Applicative (ConnectorImage u) where
  pure a    = ConnectorImage $ \_  _  -> pure a
  mf <*> ma = ConnectorImage $ \p0 p1 -> 
                getConnectorImage mf p0 p1 <*> getConnectorImage ma p0 p1

instance Applicative (ConnectorQuery u) where
  pure a    = ConnectorQuery $ \_  _  -> pure a
  mf <*> ma = ConnectorQuery $ \p0 p1 -> 
                getConnectorQuery mf p0 p1 <*> getConnectorQuery ma p0 p1


-- Monad 

instance Monad (ConnectorImage u) where
  return a  = ConnectorImage $ \_  _  -> return a
  ma >>= k  = ConnectorImage $ \p0 p1 -> 
                getConnectorImage ma p0 p1 >>= \ans -> 
                getConnectorImage (k ans) p0 p1


instance Monad (ConnectorQuery u) where
  return a  = ConnectorQuery $ \_  _  -> return a
  ma >>= k  = ConnectorQuery $ \p0 p1 -> 
                getConnectorQuery ma p0 p1 >>= \ans -> 
                getConnectorQuery (k ans) p0 p1


-- Monoid

instance Monoid a => Monoid (ConnectorImage u a) where
  mempty          = pure mempty
  ma `mappend` mb = ConnectorImage $ \p0 p1 -> 
                      getConnectorImage ma p0 p1 
                        `mappend` getConnectorImage mb p0 p1 


instance Monoid a => Monoid (ConnectorQuery u a) where
  mempty          = pure mempty
  ma `mappend` mb = ConnectorQuery $ \p0 p1 -> 
                      getConnectorQuery ma p0 p1 
                        `mappend` getConnectorQuery mb p0 p1 



-- DrawingCtxM

instance DrawingCtxM (ConnectorImage u) where
  askDC           = ConnectorImage $ \_  _  -> askDC
  asksDC fn       = ConnectorImage $ \_  _  -> asksDC fn
  localize upd ma = ConnectorImage $ \p0 p1 -> 
                      localize upd (getConnectorImage ma p0 p1)

instance DrawingCtxM (ConnectorQuery u) where
  askDC           = ConnectorQuery $ \_  _  -> askDC
  asksDC fn       = ConnectorQuery $ \_  _  -> asksDC fn
  localize upd ma = ConnectorQuery $ \p0 p1 -> 
                      localize upd (getConnectorQuery ma p0 p1)


  


instance Decorate ConnectorImage where
  decorate zo ma mz     = ConnectorImage $ \p0 p1 -> 
    decorate zo (getConnectorImage ma p0 p1) (getConnectorImage mz p0 p1)

  elaborate zo ma f     = ConnectorImage $ \p0 p1 -> 
    elaborate zo (getConnectorImage ma p0 p1) 
                 (\a -> getConnectorImage (f a) p0 p1)

  obliterate ma         = ConnectorImage $ \p0 p1 -> 
    obliterate $ getConnectorImage ma p0 p1

  hyperlink xl ma       = ConnectorImage $ \p0 p1 -> 
    hyperlink xl $ getConnectorImage ma p0 p1

  svgId ss ma           = ConnectorImage $ \p0 p1 -> 
    svgId ss $ getConnectorImage ma p0 p1

  svgAnnotate attrs ma  = ConnectorImage $ \p0 p1 -> 
    svgAnnotate attrs $ getConnectorImage ma p0 p1




runConnectorImage :: InterpretUnit u 
                  => DrawingContext -> Point2 u -> Point2 u
                  -> ConnectorImage u a
                  -> PrimResult u a
runConnectorImage ctx p0 p1 ma = 
    let dp0 = normalizeF (dc_font_size ctx) p0
        dp1 = normalizeF (dc_font_size ctx) p1 
    in runImage ctx $ getConnectorImage ma dp0 dp1


runConnectorQuery :: InterpretUnit u 
                  => DrawingContext -> Point2 u -> Point2 u 
                  -> ConnectorQuery u a
                  -> a
runConnectorQuery ctx p0 p1 ma = 
    let dp0 = normalizeF (dc_font_size ctx) p0
        dp1 = normalizeF (dc_font_size ctx) p1 
    in runQuery ctx $ getConnectorQuery ma dp0 dp1


connect :: InterpretUnit u 
        => ConnectorImage u a -> Point2 u -> Point2 u -> Image u a
connect ma p0 p1 = normalizeCtxF p0 >>= \dp0 -> 
                   normalizeCtxF p1 >>= \dp1 -> 
                   getConnectorImage ma dp0 dp1


stripConnectorImage :: ConnectorImage u a -> ConnectorQuery u a
stripConnectorImage ma = ConnectorQuery $ \p1 p2 -> 
    stripImage $ getConnectorImage ma p1 p2


liftConnectorQuery :: ConnectorQuery u a -> ConnectorImage u a
liftConnectorQuery ma = ConnectorImage $ \p1 p2 -> 
    liftQuery $ getConnectorQuery ma p1 p2


promoteConn :: InterpretUnit u 
            => (Point2 u -> Point2 u -> Image u a) -> ConnectorImage u a
promoteConn k = ConnectorImage $ \p0 p1 ->
    dinterpCtxF p0 >>= \up0 -> 
    dinterpCtxF p1 >>= \up1 -> 
    k up0 up1

applyConn :: InterpretUnit u 
          => ConnectorImage u a -> Point2 u -> Point2 u -> Image u a
applyConn ma p0 p1 = normalizeCtxF p0 >>= \dp0 -> 
                     normalizeCtxF p1 >>= \dp1 -> 
                     getConnectorImage ma dp0 dp1




qpromoteConn :: InterpretUnit u 
             => (Point2 u -> Point2 u -> Query u a) -> ConnectorQuery u a
qpromoteConn k = ConnectorQuery $ \p0 p1 ->
    dinterpCtxF p0 >>= \up0 -> 
    dinterpCtxF p1 >>= \up1 -> 
    k up0 up1

qapplyConn :: InterpretUnit u
           => ConnectorQuery u a -> Point2 u -> Point2 u -> Query u a
qapplyConn ma p0 p1 = normalizeCtxF p0 >>= \dp0 -> 
                      normalizeCtxF p1 >>= \dp1 -> 
                      getConnectorQuery ma dp0 dp1


--------------------------------------------------------------------------------
-- UConvert instance

instance UConvert ConnectorImage where
  uconvF = uconvConnectorImageF
  uconvZ = uconvConnectorImageZ


-- | Use this to convert 'ConnectorGraphic' or 'ConnectorImage' 
-- with Functor answer.
--
uconvConnectorImageF :: (InterpretUnit u, InterpretUnit u1, Functor t) 
                     => ConnectorImage u (t u) -> ConnectorImage u1 (t u1)
uconvConnectorImageF ma = ConnectorImage $ \p0 p1 -> 
    uconvF $ getConnectorImage ma p0 p1




-- | Use this to convert 'ConnectorImage' with unit-less answer.
--
uconvConnectorImageZ :: (InterpretUnit u, InterpretUnit u1) 
                     => ConnectorImage u a -> ConnectorImage u1 a
uconvConnectorImageZ ma = ConnectorImage $ \p0 p1 -> 
    uconvZ $ getConnectorImage ma p0 p1

-- | Having /empty/ at the specific 'ConnectorImage' type is useful.
-- 
emptyConnectorImage :: Monoid a => ConnectorImage u a
emptyConnectorImage = mempty


--------------------------------------------------------------------------------



--
-- Design note - potentially there are no useful combining 
-- operators on Connectors (!).
--
-- Division - i.e. splitting a path at points between the start 
-- and end - seems a more obvious operation on connector paths 
-- than combination. See the ConnectorPath operations in 
-- Wumpus-Drawing for some examples.
--

