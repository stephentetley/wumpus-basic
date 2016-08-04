{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Drawing.Chain
-- Copyright   :  (c) Stephen Tetley 2011-2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Chaining LocGraphics.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Drawing.Chain
  (
  
    GenChain
  , Chain
  , DChain
  , ChainScheme(..)

  , runGenChain
  , evalGenChain
  , execGenChain
  , stripGenChain

  , runChain
  , runChain_

  , chain1
  , chainSkip_
  , chainMany
  , chainReplicate
  , chainCount

  , iterationScheme
  , sequenceScheme
  , catTrailScheme
  , countingScheme


  , horizontalScheme
  , verticalScheme


  , rowwiseTableScheme
  , columnwiseTableScheme
  

  , distribRowwiseTable
  , duplicateRowwiseTable
  , distribColumnwiseTable
  , duplicateColumnwiseTable

  , radialChainScheme

  ) where


import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Base.WrappedPrimitive
import Wumpus.Basic.Kernel.Drawing.Basis
import Wumpus.Basic.Kernel.Objects.Basis
import Wumpus.Basic.Kernel.Objects.Displacement
import Wumpus.Basic.Kernel.Objects.Image
import Wumpus.Basic.Kernel.Objects.LocImage
import Wumpus.Basic.Kernel.Objects.Trail

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative
import Data.Monoid



newtype GenChain st u a = GenChain
          { getGenChain :: DrawingContext -> DPoint2 -> ChainSt st u 
                        -> (a, DPoint2, ChainSt st u, CatPrim) }


type instance DUnit (GenChain st u a)   = u
type instance UState  (GenChain st u a) = st

type Chain u a   = GenChain () u a

type DChain a    = Chain Double a

-- | scheme_start is a function from the origin to state.
-- 
-- For instance, we might want to cache the origin - this would
-- not be possible if start was just a pure @cst@ value. 
--
data ChainScheme u = forall cst. ChainScheme 
      { chain_init      :: Point2 u -> cst
      , chain_step      :: Point2 u -> cst -> (Point2 u,cst)
      }

type instance DUnit (ChainScheme u) = u


data ChainSt st u = forall cst. ChainSt 
       { chain_count      :: Int
       , chain_st         :: cst
       , chain_next       :: Point2 u -> cst -> (Point2 u,cst) 
       , chain_user_state :: st
       }


type instance DUnit (ChainSt st u) = u


-- Functor 

instance Functor (GenChain st u) where
  fmap f ma = GenChain $ \ctx pt s -> 
              let (a,p1,s1,w) = getGenChain ma ctx pt s in (f a, p1, s1, w)



-- Applicative

instance Applicative (GenChain st u) where
  pure a    = GenChain $ \_   pt s -> (a, pt, s, mempty)
  mf <*> ma = GenChain $ \ctx pt s -> 
                let (f,p1,s1,w1) = getGenChain mf ctx pt s
                    (a,p2,s2,w2) = getGenChain ma ctx p1 s1
                in (f a, p2, s2, w1 `mappend` w2)



-- Monad

instance Monad (GenChain st u) where
  return a  = GenChain $ \_   pt s -> (a, pt, s, mempty)
  ma >>= k  = GenChain $ \ctx pt s -> 
                let (a,p1,s1,w1) = getGenChain ma ctx pt s
                    (b,p2,s2,w2) = (getGenChain . k) a ctx p1 s1
                in (b, p2, s2, w1 `mappend` w2)


-- DrawingCtxM

instance DrawingCtxM (GenChain st u) where
  askDC           = GenChain $ \ctx pt s -> (ctx, pt, s, mempty)
  asksDC fn       = GenChain $ \ctx pt s -> (fn ctx, pt, s, mempty)
  localize upd ma = GenChain $ \ctx pt s -> getGenChain ma (upd ctx) pt s



-- UserStateM 

instance UserStateM (GenChain st u) where
  getState        = GenChain $ \_ pt s@(ChainSt _ _ _ ust) -> 
                      (ust, pt, s, mempty)
  setState ust    = GenChain $ \_ pt (ChainSt i a b _) -> 
                      ((), pt, ChainSt i a b ust, mempty)
  updateState upd = GenChain $ \_ pt (ChainSt i a b ust) -> 
                      ((), pt, ChainSt i a b (upd ust), mempty)


-- LocationM

instance InterpretUnit u => LocationM (GenChain st u) where
  location = GenChain $ \ctx pt s ->
      let upt = dinterpF (dc_font_size ctx) pt in (upt, pt, s, mempty) 



-- Monoid

instance Monoid a => Monoid (GenChain st u a) where
  mempty           = GenChain $ \_   pt s -> (mempty, pt, s, mempty)
  ma `mappend` mb  = GenChain $ \ctx pt s -> 
                       let (a,p1,s1,w1) = getGenChain ma ctx pt s
                           (b,p2,s2,w2) = getGenChain mb ctx p1 s1
                       in (a `mappend` b, p2, s2, w1 `mappend` w2)

--------------------------------------------------------------------------------
-- Run functions

runGenChain :: InterpretUnit u 
            => ChainScheme u -> st -> GenChain st u a -> LocImage u (a,st)
runGenChain (ChainScheme start step) ust ma = promoteLoc $ \pt -> 
    askDC >>= \ctx ->
    let st_zero     = ChainSt { chain_count      = 0
                              , chain_st         = start pt
                              , chain_next       = step
                              , chain_user_state = ust }
        dpt         = normalizeF (dc_font_size ctx) pt
        (a,_,s1,w1) = getGenChain ma ctx dpt st_zero
    in replaceAns (a, chain_user_state s1) $ primGraphic w1



-- | Forget the user state LocImage, just return the /answer/.
--
evalGenChain :: InterpretUnit u 
             => ChainScheme u -> st -> GenChain st u a -> LocImage u a
evalGenChain cscm st ma = fmap fst $ runGenChain cscm st ma


-- | Forget the /answer/, just return the user state.
--
execGenChain :: InterpretUnit u 
             => ChainScheme u -> st -> GenChain st u a -> LocImage u st 
execGenChain cscm st ma = fmap snd $ runGenChain cscm st ma


stripGenChain :: InterpretUnit u 
              => ChainScheme u -> st -> GenChain st u a -> LocQuery u (a,st)
stripGenChain cscm st ma = stripLocImage $ runGenChain cscm st ma



runChain :: InterpretUnit u 
         => ChainScheme u -> Chain u a -> LocImage u a
runChain cscm ma = evalGenChain cscm () ma

runChain_ :: InterpretUnit u 
          => ChainScheme u -> Chain u a -> LocGraphic u
runChain_ cscm ma = ignoreAns $ runChain cscm ma




--------------------------------------------------------------------------------
-- Operations


-- | Demand a point on the Chain and draw the LocImage
-- at it.
--
chain1 :: InterpretUnit u 
       => LocImage u a -> GenChain st u a
chain1 gf  = GenChain $ \ctx pt (ChainSt i0 s0 sf ust) -> 
    let upt       = dinterpF (dc_font_size ctx) pt
        (a,w1)    = runImage ctx $ applyLoc gf upt
        (pt1,st1) = sf upt s0
        dpt1      = normalizeF (dc_font_size ctx) pt1
        new_st    = ChainSt { chain_count      = i0 + 1
                            , chain_st         = st1
                            , chain_next       = sf
                            , chain_user_state = ust }
    in (a, dpt1, new_st, w1)


-- | Demand the next position, but draw nothing.
--
chainSkip_ :: InterpretUnit u => GenChain st u ()
chainSkip_ = GenChain $ \ctx pt (ChainSt i0 s0 sf ust) -> 
    let upt       = dinterpF (dc_font_size ctx) pt
        (pt1,st1) = sf upt s0
        dpt1      = normalizeF (dc_font_size ctx) pt1
        new_st    = ChainSt { chain_count      = i0 + 1
                            , chain_st         = st1
                            , chain_next       = sf
                            , chain_user_state = ust }
    in ((), dpt1, new_st, mempty)



-- | Chain a list of images, each demanding a succesive start 
-- point.
--
chainMany :: InterpretUnit u 
          => [LocImage u a] -> GenChain st u (UNil u)
chainMany = ignoreAns . mapM_ chain1


-- | Replicate a LocImage @n@ times along a Chain.
--
chainReplicate :: InterpretUnit u 
               => Int -> LocImage u a -> GenChain st u (UNil u)
chainReplicate n = chainMany . replicate n 


-- | Return the count of chain steps.
--
chainCount :: GenChain st u Int
chainCount = GenChain $ \_ dpt st@(ChainSt i _ _ _) -> (i, dpt, st, mempty)
             




--------------------------------------------------------------------------------
-- Schemes


-- | General scheme - iterate the next point with the supplied
-- function.
--
iterationScheme :: (Point2 u -> Point2 u) -> ChainScheme u
iterationScheme fn = ChainScheme { chain_init = const ()
                                 , chain_step = \pt _ -> (fn pt, ())
                                 }

-- | General scheme - displace successively by the elements of the
-- list of vectors. 
-- 
-- Note - the list is cycled to make the chain infinite.
--
sequenceScheme :: Num u => [Vec2 u] -> ChainScheme u
sequenceScheme [] = error "sequenceScheme - empty list."
sequenceScheme vs = ChainScheme { chain_init = const $ cycle vs
                                , chain_step = step
                                }
  where
    step _  []     = error "sequenceScheme - unreachable, cycled."
    step pt (w:ws) = (displace w pt, ws) 


-- | Derive a ChainScheme from a CatTrail.
--
-- Note - this iterates the control points of curves, it does not
-- iterate points on the curve.
--
catTrailScheme :: Num u => CatTrail u -> ChainScheme u
catTrailScheme = sequenceScheme . linear . destrCatTrail
  where
    linear (TLine v0 :xs)        = v0 : linear xs
    linear (TCurve v0 v1 v2 :xs) = v0 : v1 : v2 : linear xs
    linear []                    = []


-- | Build an (infinite) ChainScheme for a prefix list of counted 
-- schemes and a final scheme that runs out to infinity.
--
countingScheme :: [(Int, ChainScheme u)] -> ChainScheme u -> ChainScheme u
countingScheme []     rest = rest
countingScheme (x:xs) rest = chainPrefix  x (countingScheme xs rest)


-- | Helper - complicated...
--
chainPrefix :: (Int, ChainScheme u) -> ChainScheme u -> ChainScheme u
chainPrefix (ntimes, ChainScheme astart astep) rest@(ChainScheme bstart bstep)
    | ntimes < 1 = rest
    | otherwise  = ChainScheme { chain_init = start, chain_step = next }
  where
    start pt = (astart pt,ntimes, bstart pt)

    next pt (ast,n,bst) 
        | n > 0     = let (p2,ast1) = astep pt ast in (p2, (ast1,n-1,bst))
        | n == 0    = let bst1      = bstart pt 
                          (p2,bst2) = bstep pt bst1 
                      in (p2, (ast,(-1),bst2))
        | otherwise = let (p2,bst1) = bstep pt bst in (p2,(ast, (-1), bst1))
 



horizontalScheme :: Num u => u -> ChainScheme u
horizontalScheme dx = iterationScheme (displace (hvec dx))
                
   
verticalScheme :: Num u => u -> ChainScheme u
verticalScheme dy = iterationScheme (displace (vvec dy))
               




-- | Outer and inner steppers.
--
scStepper :: PointDisplace u -> Int -> PointDisplace u 
          -> ChainScheme u
scStepper outF n innF = 
    ChainScheme { chain_init = start, chain_step = step }
  where
    start pt                      = (pt,1)
    step  pt (ogin,i) | i <  n    = (innF pt, (ogin, i+1))
                      | otherwise = let o1 = outF ogin 
                                    in (o1, (o1,1)) 



-- | Generate a tabular scheme going rowwise (left-to-right) and
-- downwards.
--
-- TODO - should probably account for the initial position... 
--
rowwiseTableScheme :: Num u => Int -> (u,u) -> ChainScheme u
rowwiseTableScheme num_cols (col_width,row_height) = 
    scStepper downF num_cols rightF
  where
    downF   = displace $ vvec $ negate row_height
    rightF  = displace $ hvec col_width

-- | Generate a tabular scheme going columwise (top-to-bottom) 
-- and rightwards.
--
-- TODO - should probably account for the initial position... 
--
columnwiseTableScheme :: Num u => Int -> (u,u) -> ChainScheme u
columnwiseTableScheme num_rows (col_width,row_height) = 
    scStepper rightF num_rows downF
  where
    downF   = displace $ vvec $ negate row_height
    rightF  = displace $ hvec col_width




distribRowwiseTable :: (Monoid a, InterpretUnit u)
                    => Int -> (u,u) -> [LocImage u a] -> LocImage u a
distribRowwiseTable num_cols dims gs = fmap mconcat $ 
    runChain (rowwiseTableScheme num_cols dims) $ mapM chain1 gs
  

duplicateRowwiseTable :: (Monoid a, InterpretUnit u)
                      => Int -> Int -> (u,u) -> LocImage u a -> LocImage u a
duplicateRowwiseTable i num_cols dims gf =
    distribRowwiseTable num_cols dims (replicate i gf) 



distribColumnwiseTable :: (Monoid a, InterpretUnit u)
                       => Int -> (u,u) -> [LocImage u a] -> LocImage u a
distribColumnwiseTable num_rows dims gs = fmap mconcat $ 
    runChain (columnwiseTableScheme num_rows dims) $ mapM chain1 gs
  

duplicateColumnwiseTable :: (Monoid a, InterpretUnit u)
                         => Int -> Int -> (u,u) -> LocImage u a -> LocImage u a
duplicateColumnwiseTable i num_rows dims gf = 
    distribColumnwiseTable num_rows dims (replicate i gf) 



-- | TODO - account for CW CCW or just rely on +ve -ve angles?...
--
radialChainScheme :: Floating u 
                  => u -> Radian -> Radian -> ChainScheme u
radialChainScheme radius angstart angi = 
    ChainScheme { chain_init = start, chain_step = step }
  where
    start pt           = let ogin = displace (avec angstart (-radius)) pt
                         in (ogin, angstart)
    step  _ (ogin,ang) = let ang_next = ang + angi 
                             pt       = displace (avec ang_next radius) ogin
                         in (pt, (ogin, ang_next))

    

-- radialChain is convoluted because first point is not the 
-- circle center but a point on the circumference. Also the next
-- step iterates the (constant) origin rather than the previous 
-- point.




    
    

