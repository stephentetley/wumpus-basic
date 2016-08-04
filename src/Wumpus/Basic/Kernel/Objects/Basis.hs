{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.Basis
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

module Wumpus.Basic.Kernel.Objects.Basis
  (

    PrimResult

  , UConvert(..)

  , ignoreAns
  , replaceAns

  , Decorate(..)
  , decorateAbove
  , decorateBelow

  , elaborateAbove
  , elaborateBelow

  ) where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.WrappedPrimitive

import Wumpus.Core                              -- package: wumpus-core


type PrimResult u a = (a, CatPrim)



--------------------------------------------------------------------------------


class UConvert (f :: * -> * -> *) where
  uconvF :: (Functor t, InterpretUnit u, InterpretUnit u1) 
         => f u (t u) -> f u1 (t u1)

  uconvZ :: (InterpretUnit u, InterpretUnit u1) 
         => f u a -> f u1 a


--------------------------------------------------------------------------------



-- | Note - the kind of f allows fo unit annotation.
--
ignoreAns :: Functor (f u) => f u a -> f u (UNil u)
ignoreAns = fmap (const UNil)

-- | Replace the answer produced by a graphic object.
--
replaceAns :: Functor (f u) => a -> f u z -> f u a
replaceAns a = fmap (const a)




-- | Decorate an object
--
-- oliterate - drops the graphic from the first object replacing 
-- it with the graphic from the second.
--
class Decorate (f :: * -> * -> *) where
  -- | Should be read as @ decorate (above|below) A with B @
  decorate    :: ZOrder -> f u a -> f u z -> f u a
  elaborate   :: ZOrder -> f u a -> (a -> f u z) -> f u a
  obliterate  :: f u a -> f u a
  hyperlink   :: XLink -> f u a -> f u a
  svgId       :: String -> f u a -> f u a
  svgAnnotate :: [SvgAttr] -> f u a -> f u a



-- | Decorate (ABOVE) a with b.
--
decorateAbove :: Decorate f => f u a -> f u z -> f u a
decorateAbove = decorate ZABOVE

-- | Decorate (BELOW) a with b.
--
decorateBelow :: Decorate f => f u a -> f u z -> f u a
decorateBelow = decorate ZBELOW

-- | Elaborate (ABOVE) a with b.
--
elaborateAbove :: Decorate f => f u a -> (a -> f u z) -> f u a
elaborateAbove = elaborate ZABOVE

-- | Elaborate (BELOW) a with b.
--
elaborateBelow :: Decorate f => f u a -> (a -> f u z) -> f u a
elaborateBelow = elaborate ZBELOW




