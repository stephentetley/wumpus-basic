{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Import shim for @Wumpus.Basic.Kernel@ modules.
--
-- @Kernel.Base@ - low-level objects, general enumerations, unit 
-- and @DrawingContext@ support. @DrawingContext@ is comparative 
-- to the /graphics state/ in PostScript, but it is a read-only
-- environment (cf. the Reader monad). Like the Reader monad it 
-- supports branching update through @local@ - here called 
-- @localize@.
-- 
-- @Kernel.Objects@ - \"elementary\" drawing objects, plus some 
-- catalogues of named, predefined drawing objects 
-- (DrawingPrimitives) and useful operations (named vectors - 
-- Displacement).
--
-- @Kernel.Drawing@ - \"collective\" drawing objects. @Drawing@ is 
-- considered a higher layer than @Objects@, so there should be 
-- dependencies only from @Drawing@ to @Objects@.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel
  (
    module Wumpus.Basic.Kernel.Base.BaseDefs
  , module Wumpus.Basic.Kernel.Base.DrawingContext
  , module Wumpus.Basic.Kernel.Base.FontSupport
  , module Wumpus.Basic.Kernel.Base.QueryDC
  , module Wumpus.Basic.Kernel.Base.Units
  , module Wumpus.Basic.Kernel.Base.UpdateDC
  , module Wumpus.Basic.Kernel.Base.WrappedPrimitive
  , module Wumpus.Basic.Kernel.Drawing.Basis
  , module Wumpus.Basic.Kernel.Drawing.Chain
  , module Wumpus.Basic.Kernel.Drawing.CtxPicture
  , module Wumpus.Basic.Kernel.Drawing.LocDrawing
  , module Wumpus.Basic.Kernel.Drawing.LocTrace
  , module Wumpus.Basic.Kernel.Drawing.PosObject
  , module Wumpus.Basic.Kernel.Drawing.TraceDrawing
  , module Wumpus.Basic.Kernel.Objects.AdvObject
  , module Wumpus.Basic.Kernel.Objects.Anchors
  , module Wumpus.Basic.Kernel.Objects.Basis
  , module Wumpus.Basic.Kernel.Objects.Bounded
  , module Wumpus.Basic.Kernel.Objects.Concat
  , module Wumpus.Basic.Kernel.Objects.Connector
  , module Wumpus.Basic.Kernel.Objects.Displacement
  , module Wumpus.Basic.Kernel.Objects.DrawingPrimitives
  , module Wumpus.Basic.Kernel.Objects.Image
  , module Wumpus.Basic.Kernel.Objects.LocImage
  , module Wumpus.Basic.Kernel.Objects.LocThetaImage
  , module Wumpus.Basic.Kernel.Objects.Orientation
  , module Wumpus.Basic.Kernel.Objects.Trail

  ) where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Base.FontSupport
import Wumpus.Basic.Kernel.Base.QueryDC
import Wumpus.Basic.Kernel.Base.Units 
import Wumpus.Basic.Kernel.Base.UpdateDC
import Wumpus.Basic.Kernel.Base.WrappedPrimitive
import Wumpus.Basic.Kernel.Drawing.Basis
import Wumpus.Basic.Kernel.Drawing.Chain
import Wumpus.Basic.Kernel.Drawing.CtxPicture
import Wumpus.Basic.Kernel.Drawing.LocDrawing
import Wumpus.Basic.Kernel.Drawing.LocTrace
import Wumpus.Basic.Kernel.Drawing.PosObject
import Wumpus.Basic.Kernel.Drawing.TraceDrawing
import Wumpus.Basic.Kernel.Objects.AdvObject
import Wumpus.Basic.Kernel.Objects.Anchors
import Wumpus.Basic.Kernel.Objects.Basis
import Wumpus.Basic.Kernel.Objects.Bounded
import Wumpus.Basic.Kernel.Objects.Concat
import Wumpus.Basic.Kernel.Objects.Connector
import Wumpus.Basic.Kernel.Objects.Displacement
import Wumpus.Basic.Kernel.Objects.DrawingPrimitives
import Wumpus.Basic.Kernel.Objects.Image
import Wumpus.Basic.Kernel.Objects.LocImage
import Wumpus.Basic.Kernel.Objects.LocThetaImage
import Wumpus.Basic.Kernel.Objects.Orientation
import Wumpus.Basic.Kernel.Objects.Trail
