{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.System.FontLoader.Datatypes
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Datatypes
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.System.FontLoader.Datatypes
  (
  
  -- * Glyph metrics

    PSCharCode
  , PSEncodingScheme
  , AfmBoundingBox

  , AfmKey
  , GlobalInfo
  , AfmFile(..)
  , AfmGlyphMetrics(..)

  , MonospaceDefaults(..)

  , FontProps(..)
  , buildMetricsOps
  
  ) where


import Wumpus.Basic.Kernel.Base.FontSupport

import Wumpus.Core                              -- package: wumpus-core

import qualified Data.IntMap   as IM
import qualified Data.Map      as M








--------------------------------------------------------------------------------

-- | Afm files index glyphs by /PostScript character code/. This 
-- is not the same as Unicode, ASCII...
--
-- It is expected to be determined by @EncodingScheme@ in the
-- Global Font Information Section.
--
type PSCharCode         = Int

type PSEncodingScheme   = String

type AfmBoundingBox     = BoundingBox AfmUnit

type AfmKey         = String
type GlobalInfo     = M.Map AfmKey String



-- | Wumpus needs a very small subset of AFM files, common to both
-- version 2.0 and version 4.1.
--
-- Note - Bounding Box is mandatory for AFM versions 3.0 and 4.1
-- 
-- Cap Height is optional in AFM versions 3.0 and 4.1. As Wumpus 
-- uses cap height in calculations, glyph metrics must be build 
-- with an arbitrary value if it is not present.
--
-- Encoding Scheme is optional in AFM files.
--
data AfmFile = AfmFile 
      { afm_encoding                :: Maybe String
      , afm_letter_bbox             :: Maybe AfmBoundingBox
      , afm_cap_height              :: Maybe AfmUnit
      , afm_descender               :: Maybe AfmUnit
      , afm_underline_position      :: Maybe AfmUnit
      , afm_underline_thickness     :: Maybe AfmUnit
      , afm_glyph_metrics           :: [AfmGlyphMetrics]
      }
  deriving (Show) 
  
-- Note - for AfmFile BBox is a required field for version 4.1, 
-- but it appears to be optional for version 2.0.
--


data AfmGlyphMetrics = AfmGlyphMetrics
      { afm_char_code       :: !PSCharCode
      , afm_width_vector    :: !(Vec2 AfmUnit)
      , afm_char_name       :: !String
      }
  deriving (Eq,Show)


-- | Monospace defaults are used if the font loader fails to 
-- extract the necessary fields.
-- 
-- The values are taken from the font correpsonding to Courier 
-- in the distributed font files.
--
data MonospaceDefaults cu = MonospaceDefaults 
      { default_letter_bbox         :: BoundingBox cu
      , default_cap_height          :: cu
      , default_descender           :: cu
      , default_underline_position  :: cu
      , default_underline_thickness :: cu
      , default_char_width          :: Vec2 cu
      }
  deriving (Eq,Show)



-- | The metrics read from a font file by a font loader. 
-- 
-- NOTE - FontProps is parametric on @cu@ - /Character Unit/ and 
-- not on the usual @u@. A typical character unit is 'AfmUnit', 
-- the unit of measurement for AFM files (1000th of a point).
--
-- The is the initial representation used by Wumpus-Basic as an
-- syntax tree when loading font files. 
--
data FontProps cu = FontProps
       { fp_bounding_box        :: BoundingBox cu 
       , fp_default_adv_vec     :: Vec2 cu
       , fp_adv_vecs            :: IM.IntMap (Vec2 cu)
       , fp_cap_height          :: cu
       , fp_descender           :: cu
       , fp_underline_position  :: cu
       , fp_underline_thickness :: cu
       }


-- | Build a MetricsOps function table, from a character unit
-- scaling function and FontProps read from a file.
--
buildMetricsOps :: (FontSize -> cu -> Double) -> FontProps cu -> FontMetrics
buildMetricsOps fn font@(FontProps { fp_bounding_box = BBox ll ur
                                   , fp_default_adv_vec = V2 vx vy }) = 
    FontMetrics
      { get_bounding_box  = \sz -> BBox (scalePt sz ll) (scalePt sz ur)
      , get_cw_table      = \sz i -> 
            maybe (defaultAV sz) (scaleVec sz) $ IM.lookup i (fp_adv_vecs font)
      , get_cap_height    = \sz -> fn sz (fp_cap_height font)
      , get_descender     = \sz -> fn sz (fp_descender font)
      , get_underline_position  = \sz -> fn sz (fp_underline_position font)
      , get_underline_thickness = \sz -> fn sz (fp_underline_thickness font)
      }
  where
    defaultAV sz            = V2 (fn sz vx) (fn sz vy) 
    scalePt  sz (P2 cx cy)  = P2 (fn sz cx) (fn sz cy) 
    scaleVec sz (V2 cx cy)  = V2 (fn sz cx) (fn sz cy) 



