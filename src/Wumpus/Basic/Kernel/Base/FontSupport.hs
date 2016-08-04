{-# LANGUAGE RankNTypes                 #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Base.FontSupport
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Data types representing font metrics.
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Base.FontSupport
  ( 

    FontName
  , CodePoint
  , FontDef(..)
  , FontFamily(..)
  , regularWeight
  , boldWeight
  , italicWeight
  , boldItalicWeight

  , CharWidthLookup

  , FontMetrics(..)

  , FontTable
  , emptyFontTable
  , lookupFont
  , insertFont

  , FontLoadMsg
  , FontLoadLog
  , fontLoadMsg

  , FontLoadResult(..)
  , printLoadErrors

  , monospace_metrics

  
  ) where

import Wumpus.Basic.Utils.HList

import Wumpus.Core                              -- package: wumpus-core

import qualified Data.Map      as Map



type FontName = String

-- | A Unicode code-point.
--
type CodePoint = Int

-- | FontDef wraps @FontFace@ from Wumpus-Core with file name
-- information for the font loaders.
--
data FontDef = FontDef
      { font_def_face   :: FontFace
      , gs_file_name    :: String
      , afm_file_name   :: String
      }
  deriving (Eq,Ord,Show)



-- | A family group of FontDefs (regular, bold, italic and 
-- bold-italic).
--
-- It is convenient for some higher-level text objects in Wumpus 
-- (particularly @Doc@ in Wumpus-Drawing) to treat a font and its
-- standard weights as the same entity. This allows @Doc@ API to 
-- provide a @bold@ operation to simply change to the the bold
-- weight of the current family, rather than use the primitive 
-- @set_font@ operation to change to an explicitly named font.
--
--
data FontFamily = FontFamily
      { ff_regular      :: FontDef
      , ff_bold         :: Maybe FontDef
      , ff_italic       :: Maybe FontDef
      , ff_bold_italic  :: Maybe FontDef  
      }

-- | Extract the regular weight 'FontDef' from a 'FontFamily'.
--
regularWeight :: FontFamily -> FontDef
regularWeight = ff_regular


-- | Extract the bold weight 'FontDef' from a 'FontFamily'.
--
-- Note - this falls back to the regular weight if the font family 
-- has no bold weight. To get the bold weight or @Nothing@ if it
-- is not present use the record selector @ff_bold@.
--
boldWeight :: FontFamily -> FontDef
boldWeight s = maybe (ff_regular s) id $ ff_bold s


-- | Extract the @italic@ weight 'FontDef' from a 'FontFamily'.
--
-- Note - this falls back to the regular weight if the font family 
-- has no italic weight. To get the italic weight or @Nothing@ if 
-- it is not present use the record selector @ff_italic@.
--
italicWeight :: FontFamily -> FontDef
italicWeight s = maybe (ff_regular s) id $ ff_italic s


-- | Extract the @bold-italic@ weight 'FontDef' from a 
-- 'FontFamily'.
--
-- Note - this falls back to the regular weight if the font family 
-- has no bold-italic weight. To get the bold-italic weight or 
-- @Nothing@ if it is not present use the record selector 
-- @ff_bold_italic@.
--
boldItalicWeight :: FontFamily -> FontDef
boldItalicWeight s = maybe (ff_regular s) id $ ff_bold_italic s



-- | A lookup function from code point to /width vector/.
--
-- The unit is always stored as a Double representing PostScript
-- points.
--
-- Note - in PostScript terminology a width vector is not obliged
-- to be left-to-right (writing direction 0). It could be 
-- top-to-bottom (writing direction 1).
--
type CharWidthLookup = CodePoint -> Vec2 Double



-- | 'FontMetrics' store a subset of the properties available in 
-- a font file - enough to calculate accurate bounding boxes and
-- positions for text.
--
-- > Bounding box representing the maximum glyph area.
-- > Width vectors for each character.
-- > Cap height
-- > Descender depth.
--
-- Because Wumpus always needs font metrics respective to the 
-- current point size, the actual fields are all functions.
--
data FontMetrics = FontMetrics
    { get_bounding_box          :: FontSize -> BoundingBox Double
    , get_cw_table              :: FontSize -> CharWidthLookup
    , get_cap_height            :: FontSize -> Double
    , get_descender             :: FontSize -> Double
    , get_underline_position    :: FontSize -> Double
    , get_underline_thickness   :: FontSize -> Double
    }


-- | A map between a font name and the respective FontMetrics.
--
newtype FontTable = FontTable { 
          getFontTable :: Map.Map FontName FontMetrics }



instance Monoid FontTable where
  mempty        = emptyFontTable
  a `mappend` b = FontTable $ getFontTable a `mappend` getFontTable b


emptyFontTable :: FontTable
emptyFontTable = FontTable $ Map.empty


-- | 'FontLoadMsg' - type synonym for String.
--
type FontLoadMsg        = String

-- | 'FontLoadLog' is a Hughes list of Strings, so it supports 
-- efficient append.
--
newtype FontLoadLog     = FontLoadLog { getFontLoadLog :: H FontLoadMsg }


instance Monoid FontLoadLog where
  mempty        = FontLoadLog $ emptyH
  a `mappend` b = FontLoadLog $ getFontLoadLog a `appendH` getFontLoadLog b


fontLoadMsg :: String -> FontLoadLog 
fontLoadMsg = FontLoadLog . wrapH


-- Need a synonym for @FontLoading@...
data FontLoadResult = FontLoadResult
      { loaded_font_table    :: FontTable
      , loader_errors        :: FontLoadLog
      }


-- | Print the loader errors from the 'FontLoadResult' to std-out.
--
printLoadErrors :: FontLoadResult -> IO ()
printLoadErrors = mapM_ putStrLn . toListH . getFontLoadLog . loader_errors

--------------------------------------------------------------------------------


-- | 'lookupFont' : @ name * font_table -> Maybe FontMetrics @ 
-- 
-- Lookup a font in the font_table.
-- 
lookupFont :: FontName -> FontTable -> Maybe FontMetrics
lookupFont name = Map.lookup name . getFontTable

-- | 'insertFont' : @ name * font_metrics * font_table -> FontTable @ 
-- 
-- Insert a named font into the font_table.
-- 
insertFont :: FontName -> FontMetrics -> FontTable -> FontTable
insertFont name ops = 
    FontTable . Map.insert name ops . getFontTable

-- | This ignores the Char code lookup and just returns the 
-- default advance vector.
--
monospace_metrics :: FontMetrics
monospace_metrics = FontMetrics
    { get_bounding_box          = \sz -> BBox (lowerLeft sz) (upperRight sz)
    , get_cw_table              = \sz _ -> hvec (upscale sz width_vec) 
    , get_cap_height            = \sz -> upscale sz cap_height
    , get_descender             = \sz -> upscale sz descender
    , get_underline_position    = \sz -> upscale sz underline_pos
    , get_underline_thickness   = \sz -> upscale sz underline_width
    }
  where
    llx             = (-23)  / 1000
    lly             = (-250) / 1000
    urx             = 715    / 1000
    ury             = 805    / 1000
    width_vec       = 600    / 1000
    cap_height      = 562    / 1000
    descender       = (-157) / 1000
    underline_pos   = (-100) / 1000
    underline_width = 50     / 1000

    upscale sz d    = d * fromIntegral sz
    lowerLeft sz    = P2 (upscale sz llx) (upscale sz lly) 
    upperRight sz   = P2 (upscale sz urx) (upscale sz ury) 


