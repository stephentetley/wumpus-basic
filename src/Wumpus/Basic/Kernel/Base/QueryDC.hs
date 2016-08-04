{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Base.QueryDC
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Querying the Drawing Context.
--
-- \*\* WARNING \*\* - parts of this module especially the 
-- mono-space glyph metrics need a re-think and will change or be 
-- dropped.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Base.QueryDC
  ( 

    normalizeCtx
  , normalizeCtxF
  , dinterpCtx
  , dinterpCtxF

  , uconvertCtx1
  , uconvertCtxF

  , pointSize

  , strokeAttr
  , fillAttr
  , borderedAttr
  , textAttr

  , position
  , snapmove

  , textMargin

  , getLineWidth
  , getFontAttr
  , getFontSize
  , getFontFace
  , getTextColour

  , textlineSpace

  -- * Glyph metrics
  , glyphBoundingBox
  , capHeight
  , descender
  , underlinePosition
  , underlineThickness
  , verticalSpan
  , heightSpan

  -- * Text metrics
  , escTextVector
  , escCharVector
  , hkernVector

  , cwLookupTable

  ) where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Base.FontSupport

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Core.Text.GlyphIndices

import Data.VectorSpace                         -- package: vector-space
 
import Data.Char
import qualified Data.Map               as Map
import Data.Maybe 


--
-- NOTE 20.5.11 - The normalize functions are not satisfactory,
-- point size is too promiscuous for evaluation contextual units.
--

normalizeCtx :: (DrawingCtxM m, InterpretUnit u) => u -> m Double
normalizeCtx u = (\sz -> normalize sz u) <$> pointSize

normalizeCtxF :: (DrawingCtxM m, Functor t, InterpretUnit u) 
             => t u -> m (t Double)
normalizeCtxF t = (\sz -> fmap (normalize sz) t) <$> pointSize

dinterpCtx :: (DrawingCtxM m, InterpretUnit u) => Double -> m u
dinterpCtx u = (\sz -> dinterp sz u) <$> pointSize

dinterpCtxF :: (DrawingCtxM m, Functor t, InterpretUnit u) => t Double -> m (t u)
dinterpCtxF u = (\sz -> fmap (dinterp sz) u) <$> pointSize

uconvertCtx1 :: (DrawingCtxM m, InterpretUnit u, InterpretUnit u1) 
             => u -> m u1
uconvertCtx1 t = (\sz -> uconvert1 sz t) <$> pointSize

uconvertCtxF :: (DrawingCtxM m, Functor t, InterpretUnit u, InterpretUnit u1) 
            => t u -> m (t u1)
uconvertCtxF t = (\sz -> uconvertF sz t) <$> pointSize


pointSize :: DrawingCtxM m => m FontSize
pointSize = asksDC dc_font_size

strokeAttr :: DrawingCtxM m => m (RGBi, StrokeAttr)
strokeAttr = (,) <$> asksDC dc_stroke_colour <*> asksDC dc_stroke_props

fillAttr :: DrawingCtxM m => m RGBi
fillAttr = asksDC dc_fill_colour


borderedAttr :: DrawingCtxM m => m (RGBi, StrokeAttr, RGBi)
borderedAttr = (,,) <$> asksDC dc_fill_colour 
                    <*> asksDC dc_stroke_props 
                    <*> asksDC dc_stroke_colour



textAttr :: DrawingCtxM m => m (RGBi,FontAttr)
textAttr = 
    (\a b c -> (a, FontAttr b c)) 
      <$> asksDC dc_text_colour <*> asksDC dc_font_size <*> asksDC dc_font_face




-- | Get the Point corresponding the grid coordinates scaled by
-- the snap-grid scaling factors.
--
position :: (DrawingCtxM m, Fractional u) => (Int, Int) -> m (Point2 u)
position (x,y) = post <$> asksDC dc_snap_grid_factors
  where
    post (sx,sy) = P2 (realToFrac $ sx * fromIntegral x) 
                      (realToFrac $ sy * fromIntegral y)




-- | Scale a vector coordinate by the snap-grid scaling factors.
--
-- Absolute units.
--
snapmove :: (DrawingCtxM m, Fractional u) => (Int,Int) -> m (Vec2 u)
snapmove (x,y) = post <$> asksDC dc_snap_grid_factors
  where
    post (sx,sy) = V2 (realToFrac $ sx * fromIntegral x) 
                      (realToFrac $ sy * fromIntegral y)



-- | Get the (x,y) margin around text.
--
-- Note - not all text operations in Wumpus are drawn with text 
-- margin. 
-- 
textMargin :: (DrawingCtxM m, InterpretUnit u) => m (u,u)
textMargin = post <$> asksDC dc_font_size <*> asksDC dc_text_margin
  where
    post sz (TextMargin xem yem) = (uconvert1 sz xem, uconvert1 sz yem)





getLineWidth :: DrawingCtxM m => m Double
getLineWidth = line_width <$> asksDC dc_stroke_props

getFontAttr :: DrawingCtxM m => m FontAttr
getFontAttr = FontAttr <$> asksDC dc_font_size <*> asksDC dc_font_face


getFontSize     :: DrawingCtxM m => m Int
getFontSize     = asksDC dc_font_size

getFontFace     :: DrawingCtxM m => m FontFace
getFontFace     = asksDC dc_font_face

getTextColour   :: DrawingCtxM m => m RGBi
getTextColour   = asksDC dc_text_colour



-- | Vertical distance between descender of a line and the 
-- cap-height of the line below. 
-- 
textlineSpace :: (DrawingCtxM m, InterpretUnit u) => m u
textlineSpace = 
    post <$> asksDC dc_font_size <*> asksDC dc_line_spacing_factor
  where
    post sz factor = dinterp sz ((fromIntegral sz) * (realToFrac factor))

--------------------------------------------------------------------------------


glyphQuery :: DrawingCtxM m => (FontMetrics -> FontSize -> a) -> m a
glyphQuery fn = (\ctx -> withFontMetrics fn ctx) <$> askDC



-- | Get the font bounding box - this is the maximum boundary of 
-- the glyphs in the font. The span of the height is expected to 
-- be bigger than the cap_height plus descender depth.
--
glyphBoundingBox :: (DrawingCtxM m, InterpretUnit u) => m (BoundingBox u)
glyphBoundingBox = 
    uconvertF <$> asksDC dc_font_size <*> glyphQuery get_bounding_box



-- | Height of a capital letter.
--
capHeight :: (DrawingCtxM m, InterpretUnit u) => m u
capHeight = dinterp <$> asksDC dc_font_size <*> glyphQuery get_cap_height


-- | Note - descender is expected to be negative.
--
descender :: (DrawingCtxM m, InterpretUnit u) => m u
descender = dinterp <$> asksDC dc_font_size <*> glyphQuery get_descender


-- | Note - underline_position is expected to be negative.
--
underlinePosition :: (DrawingCtxM m, InterpretUnit u) => m u
underlinePosition = 
    dinterp <$> asksDC dc_font_size <*> glyphQuery get_underline_position


-- | Line width of underline line.
--
underlineThickness :: (DrawingCtxM m, InterpretUnit u) => m u
underlineThickness = 
    dinterp <$> asksDC dc_font_size <*> glyphQuery get_underline_thickness


-- | This is the distance from cap_height to descender.
--
verticalSpan :: (DrawingCtxM m, InterpretUnit u) => m u
verticalSpan = 
    (\ch dd -> ch - dd) <$> capHeight <*> descender



-- | Variant of 'verticalSpan' that accounts for the specified
-- 'TextHeight'.
--
-- This returns a pair of @(yminor, ymajor)@.
-- 
heightSpan :: (DrawingCtxM m, InterpretUnit u )
           => TextHeight -> m (u,u)
heightSpan JUST_CAP_HEIGHT           = (\ymaj -> (0, ymaj)) <$> capHeight
heightSpan CAP_HEIGHT_PLUS_DESCENDER = 
    (\ymin ymaj -> (abs ymin, ymaj)) <$> descender <*> capHeight



--------------------------------------------------------------------------------



-- | Find the advance vector for the supplied 'EscapedText'.
--
-- Note - the text assumed to be a single line.
-- 
escTextVector :: (DrawingCtxM m, InterpretUnit u) 
              => EscapedText -> m (Vec2 u)
escTextVector esc = 
    cwLookupTable >>= \table -> 
    pointSize     >>= \sz    -> 
    let cs = destrEscapedText id esc 
    in return $ foldr (step sz table) (vec 0 0) cs
  where
    step sz table ch v = let cv = escCharWidth sz table ch in v ^+^ cv



-- | Find the advance vector for the supplied 'EscapedChar'.
--
escCharVector :: (DrawingCtxM m, InterpretUnit u) 
           => EscapedChar -> m (Vec2 u)
escCharVector ch = 
    (\table sz -> escCharWidth sz table ch) <$> cwLookupTable <*> pointSize


-- | This is outside the Drawing context as we don\'t want to get
-- the @cwLookupTable@ for every char.
--
escCharWidth :: InterpretUnit u 
             => FontSize -> CharWidthLookup -> EscapedChar -> Vec2 u
escCharWidth sz fn (CharLiteral c) = fmap (dinterp sz) $ fn $ ord c
escCharWidth sz fn (CharEscInt i)  = fmap (dinterp sz) $ fn i
escCharWidth sz fn (CharEscName s) = fmap (dinterp sz) $ fn ix
  where
    ix = fromMaybe (-1) $ Map.lookup s ps_glyph_indices




-- | 'hkernVector' : @ [kerning_char] -> AdvanceVec @
-- 
-- 'hkernvector' takes whatever length is paired with the 
-- EscapedChar for the init of the the list, for the last element 
-- it takes the charVector.
--
hkernVector :: (DrawingCtxM m, InterpretUnit u) 
            => [KernChar u] -> m (Vec2 u)
hkernVector = go 0
  where
    go w []             = return $ V2 w 0
    go w [(dx,ch)]      = fmap (addWidth $ w + dx) (escCharVector ch)
    go w ((dx,_ ):xs)   = go (w + dx) xs
    
    addWidth w (V2 x y) = V2 (w+x) y



-- | Note the CharWidthLookup is not parameteric on units.
--
-- /CharWidth/ is always Double representing PostScript points.
-- Client code must convert this value accordingly.
--
cwLookupTable :: DrawingCtxM m => m CharWidthLookup
cwLookupTable = glyphQuery get_cw_table
