{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.System.FontLoader.AfmTopLevel
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Top-level AFM V4.1 font loader. 
--
-- Use this module to build a font loader if you want to work with 
-- the Adobe metrics sets, but find the @simpleFontLoader@ in 
-- @Wumpus.Basic.System.FontLoader@ too inflexible.
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.System.FontLoader.AfmTopLevel
  ( 

    loadAfmFontMetrics

  , loadAfmFont1  

  ) where

import Wumpus.Basic.Kernel
import Wumpus.Basic.System.FontLoader.AfmV4Dot1Parser
import Wumpus.Basic.System.FontLoader.Datatypes
import Wumpus.Basic.System.FontLoader.FontLoadMonad


import Wumpus.Core                              -- package: wumpus-core

import Control.Monad

-- The file names of the Afm fonts match there PostScript names,
-- the only difference is the addition of a @.afm@ extension.
--

-- | 'loadAfmFontMetrics' : 
-- @ path_to_afm_fonts * [font_name] -> IO FontLoadResult @ 
-- 
-- Load the supplied list of fonts. 
-- 
-- Note - if a font fails to load a message is written to the 
-- log and monospaced /fallback metrics/ are used.
--
loadAfmFontMetrics :: FilePath -> [FontDef] -> IO FontLoadResult
loadAfmFontMetrics font_dir_path ds = 
    liftM post $ runFontLoadIO $ sequenceAll $ map mkFun ds
  where
    mkFun                = afmLoadFontMetrics font_dir_path  
 
    post (Left err,msgs) = let errs = fontLoadMsg err `mappend` msgs
                           in FontLoadResult mempty errs
    post (Right xs,msgs) = let body = foldr fn mempty xs
                           in FontLoadResult body msgs
    
    fn (name,metrics) table = insertFont name metrics table


-- TODO - need a one font version...


-- | 'loadAfmFont1' : 
-- @ path_to_afm_fonts * font_def -> IO FontLoadResult @ 
-- 
-- Load a single AFM font. 
-- 
-- Note - if the font fails to load a message is written to the 
-- log and monospaced /fallback metrics/ are used.
--
loadAfmFont1 :: FilePath -> FontDef -> IO FontLoadResult
loadAfmFont1 font_dir_path font_def =
    liftM post $ runFontLoadIO $ afmLoadFontMetrics font_dir_path font_def
  where
    post (Left err,msgs)    = let errs = fontLoadMsg err `mappend` msgs
                              in FontLoadResult mempty errs
    post (Right (a,b),msgs) = let body = insertFont a b mempty
                              in FontLoadResult body msgs
    


afmLoadFontMetrics :: FilePath -> FontDef -> FontLoadIO (FontName,FontMetrics)
afmLoadFontMetrics font_dir_path font_def = do
    tellLoadMsg  $ "Loading " ++ afm_file
    path        <- checkFontPath font_dir_path afm_file
    ans         <- runParserFLIO path afmV4Dot1Parser
    props       <- buildAfmFontProps  afm_mono_defaults_4_1 ans
    return (name, buildMetricsOps afmValue props)
  where
    afm_file    = afm_file_name font_def
    name        = ps_font_name $ font_def_face font_def


-- | These are values extracted from Courier in the core 14 fonts.
--
afm_mono_defaults_4_1 :: MonospaceDefaults AfmUnit
afm_mono_defaults_4_1 = 
    MonospaceDefaults { default_letter_bbox         = bbox
                      , default_cap_height          = 562
                      , default_descender           = (-157)
                      , default_underline_position  = (-100)
                      , default_underline_thickness = 50 
                      , default_char_width          = V2 600 0
                      }
  where
    bbox = BBox (P2 (-23) (-250)) (P2 715 805)