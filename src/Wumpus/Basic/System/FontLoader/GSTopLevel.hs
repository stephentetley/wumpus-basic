{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.System.FontLoader.GSTopLevel
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Top-level GhostScript font loader. 
--
-- Use this module to build a font loader if you want to work with 
-- GhostScript, but find the @simpleFontLoader@ in 
-- @Wumpus.Basic.System.FontLoader@ too inflexible.
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.System.FontLoader.GSTopLevel
  ( 

    loadGSFontMetrics

  , loadGSFont1 
   
  ) where

import Wumpus.Basic.Kernel
import Wumpus.Basic.System.FontLoader.AfmV2Parser
import Wumpus.Basic.System.FontLoader.Datatypes
import Wumpus.Basic.System.FontLoader.FontLoadMonad

import Wumpus.Core                              -- package: wumpus-core

import Control.Monad
import Data.Monoid


-- | 'loadGSFontMetrics' : 
-- @ path_to_gs_fonts * [font_name] -> IO FontLoadResult @ 
-- 
-- Load the supplied list of fonts. 
-- 
-- Note - if a font fails to load a message is written to the 
-- log and monospaced /fallback metrics/ are used.
--
loadGSFontMetrics :: FilePath -> [FontDef] -> IO FontLoadResult
loadGSFontMetrics font_dir_path ds = 
    liftM post $ runFontLoadIO $ sequenceAll $ map mkFun ds
  where
    mkFun = gsLoadFontMetrics font_dir_path  

    post (Left err,msgs) = let errs = fontLoadMsg err `mappend` msgs
                           in FontLoadResult mempty errs 
    post (Right xs,msgs) = let body = foldr fn mempty xs
                           in FontLoadResult body msgs

    fn (name,metrics) table = insertFont name metrics table


-- | 'loadGSFont1' : 
-- @ path_to_gs_fonts * font_name -> IO FontLoadResult @ 
-- 
-- Load a single GhostScript font. 
-- 
-- Note - if the font fails to load a message is written to the 
-- log and monospaced /fallback metrics/ are used.
--
loadGSFont1 :: FilePath -> FontDef -> IO FontLoadResult
loadGSFont1 font_dir_path font_def = 
   liftM post $ runFontLoadIO $ gsLoadFontMetrics font_dir_path font_def
  where
    post (Left err,msgs)    = let errs = fontLoadMsg err `mappend` msgs
                              in FontLoadResult mempty errs 
    post (Right (a,b),msgs) = let body = insertFont a b mempty
                              in FontLoadResult body msgs



gsLoadFontMetrics :: FilePath -> FontDef
                  -> FontLoadIO (FontName,FontMetrics)
gsLoadFontMetrics font_dir_path font_def = do
    tellLoadMsg  $ "Loading " ++ gs_file
    path        <- checkFontPath font_dir_path gs_file
    ans         <- runParserFLIO path afmV2Parser
    props       <- buildAfmFontProps  ghostscript_mono_defaults_8_54 ans
    return (name, buildMetricsOps afmValue props)
  where
    gs_file     = gs_file_name font_def
    name        = ps_font_name $ font_def_face font_def



-- | These are values extracted from the file @n022003l.afm@
-- which is the font @NimbusMonL-Regu@, GhostScript\'s eqivalent 
-- font for the core 14 font Courier.
--
ghostscript_mono_defaults_8_54 :: MonospaceDefaults AfmUnit
ghostscript_mono_defaults_8_54 = 
    MonospaceDefaults { default_letter_bbox         = bbox
                      , default_cap_height          = 563
                      , default_descender           = (-186)
                      , default_underline_position  = (-100)
                      , default_underline_thickness = 50 
                      , default_char_width          = V2 600 0
                      }
  where
    bbox = BBox (P2 (-46) (-273)) (P2 650 820)