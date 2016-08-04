{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.System.FontLoader.FontLoadMonad
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Font load monad handling IO (file system access), failure and 
-- logging.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.System.FontLoader.FontLoadMonad
  (
    FontLoadIO
  , runFontLoadIO
  , evalFontLoadIO
  , loadError
  , tellLoadMsg
  , promoteIO
  , promoteEither
  , runParserFLIO

  , sequenceAll

  -- * Font loading

  , buildAfmFontProps
  , checkFontPath
  
  ) where

import Wumpus.Basic.Kernel
import Wumpus.Basic.System.FontLoader.Datatypes
import Wumpus.Basic.Utils.ParserCombinators


import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Core.Text.GlyphIndices


import Control.Monad
import qualified Data.IntMap            as IntMap
import qualified Data.Map as Map
import Data.Monoid
import System.Directory
import System.FilePath



--------------------------------------------------------------------------------
-- FontLoadIO monad - IO plus Error



newtype FontLoadIO a = FontLoadIO { 
          getFontLoadIO :: IO (Either FontLoadMsg a, FontLoadLog ) }

instance Functor FontLoadIO where
  fmap f ma = FontLoadIO $ getFontLoadIO ma >>= \(a,w) -> return (fmap f a, w)
 
instance Monad FontLoadIO where
  return a = FontLoadIO $ return (Right a, mempty)
  m >>= k  = FontLoadIO $ getFontLoadIO m >>= fn 
              where
                fn (Left err, w) = return (Left err, w)
                fn (Right a, w1) = getFontLoadIO (k a) >>= \(b,w2) -> 
                                   return (b, w1 `mappend` w2)

runFontLoadIO :: FontLoadIO a -> IO (Either FontLoadMsg a, FontLoadLog)
runFontLoadIO ma = getFontLoadIO ma 


evalFontLoadIO :: FontLoadIO a -> IO (Either FontLoadMsg a)
evalFontLoadIO ma = liftM post $ getFontLoadIO ma
  where
    post (ans,_) = ans


loadError :: FontLoadMsg -> FontLoadIO a
loadError msg = FontLoadIO $ return (Left msg, mempty)

tellLoadMsg :: String -> FontLoadIO ()
tellLoadMsg msg = FontLoadIO $ return (Right (), fontLoadMsg msg ) 



-- | Promote an @IO@ action into the the @FontLoadIO@ monad.
--
-- This function is equivalent to @liftIO@.
--
promoteIO :: IO a -> FontLoadIO a
promoteIO ma = FontLoadIO $ ma >>= \a -> return (Right a, mempty)

promoteEither :: Either FontLoadMsg a -> FontLoadIO a
promoteEither = either loadError return 

runParserFLIO :: FilePath -> Parser Char a -> FontLoadIO a
runParserFLIO filepath p = 
   promoteIO (readFile filepath) >>= promoteEither . runParserEither p


-- | The standard monadic @sequence@ would finish on first fail
-- for the FontLoadIO monad. As we want to be able to sequence
-- the loading of a list of fonts, this is not really the 
-- behaviour we want for Wumpus. Instead we prefer to use fallback 
-- metrics and produce an inaccurate drawing on a font load error
-- rather than fail and produce no drawing.
--
sequenceAll :: [FontLoadIO a] -> FontLoadIO [a]
sequenceAll = FontLoadIO . step
   where
    step []     = return (Right [], mempty)
    step (m:ms) = liftM2 cons (getFontLoadIO m) (step ms) 

cons :: (Either FontLoadMsg a, FontLoadLog)
     -> (Either FontLoadMsg [a], FontLoadLog)
     -> (Either FontLoadMsg [a], FontLoadLog)
cons (Right a, w1)  (Right as, w2) = 
    (Right $ a:as,  w1 `mappend` w2)

cons (Right a, w1)  (Left e2, w2) = 
    (Right [a], w1 `mappend` w2 `mappend` fontLoadMsg e2)

cons (Left e1, w1)  (Right as, w2) = 
    (Right as, w1 `mappend` fontLoadMsg e1 `mappend` w2)

cons (Left e1, w1)  (Left e2,  w2) = 
    (Right [], w1 `mappend` fontLoadMsg e1 `mappend` w2 `mappend` fontLoadMsg e2)



--------------------------------------------------------------------------------


-- | Afm files do not have a default advance vec so use the 
-- monospace default.
-- 
-- Afm files hopefully have @CapHeight@ and @FontBBox@ properties
-- in the header. Use the monospace default only if they are 
-- missing.
-- 
buildAfmFontProps :: MonospaceDefaults AfmUnit 
                  -> AfmFile 
                  -> FontLoadIO (FontProps AfmUnit)
buildAfmFontProps defaults afm = do 
    cap_height    <- extractCapHeight   defaults afm
    desc_depth    <- extractDescender   defaults afm
    ul_position   <- extractUlPosition  defaults afm
    ul_thickness  <- extractUlThickness defaults afm
    bbox          <- extractFontBBox    defaults afm 
    return $ FontProps 
               { fp_bounding_box        = bbox
               , fp_default_adv_vec     = default_char_width defaults
               , fp_adv_vecs            = char_widths
               , fp_cap_height          = cap_height
               , fp_descender           = desc_depth
               , fp_underline_position  = ul_position
               , fp_underline_thickness = ul_thickness
               }  
  where
    char_widths = foldr fn IntMap.empty $ afm_glyph_metrics afm
 
    fn (AfmGlyphMetrics _ v ss) table = case Map.lookup ss ps_glyph_indices of
                                          Nothing -> table
                                          Just i  -> IntMap.insert i v table


extractCapHeight :: MonospaceDefaults AfmUnit -> AfmFile -> FontLoadIO AfmUnit
extractCapHeight defaults afm = maybe errk return $ afm_cap_height afm
  where
    errk = tellLoadMsg "WARNING - Could not extract CapHeight" >> 
           return (default_cap_height defaults)



extractDescender :: MonospaceDefaults AfmUnit -> AfmFile -> FontLoadIO AfmUnit
extractDescender defaults afm = maybe errk return $ afm_descender afm
  where
    errk = tellLoadMsg "WARNING - Could not extract Descender" >> 
           return (default_descender defaults)

extractUlPosition :: MonospaceDefaults AfmUnit -> AfmFile -> FontLoadIO AfmUnit
extractUlPosition defaults afm = 
    maybe errk return $ afm_underline_position afm
  where
    errk = tellLoadMsg "WARNING - Could not extract UnderlinePosition" >> 
           return (default_underline_position defaults)

extractUlThickness :: MonospaceDefaults AfmUnit -> AfmFile -> FontLoadIO AfmUnit
extractUlThickness defaults afm = 
    maybe errk return $ afm_underline_thickness afm
  where
    errk = tellLoadMsg "WARNING - Could not extract UnderlineThickness" >> 
           return (default_underline_thickness defaults)


extractFontBBox :: MonospaceDefaults AfmUnit -> AfmFile 
                -> FontLoadIO (BoundingBox AfmUnit)
extractFontBBox defaults afm = maybe errk return $ afm_letter_bbox afm
  where
    errk = tellLoadMsg "WARNING - Could not extract CapHeight" >> 
           return (default_letter_bbox defaults)



checkFontPath :: FilePath -> FilePath -> FontLoadIO FilePath
checkFontPath path_root font_file_name = 
    let full_path = normalise (path_root </> font_file_name)
    in do { check <- promoteIO (doesFileExist full_path)
          ; if check then return full_path
                     else loadError $ "Could not resolve path: " ++ full_path
          }
