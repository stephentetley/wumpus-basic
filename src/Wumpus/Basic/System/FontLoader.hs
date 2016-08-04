{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.System.FontLoader
-- Copyright   :  (c) Stephen Tetley 2011-2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Top level module for font loading...
--
--------------------------------------------------------------------------------

module Wumpus.Basic.System.FontLoader
  (
    FontLoader
  , afmLoaderByEnv
  , gsLoaderByEnv
  , simpleFontLoader

  , default_font_loader_help

  ) where

import Wumpus.Basic.Kernel.Base.FontSupport  
import Wumpus.Basic.System.FontLoader.AfmTopLevel
import Wumpus.Basic.System.FontLoader.GSTopLevel

import Control.Monad
import Control.Exception ( try )
import System.Environment
-- import System.IO.Error




-- | A FontLoader is an action from a list of fonts to a
-- 'FontLoadResult' returned in @IO@.
-- 
-- Fonts are supplied in a list of @Either FontDef FontFamily@,
-- this is a little cumbersome but it allows the loader to load
-- individual fonts and \/ or a whole families with a single API
-- call.
--
type FontLoader = [Either FontDef FontFamily] -> IO FontLoadResult





-- | Environment variable pointing to the GhostScript font
-- directory.
-- 
-- > WUMPUS_GS_FONT_DIR
--
wumpus_gs_font_dir :: String
wumpus_gs_font_dir = "WUMPUS_GS_FONT_DIR"


-- | Environment variable pointing to the diretory containing 
-- the Adobe Font Metrics files.
-- 
-- > WUMPUS_AFM_FONT_DIR
--
wumpus_afm_font_dir :: String
wumpus_afm_font_dir = "WUMPUS_AFM_FONT_DIR"



afmLoaderByEnv :: IO (Maybe FontLoader)
afmLoaderByEnv = do 
    mb <- envLookup wumpus_afm_font_dir
    case mb of 
      Nothing   -> return Nothing
      Just path -> return $ Just (\xs -> loadAfmFontMetrics path $ fontList xs)


gsLoaderByEnv :: IO (Maybe FontLoader)
gsLoaderByEnv = do
    mb <- envLookup wumpus_gs_font_dir
    case mb of
      Nothing   -> return Nothing
      Just path -> return $ Just (\xs -> loadGSFontMetrics path $ fontList xs)


-- | Tries to find the GhostScript metrics first...
--
-- Runs the IO action on the loader if it finds one.
--
-- Either of one of the environment variables 
-- @WUMPUS_AFM_FONT_DIR@ or @WUMPUS_GS_FONT_DIR@ must be defined
-- and point to their respective directory. 
-- 
simpleFontLoader :: (FontLoader -> IO a) ->  IO (Maybe a)
simpleFontLoader mf = 
    gsLoaderByEnv >>= maybe fk1 sk 
  where
   fk1       = afmLoaderByEnv >>= maybe fk2 sk
   fk2       = putStrLn default_font_loader_help >> return Nothing
   sk loader = mf loader >>= return . Just


envLookup :: String -> IO (Maybe String)
envLookup name = liftM fn $ try $ getEnv name
  where
    fn :: Either IOError String -> Maybe String
    fn (Left _)  = Nothing
    fn (Right a) = Just a

   

fontList :: [Either FontDef FontFamily] -> [FontDef]
fontList = foldr fn []
  where
    fn (Left a)  acc = a:acc
    fn (Right b) acc = let f1 = maybe id (\a -> (a:)) $ ff_bold b
                           f2 = maybe id (\a -> (a:)) $ ff_italic b
                           f3 = maybe id (\a -> (a:)) $ ff_bold_italic b
                     in ff_regular b : (f1 $ f2 $ f3 acc)


default_font_loader_help :: String
default_font_loader_help = unlines $ 
    [ "This example uses glyph metrics loaded at runtime."
    , "It can use either the metrics files supplied with GhostScript,"
    , "or the AFM v4.1 metrics for the Core 14 fonts available from"
    , "Adobe's website."
    , "" 
    , "To use GhostScripts font metrics set the environemt variable"
    , wumpus_gs_font_dir ++ " to point to the GhostScript fonts"
    , "directory (e.g. /usr/share/ghostscript/fonts)."
    , ""
    , "To use the Adode Core 14 font metrics download the archive from"
    , "the Adobe website and set the environment variable "
    , wumpus_afm_font_dir ++ " to point to it."
    , ""
    , "If you have both environment variables set, the GhostScript loader"
    , "will be used."
    ]

