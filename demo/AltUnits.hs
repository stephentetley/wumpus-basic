{-# OPTIONS -Wall #-}


module AltUnits where

import Wumpus.Basic.Kernel

import Wumpus.Core                              -- package: wumpus-core

import Data.Monoid
import System.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let pic1 = runCtxPictureU std_attr drawing01
    writeEPS "./out/alt_units01.eps" pic1
    writeSVG "./out/alt_units01.svg" pic1


std_attr :: DrawingContext
std_attr = (stroke_colour firebrick . fill_colour linen) $ standardContext 12


drawing01 :: CtxPicture
drawing01 = drawTracing $ combined


mf1 :: TraceDrawing Double ()
mf1 = do
    draw $ dblLocGraphic `at` P2 12 0

mf2 :: TraceDrawing Em ()
mf2 = do
    draw $ emLocGraphic `at` P2 1 0

combined :: TraceDrawing Double ()
combined = do 
    draw $ dblLocGraphic `at` P2 12 108
    drawl (P2 12 72) $ uconvF emLocGraphic
    draw $ uconvF $ emLocGraphic `at` P2 1 3
    drawl (P2 12 0) $ uconvF $ hyperlink haskell_org enLocGraphic

dblLocGraphic :: LocGraphic Double
dblLocGraphic = rect1 `mappend` rect2  
  where
    rect1 = dcRectangle DRAW_FILL_STROKE 36 24 
    rect2 = moveStart (hvec 36) $ dcRectangle DRAW_FILL_STROKE 60 24 



emLocGraphic :: LocGraphic Em
emLocGraphic = localize swap_colours $  rect1 `mappend` rect2  
  where
    rect1 = dcRectangle DRAW_FILL_STROKE 3 2 
    rect2 = moveStart (hvec 3) $ dcRectangle DRAW_FILL_STROKE 5 2 


enLocGraphic :: LocGraphic En
enLocGraphic = localize (fill_colour lemon_chiffon) $ rect1 `mappend` rect2  
  where
    rect1 = dcRectangle DRAW_FILL_STROKE 6 4
    rect2 = moveStart (hvec 6) $ dcRectangle DRAW_FILL_STROKE 10 4


haskell_org :: XLink
haskell_org = xlinkhref "http://www.haskell.org"

firebrick               :: RGBi
firebrick               = RGBi 0xb2 0x22 0x22

lemon_chiffon           :: RGBi
lemon_chiffon           = RGBi 0xff 0xfa 0xcd

linen                   :: RGBi
linen                   = RGBi 0xfa 0xf0 0xe6


