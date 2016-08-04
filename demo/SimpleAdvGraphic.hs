{-# OPTIONS -Wall #-}


module SimpleAdvGraphic where

import Wumpus.Basic.Kernel

import Wumpus.Core                      -- package: wumpus-core

import Control.Applicative
import System.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let pic1 = runCtxPictureU std_attr drawing01
    writeEPS "./out/simple_adv_graphic01.eps" pic1
    writeSVG "./out/simple_adv_graphic01.svg" pic1


std_attr :: DrawingContext
std_attr = standardContext 24


drawing01 :: CtxPicture
drawing01 = drawTracing mf 



-- Although TraceDrawing can be fully unit polymorphic, it seems 
-- always best to specialize as we are stating concrete values
-- (and they will be in some unit).
--

mf :: TraceDrawing Double ()
mf = do
    drawl (P2 0 120) $ 
        runAdvObject $ evenspace (hvec 10) [text01, text02, text01]

    drawl (P2 0 80) $ 
        runAdvObject $ advances [text01, text02, text01]

    drawl (P2 0 40) $ 
        runAdvObject (miniDisk `advance` text01 `advance` miniDisk)

    drawl (P2 0 0) $
        runAdvObject (miniDisk `advance` text02 `advance` miniDisk)


-- Normally, text calculate the advance vector from the font 
-- metrics...
--
text01 :: AdvGraphic Double
text01 = makeAdvObject (pure $ hvec 84) $ dcTextlabel "text01"
    

text02 :: AdvGraphic Double
text02 = makeAdvObject (pure $ hvec 210) $ dcTextlabel "text number two"


miniDisk :: AdvGraphic Double
miniDisk = makeAdvObject (pure $ V2 0 0) disk1 
  where
    disk1 = localize (fill_colour sienna) $ dcDisk DRAW_FILL 3


sienna :: RGBi
sienna = RGBi 160 82 45