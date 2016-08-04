{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.DrawingPrimitives
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Primitive drawings - text, paths, lines, rectangles, disks, 
-- ellipses...
--
-- All the primitives take their drawing properties (colour, line 
-- width, etc.) from the DrawingContext.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.DrawingPrimitives
  (


  -- * Prim Paths
    locPP

  , emptyLocPP
  , vertexPP
  , curvePP


  , dcPath
  , dcOpenPath
  , dcClosedPath

  -- * Text
  , dcTextlabel
  , dcRTextlabel
  , dcEscapedlabel
  , dcREscapedlabel

  , hkernLine
  , vkernLine

  -- * Lines
  , straightLine
  , locStraightLine
  , curvedLine
  , straightConnector

  -- * Circles
  , dcCircle

  -- * Ellipses
  , dcEllipse
  , dcREllipse


  -- * Rectangles
  , dcRectangle

  -- * Disks  
  , dcDisk
  , dcEllipseDisk

  -- * Arc
  , dcArc


  ) where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.QueryDC
import Wumpus.Basic.Kernel.Base.WrappedPrimitive
import Wumpus.Basic.Kernel.Objects.Connector
import Wumpus.Basic.Kernel.Objects.Image
import Wumpus.Basic.Kernel.Objects.LocImage
import Wumpus.Basic.Kernel.Objects.LocThetaImage

import Wumpus.Core                              -- package: wumpus-core



-- Helpers


textPrim :: (RGBi -> FontAttr -> Primitive) -> Graphic u
textPrim fn = 
    textAttr >>= \(rgb,attr) -> primGraphic (prim1 $ fn rgb attr)
    


strokePrim :: (RGBi -> StrokeAttr -> Primitive) -> Graphic u
strokePrim fn = 
    strokeAttr >>= \(rgb,attr) -> primGraphic (prim1 $ fn rgb attr)
                     

fillPrim :: (RGBi -> Primitive) -> Graphic u
fillPrim fn = 
    fillAttr >>= \rgb -> primGraphic (prim1 $ fn rgb)

fillStrokePrim :: (RGBi -> StrokeAttr -> RGBi -> Primitive) 
               -> Graphic u
fillStrokePrim fn = 
    borderedAttr >>= \(frgb,attr,srgb) -> 
        primGraphic (prim1 $ fn frgb attr srgb)


textLoc :: InterpretUnit u 
        => (RGBi -> FontAttr -> DPoint2 -> Primitive) -> LocGraphic u
textLoc fn = promoteLoc $ \pt -> 
    normalizeCtxF pt >>= \dpt -> textPrim (\rgb attr -> fn rgb attr dpt)


strokeLoc :: InterpretUnit u
          => (RGBi -> StrokeAttr -> DPoint2 -> Primitive) -> LocGraphic u
strokeLoc fn = promoteLoc $ \pt -> 
    normalizeCtxF pt >>= \dpt -> strokePrim (\rgb attr -> fn rgb attr dpt)

fillLoc :: InterpretUnit u 
        => (RGBi -> DPoint2 -> Primitive) -> LocGraphic u
fillLoc fn = promoteLoc $ \pt ->
    normalizeCtxF pt >>= \dpt -> fillPrim (\rgb -> fn rgb dpt)

fillStrokeLoc :: InterpretUnit u
              => (RGBi -> StrokeAttr -> RGBi -> DPoint2 -> Primitive) 
              -> LocGraphic u
fillStrokeLoc fn = promoteLoc $ \pt ->
    normalizeCtxF pt >>= \dpt -> 
    fillStrokePrim (\frgb attr srgb -> fn frgb attr srgb dpt) 
 


textLocTheta :: InterpretUnit u 
             => (RGBi -> FontAttr -> DPoint2 -> Radian -> Primitive) 
             -> LocThetaGraphic u
textLocTheta fn = promoteLocTheta $ \pt ang -> 
    normalizeCtxF pt >>= \dpt -> textPrim (\rgb attr -> fn rgb attr dpt ang)




--------------------------------------------------------------------------------
-- Paths

-- Note - naming convention, the PP suffix is to avoid confusion 
-- with the Path data type in Wumpus-Drawing. These paths are
-- considered more /internal/.
--

-- | 'locPP' : @ [next_vector] -> LocImage PrimPath @
--
-- Create a path /query/ - i.e. a functional type 
-- /from Point to PrimPath/.
-- 
-- This is the analogue to 'vectorPath' in @Wumpus-Core@, but the 
-- result is produced /within/ the 'DrawingContext'.
--
locPP :: InterpretUnit u => [Vec2 u] -> LocQuery u PrimPath
locPP vs = qpromoteLoc $ \pt -> 
    vectorPrimPath <$> normalizeCtxF pt <*> mapM normalizeCtxF vs




-- | 'emptyLocPP' : @ (Point ~> PrimPath) @
--
-- Create an empty path /query/ - i.e. a functional type 
-- /from Point to PrimPath/.
--
-- This is the analogue to 'emptyPath' in @Wumpus-Core@, but the
-- result is produced /within/ the 'DrawingContext'.
--
emptyLocPP :: InterpretUnit u => LocQuery u PrimPath
emptyLocPP = locPP []




-- | 'vertexPP' : @ (Point ~> PrimPath) @
--
-- Create a PrimPath made of straight line segments joining the 
-- supplied points.
--
-- This is the analogue to 'vertexPrimPath' in @Wumpus-Core@, but 
-- it is polymorphic on unit.
--
vertexPP :: InterpretUnit u => [Point2 u] -> Query u PrimPath
vertexPP xs = vertexPrimPath <$> mapM normalizeCtxF xs



-- | 'curvePP' : @ (Point ~> PrimPath) @
--
-- Create a path made of curve segments joining the 
-- supplied points.
--
-- This is the analogue to 'curvedPrimPath' in @Wumpus-Core@, but 
-- it is polymorphic on unit.
--
curvePP :: InterpretUnit u => [Point2 u] -> Query u PrimPath
curvePP xs = curvedPrimPath <$> mapM normalizeCtxF xs


--------------------------------------------------------------------------------

--
-- Drawing paths (stroke, fill, fillStroke)...
--

dcPath :: PathMode -> PrimPath -> Graphic u
dcPath OSTROKE      = dcOpenPath
dcPath CSTROKE      = dcClosedPath DRAW_STROKE
dcPath CFILL        = dcClosedPath DRAW_FILL
dcPath CFILL_STROKE = dcClosedPath DRAW_FILL_STROKE



-- | 'dcOpenPath' : @ path -> Graphic @
--
-- This is the analogue to the 'ostroke' function in 
-- @Wumpus-Core@, but the drawing properties (colour, line width, 
-- etc.) are taken from the implicit 'DrawingContext'.
--
dcOpenPath :: PrimPath -> Graphic u
dcOpenPath pp = strokePrim (\rgb attr -> ostroke rgb attr pp)



-- | 'dcClosedPath' : @ DrawStyle * path -> Graphic @
--
-- Draw a closed path according to the supplied DrawStyle
-- ( fill | stroke | fill_stroke). 
---
-- Drawing properties (colour, line width, etc.) for the 
-- respective style are taken from the implicit 'DrawingContext'.
--
dcClosedPath :: DrawMode -> PrimPath -> Graphic u
dcClosedPath DRAW_FILL        pp = fillPrim (\rgb -> fill rgb pp)

dcClosedPath DRAW_STROKE      pp = strokePrim (\rgb attr -> cstroke rgb attr pp)

dcClosedPath DRAW_FILL_STROKE pp = 
    fillStrokePrim (\frgb attr srgb -> fillStroke frgb attr srgb pp)


--------------------------------------------------------------------------------
-- Text

-- | 'dcTextlabel' : @ string -> LocGraphic @
-- 
-- Create a text 'LocGraphic' - i.e. a functional type 
-- /from Point to Graphic/.
--
-- The implicit point of the LocGraphic is the baseline left.
--
-- This is the analogue to 'textlabel' in @Wumpus-core@, but the
-- text properties (font family, font size, colour) are taken from
-- the implicit 'DrawingContext'.
--
dcTextlabel :: InterpretUnit u => String -> LocGraphic u
dcTextlabel ss = textLoc (\rgb attr pt -> textlabel rgb attr ss pt)





-- | 'dcRTextlabel' : @ string -> LocThetaGraphic @
-- 
-- Create a text 'LocThetaGraphic' - i.e. a functional type 
-- /from Point and Angle to Graphic/.
--
-- The implicit point of the LocGraphic is the baseline left, the
-- implicit angle is rotation factor of the text.
--
-- Note - rotated text often does not render well in PostScript or
-- SVG. Rotated text should be used sparingly.
-- 
-- This is the analogue to 'rtextlabel' in @Wumpus-core@.
--
dcRTextlabel :: InterpretUnit u => String -> LocThetaGraphic u
dcRTextlabel ss =
    textLocTheta (\rgb attr pt ang -> rtextlabel rgb attr ss ang pt)


-- | 'dcEscapedlabel' : @ escaped_text -> LocGraphic @
-- 
-- Create a text 'LocGraphic' - i.e. a functional type 
-- /from Point to Graphic/.
--
-- The implicit point of the LocGraphic is the baseline left.
--
-- This is the analogue to 'escapedlabel' in @Wumpus-core@, but 
-- the text properties (font family, font size, colour) are taken 
-- from the implicit 'DrawingContext'.
--
dcEscapedlabel :: InterpretUnit u => EscapedText -> LocGraphic u
dcEscapedlabel esc =           
    textLoc (\rgb attr pt -> escapedlabel rgb attr esc pt)



-- | 'dcREscapedlabel' : @ escaped_text -> LocThetaGraphic @
-- 
-- Create a text 'LocThetaGraphic' - i.e. a functional type 
-- /from Point and Angle to Graphic/.
--
-- The implicit point of the LocGraphic is the baseline left, the
-- implicit angle is rotation factor of the text.
--
-- Note - rotated text often does not render well in PostScript or
-- SVG. Rotated text should be used sparingly.
-- 
-- This is the analogue to 'rescapedlabel' in @Wumpus-core@, but
-- the text properties (font family, font size, colour) are taken 
-- from the implicit 'DrawingContext'.
--
dcREscapedlabel :: InterpretUnit u => EscapedText -> LocThetaGraphic u
dcREscapedlabel esc = 
    textLocTheta (\rgb attr pt ang -> rescapedlabel rgb attr esc ang pt)




uconvKernChar :: InterpretUnit u => [KernChar u] -> Query u [KerningChar]
uconvKernChar = mapM mf
  where
    mf (u,ch) = (\u1 -> (u1,ch)) <$> normalizeCtx u



-- | 'hkernLine' : @ [kern_char] -> LocGraphic @
-- 
-- Create a horizontally kerned text 'LocGraphic' - i.e. a 
-- functional type /from Point to Graphic/.
--
-- The implicit point of the LocGraphic is the baseline left.
--
-- This is the analogue to 'hkernlabel' in @Wumpus-core@, but 
-- the text properties (font family, font size, colour) are taken 
-- from the implicit 'DrawingContext'.
--
hkernLine :: InterpretUnit u => [KernChar u] -> LocGraphic u
hkernLine ks = promoteLoc $ \pt -> 
    normalizeCtxF pt >>= \dpt -> liftQuery (uconvKernChar ks) >>= body dpt
  where
    body pt ans = textPrim (\rgb attr -> hkernlabel rgb attr ans pt)



-- | 'vkernLine' : @ [kern_char] -> LocGraphic @
-- 
-- Create a vertically kerned text 'LocGraphic' - i.e. a 
-- functional type /from Point to Graphic/.
--
-- The implicit point of the LocGraphic is the baseline left.
--
-- This is the analogue to 'vkernlabel' in @Wumpus-core@, but 
-- the text properties (font family, font size, colour) are taken 
-- from the implicit 'DrawingContext'.
--
vkernLine :: InterpretUnit u => [KernChar u] -> LocGraphic u
vkernLine ks = promoteLoc $ \pt -> 
    normalizeCtxF pt >>= \dpt -> liftQuery (uconvKernChar ks) >>= body dpt
  where
    body pt ans = textPrim (\rgb attr -> vkernlabel rgb attr ans pt)

--------------------------------------------------------------------------------
-- Lines

-- | 'straightLine' : @ start_point * end_point -> LocGraphic @ 
-- 
-- Create a straight line 'Graphic', the start and end point 
-- are supplied explicitly.
-- 
-- The line properties (colour, pen thickness, etc.) are taken 
-- from the implicit 'DrawingContext'.
-- 
straightLine :: InterpretUnit u => Point2 u -> Point2 u -> Graphic u
straightLine p1 p2 = liftQuery (vertexPP [p1,p2]) >>= dcOpenPath


-- | 'locStraightLine' : @ vec_to -> LocGraphic @ 
--
-- Create a stright line 'LocGraphic' - i.e. a functional type 
-- /from Point to Graphic/.
--
-- The implicit point of the LocGraphic is the start point, the 
-- end point is calculated by displacing the start point with the 
-- supplied vector.
--
-- The line properties (colour, pen thickness, etc.) are taken 
-- from the implicit 'DrawingContext'.
-- 
locStraightLine :: InterpretUnit u => Vec2 u -> LocGraphic u
locStraightLine v = promoteLoc $ \pt -> 
    liftQuery (qapplyLoc (locPP [v]) pt) >>= dcOpenPath



-- | 'curveLine' : @ start_point * control_point1 * 
--        control_point2 * end_point -> Graphic @ 
-- 
-- Create a Bezier curve 'Graphic', all control points are 
-- supplied explicitly.
-- 
-- The line properties (colour, pen thickness, etc.) are taken 
-- from the implicit 'DrawingContext'.
-- 
curvedLine :: InterpretUnit u
           => Point2 u -> Point2 u -> Point2 u -> Point2 u -> Graphic u
curvedLine p0 p1 p2 p3 = liftQuery (curvePP [p0,p1,p2,p3]) >>= dcOpenPath




-- | 'straightConnector' : @ start_point * end_point -> Connector @ 
-- 
-- Create a straight line 'Graphic', the start and end point 
-- are supplied implicitly.
-- 
-- The line properties (colour, pen thickness, etc.) are taken 
-- from the implicit 'DrawingContext'.
-- 
straightConnector :: InterpretUnit u => ConnectorGraphic u
straightConnector = promoteConn $ \p0 p1 -> 
    liftQuery (vertexPP [p0,p1]) >>= dcOpenPath



--------------------------------------------------------------------------------
-- Circles

-- | Helper for circle drawing.
--
circlePath :: InterpretUnit u 
         => u -> LocQuery u PrimPath
circlePath r = qpromoteLoc $ \pt -> 
    (\dr dpt -> curvedPrimPath $ bezierCircle dr dpt) 
      <$> normalizeCtx r <*> normalizeCtxF pt


-- | 'dcCircle' : @ DrawStyle * radius -> LocGraphic @
--
-- Create a circle 'LocGraphic' - the implicit point is 
-- center. The circle is drawn with four Bezier curves. 
-- 
-- The respective line or fill properties for the 'DrawStyle' are 
-- taken from the implicit 'DrawingContext'.
-- 
dcCircle :: InterpretUnit u => DrawMode -> u -> LocGraphic u
dcCircle style r = promoteLoc $ \pt -> 
    liftQuery (qapplyLoc (circlePath r) pt) >>= dcClosedPath style



--------------------------------------------------------------------------------
-- Ellipses


-- | Helper for ellipse drawing.
--
ellipsePath :: InterpretUnit u 
            => u -> u -> LocQuery u PrimPath
ellipsePath rx ry = qpromoteLoc $ \pt -> 
    (\drx dry dpt -> curvedPrimPath $ bezierEllipse drx dry dpt) 
      <$> normalizeCtx rx <*> normalizeCtx ry <*> normalizeCtxF pt


-- | Helper for ellipse drawing.
--
rellipsePath :: InterpretUnit u 
            => u -> u -> LocThetaQuery u PrimPath
rellipsePath rx ry = qpromoteLocTheta $ \pt ang ->
    (\drx dry dpt -> curvedPrimPath $ rbezierEllipse drx dry ang dpt) 
      <$> normalizeCtx rx <*> normalizeCtx ry <*> normalizeCtxF pt


-- | 'strokedEllipse' : @ x_radius * y_radius -> LocGraphic @
--
-- Create a stroked ellipse 'LocGraphic' - the implicit point is 
-- center. The ellipse is drawn with four Bezier curves. 
-- 
-- The line properties (colour, pen thickness, etc.) are taken 
-- from the implicit 'DrawingContext'.
-- 
dcEllipse :: InterpretUnit u => DrawMode -> u -> u -> LocGraphic u
dcEllipse style rx ry = promoteLoc $ \pt ->
   liftQuery (qapplyLoc (ellipsePath rx ry) pt) >>= dcClosedPath style 


-- | 'dcREllipse' : @ x_radius * y_radius -> LocGraphic @
--
-- Create a bordered ellipse 'LocThetaGraphic' - the implicit point
-- is center and the angle is rotation about the center. The 
-- ellipse is drawn with four Bezier curves.  
-- 
-- The background fill colour and the outline stroke properties 
-- are taken from the implicit 'DrawingContext'.
-- 
dcREllipse :: InterpretUnit u
           => DrawMode -> u -> u -> LocThetaGraphic u
dcREllipse style rx ry = promoteLocTheta $ \pt ang -> 
    liftQuery (qapplyLocTheta (rellipsePath rx ry) pt ang) >>= 
    dcClosedPath style


-- Note - clipping needs some higher level path object than is defined here.

--------------------------------------------------------------------------------
-- Rectangles

-- | Supplied point is /bottom-left/.
--
rectanglePath :: InterpretUnit u 
              => u -> u -> LocQuery u PrimPath
rectanglePath w h = locPP [hvec w, vvec h, hvec (-w)]



-- | 'strokedRectangle' : @ style * width * height -> LocGraphic @
--
-- Create a stroked rectangle 'LocGraphic' - the implicit point is 
-- bottom-left. 
-- 
-- The line properties (colour, pen thickness, etc.) are taken 
-- from the implicit 'DrawingContext'.
-- 
dcRectangle :: InterpretUnit u => DrawMode -> u -> u -> LocGraphic u
dcRectangle style w h = promoteLoc $ \pt -> 
    liftQuery (qapplyLoc (rectanglePath w h) pt) >>= dcClosedPath style


---------------------------------------------------------------------------

-- | 'dcDisk' : @ radius -> LocGraphic @
--
-- Create a circle 'LocGraphic' - the implicit point is the 
-- center. 
-- 
-- This is a efficient representation of circles using 
-- PostScript\'s @arc@ or SVG\'s @circle@ in the generated 
-- output. However, stroked-circles do not draw well after 
-- non-uniform scaling - the pen width is scaled as well as 
-- the shape.
--
-- For stroked circles that can be adequately scaled, use 
-- 'dcCircle' instead.
--
-- The fill or stroke properties for the respective DrawStyle are
-- taken from the implicit 'DrawingContext'.
-- 
dcDisk :: InterpretUnit u => DrawMode -> u -> LocGraphic u
dcDisk DRAW_FILL radius = 
    normalizeCtx radius >>= \r -> 
    fillLoc (\rgb pt -> fillEllipse rgb r r pt)

dcDisk DRAW_STROKE radius = 
    normalizeCtx radius >>= \r -> 
    strokeLoc (\rgb attr pt -> strokeEllipse rgb attr r r pt)

dcDisk DRAW_FILL_STROKE radius = 
    normalizeCtx radius >>= \r -> 
    fillStrokeLoc (\frgb attr srgb pt -> fillStrokeEllipse frgb attr srgb r r pt)




-- | 'strokeEllipseDisk' : @ x_radius * y_radius -> LocGraphic @
--
-- Create a stroked ellipse 'LocGraphic' - the implicit point is 
-- the center. 
-- 
-- This is a efficient representation of circles using 
-- PostScript\'s @arc@ or SVG\'s @ellipse@ in the generated 
-- output. However, stroked ellipses do not draw well after 
-- non-uniform scaling - the pen width is scaled as well as 
-- the shape.
--
-- For stroked ellipses that can be adequately scaled, use 
-- 'strokedEllipse' instead.
--
-- The line properties (colour, pen thickness, etc.) are taken 
-- from the implicit 'DrawingContext'.
-- 
dcEllipseDisk :: InterpretUnit u 
              => DrawMode -> u -> u -> LocGraphic u
dcEllipseDisk style rx ry = 
    normalizeCtx rx >>= \drx -> 
    normalizeCtx ry >>= \dry -> 
    case style of
      DRAW_FILL -> fillLoc (\rgb pt -> fillEllipse rgb drx dry pt)
      DRAW_STROKE -> strokeLoc 
                       (\rgb attr pt -> strokeEllipse rgb attr drx dry pt)
      DRAW_FILL_STROKE -> fillStrokeLoc $ 
                       (\frgb attr srgb pt -> 
                           fillStrokeEllipse frgb attr srgb drx dry pt)



--------------------------------------------------------------------------------



-- | dcArc : radius * apex_angle
-- 
-- Always open-stroked.
--
dcArc :: (Floating u, InterpretUnit u) => u -> Radian -> LocThetaGraphic u
dcArc radius ang = promoteLocTheta $ \pt inclin -> 
    let ps = bezierArcPoints ang radius inclin pt
    in liftQuery (curvePP ps) >>= dcOpenPath
