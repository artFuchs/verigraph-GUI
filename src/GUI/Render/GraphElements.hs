-- | module containing functions related with the rendering of a graph
module GUI.Render.GraphElements(
  renderNode
, renderEdge
)where

import GI.Cairo hiding (Context)
import qualified GI.Cairo as Cairo
import GI.Cairo.Render
import GI.Cairo.Render.Connector
import qualified GI.Pango as Pango
import qualified GI.PangoCairo as Pango

import qualified Data.Text as T
import Data.List
import GUI.Data.GraphicalInfo
import GUI.Helper.Geometry
import Control.Monad


type Quadruple a = (a, a, a, a)
type GIRect = Quadruple Double

-- draw a node with it's label
renderNode :: NodeGI -> String -> Maybe GIColor -> Maybe  GIColor -> Render ()
renderNode node content shadowColor textColor = do
  let (x,y) = position node
      (r,g,b) = fillColor node
      (rl,gl,bl) = lineColor node
      (pw,ph) = dims node

  setLineWidth 2
  setSourceRGB r g b
  case shape node of
    NCircle -> let radius = (max pw ph)/2 in renderCircle (x,y) radius (r,g,b) (rl,gl,bl) shadowColor
    NRect -> renderRectangle (x, y, pw, ph) (r,g,b) (rl,gl,bl) shadowColor
    NSquare -> renderRectangle (x,y, (max pw ph), (max pw ph)) (r,g,b) (rl,gl,bl) shadowColor


  moveTo (x-(pw/2-2)) (y-(ph/2-2))

  case textColor of
    Nothing -> renderNodeLabel content (rl,gl,bl) "Sans Regular 10"
    Just color -> renderNodeLabel content color "Sans Bold 10"

renderCircle :: GIPos -> Double -> GIColor -> GIColor -> Maybe GIColor -> Render ()
renderCircle (x,y) radius (r,g,b) (lr,lg,lb) shadowColor  = do
  case shadowColor of
    Nothing -> return ()
    Just (sr,sg,sb) -> do
      setSourceRGB sr sg sb
      arc x y (radius+3) 0 (2*pi)
      fill

  setSourceRGB r g b
  arc x y radius 0 (2*pi)
  fill
  setSourceRGB lr lg lb
  arc x y radius 0 (2*pi)
  stroke

renderRectangle :: GIRect -> GIColor -> GIColor -> Maybe GIColor ->  Render ()
renderRectangle (x,y,w,h) (r,g,b) (lr,lg,lb) shadowColor = do
  case shadowColor of
    Nothing -> return ()
    Just (sr,sg,sb) -> do
      setSourceRGB sr sg sb
      rectangle (x-(w/2+3)) (y-(h/2+3)) (w+6) (h+6)
      fill
  setSourceRGB r g b
  rectangle (x-(w/2)) (y-(h/2)) w h
  fill
  setSourceRGB lr lg lb
  rectangle (x-(w/2)) (y-(h/2)) w h
  stroke


-- draws an edge
renderEdge :: EdgeGI -> String -> NodeGI -> NodeGI -> Maybe GIColor -> Maybe GIColor -> Render ()
renderEdge edge content nodeSrc nodeDst shadowColor textColor = do
  if nodeSrc == nodeDst
    then renderLoop edge content nodeSrc shadowColor textColor
    else renderNormalEdge edge content nodeSrc nodeDst shadowColor textColor

renderNormalEdge :: EdgeGI -> String -> NodeGI -> NodeGI -> Maybe GIColor -> Maybe GIColor -> Render ()
renderNormalEdge edge content nodeSrc nodeDst shadowColor textColor = do
  -- calculate the intersection points of the edge with the source and target nodes
  let (x1, y1) = position nodeSrc
      (pw, ph) = dims nodeSrc
      (x2, y2) = position nodeDst
      (pw2, ph2) = dims nodeDst
      (ae,de) = cPosition edge
      ang = angle (x1,y1) (x2,y2)
      (xe, ye) = pointAt (ae+ang) de (midPoint (x1,y1) (x2,y2))
      (x1', y1') = case shape nodeSrc of
        NCircle ->
          let d1 = pointDistance (x1,y1) (xe,ye)
              (vx1,vy1) = ((xe-x1)/d1 , (ye-y1)/d1)
              n1 = (max pw ph + 1)/2
          in (x1 + vx1*n1, y1 + vy1*n1)
        NRect -> intersectLineRect (xe,ye) (x1,y1,pw+3,ph+3)
        NSquare -> let l = max pw ph in intersectLineRect (xe,ye) (x1,y1,l+3,l+3)
      (x2', y2') = case shape nodeDst of
        NCircle ->
          let d2 = pointDistance (xe,ye) (x2,y2)
              (vx2,vy2) = ((x2-xe)/d2 , (y2-ye)/d2)
              n2 = (max pw2 ph2 + 1)/2
          in (x2 - vx2*n2, y2 - vy2*n2)
        NRect-> intersectLineRect (xe,ye) (x2,y2,pw2+3,ph2+3)
        NSquare -> let l = max pw2 ph2 in intersectLineRect (xe,ye) (x2,y2,l+3,l+3)

  let (rl,gl,bl) = color edge
      centered = (xe,ye) == midPoint (x1,y1) (x2,y2)

  -- draw shadow
  case (shadowColor, centered) of
    (Nothing, _) -> return ()
    (Just (sr,sg, sb), True) -> do
      setLineWidth 6
      setSourceRGB sr sg sb
      moveTo x1' y1'
      lineTo x2' y2'
      stroke
    (Just (sr,sg,sb), False) -> do
      setLineWidth 6
      setSourceRGB sr sg sb
      let aglobal = angle (x1',y1') (x2', y2')
          d = pointDistance (x1',y1') (x2',y2')
          p1 = pointAt (pi+aglobal) (d/4) (xe,ye)
          p2 = pointAt (aglobal) (d/4) (xe,ye)
      moveTo x1' y1'
      curveTo x1' y1' (fst p1) (snd p1) xe ye
      curveTo (fst p2) (snd p2) x2' y2' x2' y2'
      stroke

  -- draw the edge
  setLineWidth 2
  setSourceRGB rl gl bl
  if centered
    then do
      case style edge of
        ENormal -> do
          moveTo x1' y1'
          lineTo x2' y2'
          stroke
        EPointed -> drawPointedLine (x1',y1') (x2',y2')
        ESlashed -> drawSlashedLine (x1',y1') (x2',y2')
    else do
      let aglobal = angle (x1',y1') (x2', y2')
          d = pointDistance (x1',y1') (x2',y2')
          p1 = pointAt (pi+aglobal) (d/4) (xe,ye)
          p2 = pointAt (aglobal) (d/4) (xe,ye)
      case style edge of
        ENormal -> do
          moveTo x1' y1'
          curveTo x1' y1' (fst p1) (snd p1) xe ye
          curveTo (fst p2) (snd p2) x2' y2' x2' y2'
          stroke
        EPointed -> do
          drawPointedCurve (x1',y1') (x1',y1') p1 (xe,ye)
          drawPointedCurve (xe,ye) p2 (x2',y2') (x2',y2')
        ESlashed -> do
          drawSlashedCurve (x1',y1') (x1',y1') p1 (xe,ye)
          drawSlashedCurve (xe,ye) p2 (x2',y2') (x2',y2')

  -- draws an arrow pointing to the target node
  let a = (angle (xe,ye) (x2',y2'))
      (xa1,ya1) = (x2',y2')
      (xa2,ya2) = pointAt (a+7*pi/8) 10 (x2',y2')
      (xa3,ya3) = pointAt (a-7*pi/8) 10 (x2',y2')
  moveTo xa1 ya1
  lineTo xa2 ya2
  lineTo xa3 ya3
  lineTo xa1 ya1
  fill
  -- draws a circle to show the edge's control point
  arc xe  ye 2 0 (2*pi)
  fill

  -- draw Label
  if null content
    then return ()
    else do

      let calculateLabelPos (px,py,pw,ph) =
            let a = angle (x1,y1) (x2,y2)
                (x0,y0) = multPoint (quadrant a) (pw/2,ph/2)
                minD = (abs $ tan(a)*x0 + y0) / sqrt(tan(a)*tan(a) + 1)
                (x,y) = pointAt (a - pi/2) (minD+8) (xe, ye)
            in (x - pw/2, y - ph/2)

      case textColor of
        Just color -> renderEdgeLabel content color "Sans Bold 10" calculateLabelPos
        Nothing -> renderEdgeLabel content (rl,gl,bl) "Sans Regular 10" calculateLabelPos








renderLoop:: EdgeGI -> String -> NodeGI -> Maybe GIColor -> Maybe GIColor -> Render ()
renderLoop edge content node shadowColor textColor = do
  let (a, d) = cPosition edge
      (x,y) = position node
      (xe, ye) = pointAt a d (x,y)
      p1 = pointAt (a+pi/8) (d/8) (x,y)
      p2 = pointAt (a+pi/2) (d/1.5) (xe,ye)
      p1' = pointAt (a-pi/8) (d/8) (x,y)
      p2' = pointAt (a-pi/2) (d/1.5) (xe,ye)
      (rl,gl,bl) = color edge

  case shadowColor of
    Nothing -> return ()
    Just (sr,sg,sb) -> do
      setSourceRGB sr sg sb
      setLineWidth 6
      moveTo x y
      curveTo (fst p1) (snd p1) (fst p2) (snd p2) xe ye
      moveTo x y
      curveTo (fst p1') (snd p1') (fst p2') (snd p2') xe ye
      stroke

  -- draws a bezier curve pointing to the node itself
  setSourceRGB rl gl bl
  setLineWidth 2
  case style edge of
    ENormal -> do
      moveTo x y
      curveTo (fst p1) (snd p1) (fst p2) (snd p2) xe ye
      moveTo x y
      curveTo (fst p1') (snd p1') (fst p2') (snd p2') xe ye
      stroke
    EPointed -> do
      drawPointedCurve (x,y) p1 p2 (xe,ye)
      drawPointedCurve (x,y) p1' p2' (xe,ye)
    ESlashed -> do
      drawSlashedCurve (x,y) p1 p2 (xe,ye)
      drawSlashedCurve (x,y) p1' p2' (xe,ye)
  -- draws a circle to show the edge's control point
  arc xe ye 2 0 (2*pi)
  fill

  -- draw Label
  if null content
    then  return ()
    else  do
      let calculateLabelPos (px,py,pw,ph) =
            let a = angle (x,y) (xe,ye) + pi/2
                (x0,y0) = multPoint (quadrant a) (pw/2,ph/2)
                minD = (abs $ tan(a)*x0 + y0) / sqrt(tan(a)*tan(a) + 1)
                pos = pointAt (a-pi/2) ( minD+8 ) (xe, ye)
            in ((fst pos - pw/2), (snd pos - ph/2))

      case textColor of
        Just color -> renderEdgeLabel content color "Sans Bold 10" calculateLabelPos
        Nothing -> renderEdgeLabel content (rl,gl,bl) "Sans Regular 10" calculateLabelPos


renderEdgeLabel :: String -> GIColor -> String -> (Quadruple Double -> (Double, Double)) -> Render ()
renderEdgeLabel content (r,g,b) descStr calcLabelPos =
  do
    context <- getContext
    setSourceRGB r g b
    pL <- createLabelLayout content descStr
    extents <- getLabelExtents pL
    let (lx, ly) = calcLabelPos extents
    moveTo lx ly
    Pango.showLayout context pL



renderNodeLabel :: String -> GIColor -> String -> Render ()
renderNodeLabel content (r,g,b) descStr =
  do
    setSourceRGB r g b
    pL <- createLabelLayout content descStr
    context <- getContext
    Pango.showLayout context pL

createLabelLayout :: String -> String -> Render Pango.Layout
createLabelLayout content descStr =
  do
    context <- getContext
    pL <- Pango.createLayout context
    Pango.layoutSetText pL (T.pack content) (-1)
    desc <- Pango.fontDescriptionFromString (T.pack descStr)
    Pango.layoutSetFontDescription pL (Just desc)
    return pL

getLabelExtents :: Pango.Layout -> Render (Quadruple Double)
getLabelExtents pL =
  do
    (rect,_) <- Pango.layoutGetExtents pL
    px <- Pango.getRectangleX rect
    py <- Pango.getRectangleY rect
    pw <- Pango.getRectangleWidth rect
    ph <- Pango.getRectangleHeight rect
    let [px', py', pw', ph'] =
          map ((\x-> x / (fromIntegral Pango.SCALE)) . fromIntegral) [px,py,pw,ph]
    return (px', py', pw', ph')




drawPointedLine :: (Double,Double) -> (Double,Double) -> Render ()
drawPointedLine p1@(x1,y1) p2@(x2,y2)= do
  let dist = pointDistance p1 p2
      pointsN = dist/4 :: Double
      linePoints = [0,(1/pointsN)..1]
  forM linePoints (\t -> drawPointInLine p1 p2 t)
  return ()

drawPointInLine :: (Double,Double) -> (Double,Double) -> Double -> Render ()
drawPointInLine p0@(x0,y0) p1@(x1,y1) t = do
  let (x,y) = interpolate p0 p1 t
  arc x y 1 0 (2*pi)
  fill

drawPointedCurve :: (Double,Double) -> (Double,Double) -> (Double,Double) -> (Double,Double) -> Render ()
drawPointedCurve p0@(x0,y0) p1@(x1,y1) p2@(x2,y2) p3@(x3,y3) = do
  let mid = midPoint p1 p2
      dist = pointDistance p0 mid + pointDistance p3 mid
      pointsN = dist/4 :: Double
      linePoints = [0,(1/pointsN)..1]
  forM linePoints (\t -> drawPointInCurve p0 p1 p2 p3 t)
  return ()

drawPointInCurve :: (Double,Double) -> (Double,Double) -> (Double,Double) -> (Double,Double) -> Double -> Render ()
drawPointInCurve p0@(x0,y0) p1@(x1,y1) p2@(x2,y2) p3@(x3,y3) t = do
  let b03 = (1-t)**3
      b13 = 3 * t * (1-t)**2
      b23 = 3 * t**2 * (1-t)
      b33 = t**3
      x = b03*x0 + b13*x1 + b23*x2 + b33*x3
      y = b03*y0 + b13*y1 + b23*y2 + b33*y3
  arc x y 1 0 (2*pi)
  fill

drawSlashedLine :: (Double,Double) -> (Double,Double) -> Render ()
drawSlashedLine p0@(x0,y0) p1@(x1,y1) = do
    let dist = pointDistance p0 p1
        pointsN = dist/4 :: Double
        linePoints = genPairs [0,(1/pointsN)..1]
    forM linePoints (\(t0,t1) -> drawSlashInLine p0 p1 t0 t1)
    return ()

drawSlashInLine :: (Double,Double) -> (Double,Double) -> Double -> Double ->  Render ()
drawSlashInLine p0@(x0,y0) p1@(x1,y1) t0 t1 = do
  let
    (x,y)   = interpolate p0 p1 t0
    (x',y') = interpolate p0 p1 t1
  moveTo x y
  lineTo x' y'
  stroke

drawSlashedCurve :: (Double,Double) -> (Double,Double) -> (Double,Double) -> (Double,Double) -> Render ()
drawSlashedCurve p0@(x0,y0) p1@(x1,y1) p2@(x2,y2) p3@(x3,y3) = do
    let mid = midPoint p1 p2
        dist = pointDistance p0 mid + pointDistance p3 mid
        pointsN = dist/4 :: Double
        curvePoints = genPairs [0,(1/pointsN)..1]

    forM curvePoints (\(t0,t1) -> drawSlashInCurve p0 p1 p2 p3 t0 t1)
    return ()

drawSlashInCurve :: (Double,Double) -> (Double,Double) -> (Double,Double) -> (Double,Double) -> Double -> Double -> Render ()
drawSlashInCurve p0@(x0,y0) p1@(x1,y1) p2@(x2,y2) p3@(x3,y3) t1 t2 = do
  let b03 t = (1-t)**3
      b13 t = 3 * t * (1-t)**2
      b23 t = 3 * t**2 * (1-t)
      b33 t = t**3
      x = (b03 t1)*x0 + (b13 t1)*x1 + (b23 t1)*x2 + (b33 t1)*x3
      y = (b03 t1)*y0 + (b13 t1)*y1 + (b23 t1)*y2 + (b33 t1)*y3
      x' = (b03 t2)*x0 + (b13 t2)*x1 + (b23 t2)*x2 + (b33 t2)*x3
      y' = (b03 t2)*y0 + (b13 t2)*y1 + (b23 t2)*y2 + (b33 t2)*y3
  moveTo x y
  lineTo x' y'
  stroke

-- auxiliar functions not directly related with rendering ----------------------
--------------------------------------------------------------------------------

-- auxiliar function used in the functions 'drawSlashedLine' and 'drawSlashedCurve'
genPairs :: [a] -> [(a,a)]
genPairs [] = []
genPairs (x:[]) = []
genPairs (x:y:ls) = (x,y):(genPairs ls)

-- calculates the intersection point of a line with a rectangle
-- receives the init point of the line and the rectangle representation
-- used in the function 'renderNormalEdge'
intersectLineRect :: (Double,Double) -> (Double,Double,Double,Double) -> (Double,Double)
intersectLineRect (lx,ly) (rx,ry,rw,rh) = (rx + t*(lx - rx), ry + t*(ly - ry))
  where t = min tx ty
        tx = (rw/2) / abs (lx - rx)
        ty = (rh/2) / abs (ly - ry)
