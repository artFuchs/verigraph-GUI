-- | module containing functions related with the rendering of a graph
module GUI.Render.GraphElements(
  renderNode
, renderEdge
, renderLabel
)where

import GI.Cairo
import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.Internal (Render(runRender))
import Graphics.Rendering.Cairo.Types (Cairo(Cairo))
import Control.Monad.Trans.Reader (runReaderT)
import Foreign.Ptr (castPtr)

import Graphics.Rendering.Pango.Cairo as GRPC
import Graphics.Rendering.Pango.Layout as GRPL
import Graphics.Rendering.Pango as GRP

import qualified Data.Text as T
import Data.List
import GUI.Data.GraphicalInfo
import GUI.Helper.Geometry
import Control.Monad


-- draw a node with it's label
renderNode :: NodeGI -> String -> Bool -> GIColor -> Bool -> GIColor -> Render ()
renderNode node content drawShadow shadowColor highlightText textColor = do
  let (x,y) = position node
      (r,g,b) = fillColor node
      (rl,gl,bl) = lineColor node
      (pw,ph) = dims node

  setLineWidth 2
  setSourceRGB r g b
  case shape node of
    NCircle -> let radius = (max pw ph)/2 in renderCircle (x,y) radius (r,g,b) (rl,gl,bl) drawShadow shadowColor
    NRect -> renderRectangle (x, y, pw, ph) (r,g,b) (rl,gl,bl) drawShadow shadowColor
    NSquare -> renderRectangle (x,y, (max pw ph), (max pw ph)) (r,g,b) (rl,gl,bl) drawShadow shadowColor


  moveTo (x-(pw/2-2)) (y-(ph/2-2))

  case highlightText of
    False -> do
      setSourceRGB rl gl bl
      pL <- GRPC.createLayout content
      desc <- liftIO $ GRP.fontDescriptionFromString "Sans Regular 10"
      liftIO $ GRPL.layoutSetFontDescription pL (Just desc)
      showLayout pL
    True -> do
      let (r,g,b) = textColor
      setSourceRGB r g b
      pL <- GRPC.createLayout content
      desc <- liftIO $ GRP.fontDescriptionFromString "Sans Bold 10"
      liftIO $ GRPL.layoutSetFontDescription pL (Just desc)
      showLayout pL

renderCircle :: (Double,Double) -> Double -> (Double,Double,Double) -> (Double,Double,Double) -> Bool -> (Double,Double,Double) -> Render ()
renderCircle (x,y) radius (r,g,b) (lr,lg,lb) drawShadow (sr,sg,sb) = do
  if drawShadow
    then do
      setSourceRGB sr sg sb
      arc x y (radius+3) 0 (2*pi)
      fill
    else
      return ()
  setSourceRGB r g b
  arc x y radius 0 (2*pi)
  fill
  setSourceRGB lr lg lb
  arc x y radius 0 (2*pi)
  stroke

renderRectangle :: (Double,Double,Double,Double) -> (Double,Double,Double) -> (Double,Double,Double) -> Bool -> (Double,Double,Double) ->  Render ()
renderRectangle (x,y,w,h) (r,g,b) (lr,lg,lb) drawShadow (sr,sg,sb) = do
  if drawShadow
    then do
      setSourceRGB sr sg sb
      rectangle (x-(w/2+3)) (y-(h/2+3)) (w+6) (h+6)
      fill
    else
      return ()
  setSourceRGB r g b
  rectangle (x-(w/2)) (y-(h/2)) w h
  fill
  setSourceRGB lr lg lb
  rectangle (x-(w/2)) (y-(h/2)) w h
  stroke


-- draws an edge
renderEdge :: EdgeGI -> String -> NodeGI -> NodeGI -> Bool -> GIColor -> Bool -> GIColor -> Render ()
renderEdge edge content nodeSrc nodeDst drawShadow (shadowColor) highlightText textColor = do
  if nodeSrc == nodeDst
    then renderLoop edge content nodeSrc drawShadow (shadowColor) highlightText textColor
    else renderNormalEdge edge content nodeSrc nodeDst drawShadow (shadowColor) highlightText textColor

renderNormalEdge :: EdgeGI -> String -> NodeGI -> NodeGI -> Bool -> GIColor -> Bool -> GIColor -> Render ()
renderNormalEdge edge content nodeSrc nodeDst drawShadow (sr,sg,sb) highlightText textColor = do
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

  let (r,g,b) = color edge
      centered = (xe,ye) == midPoint (x1,y1) (x2,y2)

  -- draw shadow
  case (drawShadow, centered) of
    (False, _) -> return ()
    (True, True) -> do
      setLineWidth 6
      setSourceRGB sr sg sb
      moveTo x1' y1'
      lineTo x2' y2'
      stroke
    (True, False) -> do
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
  setSourceRGB r g b
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
      pL <- GRPC.createLayout content
      desc <- case highlightText of
        False -> liftIO $ GRP.fontDescriptionFromString "Sans Regular 10"
        True -> liftIO $ GRP.fontDescriptionFromString "Sans Bold 10"
      (rl,gl,bl) <- case highlightText of
        False -> return (r,g,b)
        True -> return textColor
      liftIO $ GRPL.layoutSetFontDescription pL (Just desc)
      (_, PangoRectangle px py pw ph) <- liftIO $ layoutGetExtents pL
      let a = angle (x1,y1) (x2,y2)
          (x0,y0) = multPoint (quadrant a) (pw/2,ph/2)
          minD = (abs $ tan(a)*x0 + y0) / sqrt(tan(a)*tan(a) + 1)
          labelPos = pointAt (a - pi/2) (minD+8) (xe, ye)
      setSourceRGB rl gl bl
      moveTo (fst labelPos - pw/2) (snd labelPos - ph/2)
      showLayout pL

renderLoop:: EdgeGI -> String -> NodeGI -> Bool -> (Double, Double, Double) -> Bool -> GIColor -> Render ()
renderLoop edge content node drawShadow (sr,sg,sb) highlightText textColor = do
  let (a, d) = cPosition edge
      (x,y) = position node
      (xe, ye) = pointAt a d (x,y)
      p1 = pointAt (a+pi/8) (d/8) (x,y)
      p2 = pointAt (a+pi/2) (d/1.5) (xe,ye)
      p1' = pointAt (a-pi/8) (d/8) (x,y)
      p2' = pointAt (a-pi/2) (d/1.5) (xe,ye)
      (rl,gl,bl) = color edge

  if drawShadow
    then do
      setSourceRGB sr sg sb
      setLineWidth 6
      moveTo x y
      curveTo (fst p1) (snd p1) (fst p2) (snd p2) xe ye
      moveTo x y
      curveTo (fst p1') (snd p1') (fst p2') (snd p2') xe ye
      stroke
    else return ()

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
      pL <- GRPC.createLayout content
      desc <- case highlightText of
        False -> liftIO $ GRP.fontDescriptionFromString "Sans Regular 10"
        True -> liftIO $ GRP.fontDescriptionFromString "Sans Bold 10"
      (r,g,b) <- case highlightText of
        True -> return textColor
        False -> return (rl,gl,bl)
      liftIO $ GRPL.layoutSetFontDescription pL (Just desc)
      setSourceRGB r g b
      (_, PangoRectangle px py pw ph) <- liftIO $ layoutGetExtents pL
      let a = angle (x,y) (xe,ye) + pi/2
          (x0,y0) = multPoint (quadrant a) (pw/2,ph/2)
          minD = (abs $ tan(a)*x0 + y0) / sqrt(tan(a)*tan(a) + 1)
          labelPos = pointAt (a-pi/2) ( minD+8 ) (xe, ye)
      moveTo (fst labelPos - pw/2) (snd labelPos - ph/2)
      showLayout pL


renderLabel :: String -> (Double,Double) -> Render ()
renderLabel content (x,y) = do
  pL <- GRPC.createLayout content
  desc <- liftIO $ GRP.fontDescriptionFromString "Sans Regular 10"
  liftIO $ GRPL.layoutSetFontDescription pL (Just desc)
  setSourceRGB 0 0 0
  moveTo x y
  GRP.showLayout pL


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
