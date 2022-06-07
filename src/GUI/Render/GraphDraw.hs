-- | This module contains functions to draw diferent kindos of graphs
module GUI.Render.GraphDraw (
  drawGraph
, drawTypeGraph
, drawHostGraph
, drawRuleGraph
, drawNACGraph
, drawGraphHighlighting
) where

import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import Control.Monad
import GI.Cairo.Render

import Data.Graphs hiding (null)

import GUI.Data.Info
import GUI.Data.GraphState
import GUI.Data.GraphicalInfo
import GUI.Data.Nac
import GUI.Render.GraphElements
import GUI.Helper.GraphValidation
import GUI.Helper.Geometry


type Color = (Double,Double,Double)
type SquareSelection = Maybe (Double,Double,Double,Double)

-- highlight colors for basic situations
selectColor = (0.29,0.56,0.85)
errorColor = (0.9,0.2,0.2)
bothColor = mixColors selectColor errorColor
lockedColor = (0.90,0.75,0.05)

mixColors :: Color -> Color -> Color
mixColors (r1,g1,b1) (r2,g2,b2) = ( (r1+r2)/2, (g1+g2)/2, (b1+b2)/2 )

drawSelectionBox:: SquareSelection -> Render()
drawSelectionBox sq = case sq of
  Just (x,y,w,h) -> do
    rectangle x y w h
    setSourceRGBA 0.29 0.56 0.85 0.5
    fill
    rectangle x y w h
    setSourceRGBA 0.29 0.56 0.85 1
    stroke
  Nothing -> return ()

drawGraph :: GraphState -> SquareSelection -> M.Map NodeId Color -> M.Map EdgeId Color -> M.Map NodeId Color -> M.Map EdgeId Color -> Maybe (Double,Double) -> Render ()
drawGraph state sq nodeColors edgeColors nodeTextColors edgeTextColors mRect = do
  let g = stateGetGraph state
      (nGI,eGI) = stateGetGI state
      (sNodes,sEdges) = stateGetSelected state
      z = stateGetZoom state
      (px,py) = stateGetPan state

  scale z z
  translate px py

  -- draw the edges
  forM (edges g) $ \e -> do
    let tgtN = M.lookup (fromEnum . targetId $ e) nGI
        srcN = M.lookup (fromEnum . sourceId $ e) nGI
        egi  = M.lookup (fromEnum . edgeId   $ e) eGI
        info = infoVisible $ edgeInfo e
        shadowColor = case (edgeId e `elem` sEdges, M.lookup (edgeId e) edgeColors) of
            (False,Nothing) -> Nothing
            (False,Just c)  -> Just c
            (True, _) -> Just selectColor
        textColor = M.lookup (edgeId e) edgeTextColors
    case (egi, srcN, tgtN, mRect) of
      (Just gi, Just src, Just tgt, Nothing) -> renderEdge gi info src tgt shadowColor textColor
      (Just gi, Just src, Just tgt, Just (sw,sh)) ->
        if isEdgeOnScreen (px,py) z (sw,sh) gi src tgt
          then renderEdge gi info src tgt shadowColor textColor
          else return ()
      _ -> return ()

  -- draw the nodes
  forM (nodes g) $ \n -> do
    let ngi = M.lookup (fromEnum . nodeId $ n) nGI
        info = infoVisible $ nodeInfo n
        shadowColor = case (nodeId n `elem` sNodes, M.lookup (nodeId n) nodeColors) of
            (False,Nothing) -> Nothing
            (False, Just c) -> Just c
            (True, Nothing) -> Just selectColor
            (True, Just c)  -> Just (mixColors selectColor c)
        textColor = M.lookup (nodeId n) nodeTextColors
    case (ngi, mRect) of
      (Just gi, Nothing) -> renderNode gi info shadowColor textColor
      (Just gi, Just (sw,sh)) ->
        if isNodeOnScreen (px,py) z (sw,sh) gi
          then renderNode gi info shadowColor textColor
          else return ()
      (Nothing,_) -> return ()


  drawSelectionBox sq

isEdgeOnScreen :: (Double,Double) -> Double -> (Double,Double) -> EdgeGI -> NodeGI ->  NodeGI ->  Bool
isEdgeOnScreen (px,py) z (sw, sh) egi srcgi tgtgi =
  isRectOnScreen (px,py) z (sw, sh) rect
  where
    rect =
      if position srcgi == position tgtgi
        then pointsToRectangle (position srcgi) (getEGIPosition egi srcgi tgtgi)
        else
          let (x1,y1,w1,h1) = pointsToRectangle (position srcgi) (getEGIPosition egi srcgi tgtgi)
              (x2,y2,w2,h2) = pointsToRectangle (position tgtgi) (getEGIPosition egi srcgi tgtgi)
              left = min (x1 - w1/2) (x2 - w2/2)
              right = max (x1 + w1/2) (x2 + w2/2)
              top = min (y1 - h1/2) (y2 - h2/2)
              botton = max (y1 + h1/2) (y2 + h2/2)
              we = right - left
              he = botton - top
              (xe,ye) = midPoint (left,top) (right,botton)
          in (xe,ye,we,he)


isNodeOnScreen :: (Double,Double) -> Double -> (Double,Double) -> NodeGI -> Bool
isNodeOnScreen (px,py) z (sw, sh) ngi =
  isRectOnScreen (px,py) z (sw, sh) rect
  where
    (x,y) = position ngi
    (w,h) = dims ngi
    rect = case (shape ngi) of
      NCircle -> let d = max w h in (x,y,d,d)
      NSquare -> let d = max w h in (x,y,d,d)
      _ -> (x,y,w,h)


isRectOnScreen :: (Double,Double) -> Double -> (Double,Double) -> (Double,Double,Double,Double) -> Bool
isRectOnScreen (px,py) z (sw, sh) rect@(rx,ry,rw,rh) =
  rectangleOverlapsRectangle screenRect rect'
  where
    screenRect = (sw/2,sh/2,sw,sh)
    rect' = ((rx+px)*z,(ry+py)*z,rw*z,rh*z)

-- draw a typegraph in the canvas
-- if there are nodes or edges with same names, then highlight them as errors
-- if any element is selected, highlight it as selected
drawTypeGraph :: GraphState -> SquareSelection -> Maybe (Double,Double) -> Render ()
drawTypeGraph state sq mRect = drawGraph state sq nodeColors edgeColors M.empty M.empty mRect
  where
    g = stateGetGraph state
    cg = nameConflictGraph g
    edgeColors = M.fromList $ map (\e -> (edgeId e, errorColor)) $ filter (not . edgeInfo) (edges cg)
    nodeColors = M.fromList $ map (\n -> (nodeId n, errorColor)) $ filter (not . nodeInfo) (nodes cg)

-- draw a typed graph
-- if there are nodes or edges not correctly typed, highlight them as errors
-- if any element is selected, highlight it as selected
drawHostGraph :: GraphState -> SquareSelection -> Graph Info Info -> Maybe (Double,Double) -> Render ()
drawHostGraph state sq tg mRect = drawGraph state sq nodeColors edgeColors M.empty M.empty mRect
  where
    g = stateGetGraph state
    vg = correctTypeGraph g tg
    edgeColors = M.fromList $ map (\e -> (edgeId e, errorColor)) $ filter (not . edgeInfo) (edges vg)
    nodeColors = M.fromList $ map (\n -> (nodeId n, errorColor)) $ filter (not . nodeInfo) (nodes vg)

-- draw a rulegraph
-- similar to drawHostGraph, but draws bold texts to indicate operations
drawRuleGraph :: GraphState -> SquareSelection -> Graph Info Info -> Maybe (Double,Double) -> Render ()
drawRuleGraph state sq tg mRect = drawGraph state sq nodeColors edgeColors nodeTextColors edgeTextColors mRect
  where
    g = stateGetGraph state
    (nGI,eGI) = stateGetGI state
    vg = correctTypeGraph g tg
    ovg = opValidationGraph g
    edgeColors = (M.fromList $ map (\e -> (edgeId e, errorColor)) $ filter (not . edgeInfo) (edges vg))
                 `M.union`
                 (M.fromList $ map (\e -> (edgeId e, errorColor)) $ filter (not . edgeInfo) (edges ovg))
                 `M.union`
                 edgeTextColors
    nodeColors = (M.fromList $ map (\n -> (nodeId n, errorColor)) $ filter (not . nodeInfo) (nodes vg))
                 `M.union`
                 (M.fromList $ map (\n -> (nodeId n, errorColor)) $ filter (not . nodeInfo) (nodes ovg))
                 `M.union`
                 nodeTextColors
    edgeTextColors = M.fromList $ Maybe.catMaybes
                                $ map (\e -> case infoOperation (edgeInfo e) of
                                                Create -> Just (edgeId e, createColor)
                                                Delete -> Just (edgeId e, deleteColor)
                                                _ -> Nothing)
                                  (edges g)
    nodeTextColors = M.fromList $ Maybe.catMaybes
                                $ map (\n -> case infoOperation (nodeInfo n) of
                                                Create -> Just (nodeId n, createColor)
                                                Delete -> Just (nodeId n, deleteColor)
                                                _ -> Nothing)
                                  (nodes g)
    createColor = (0.12, 0.48, 0.10)
    deleteColor = (0.17, 0.28, 0.77)

-- draw a nac
-- it highlights elements of the lhs part of the rule with yellow shadows
-- and highlights merged elements of the lhs part of the rule with green shadows
drawNACGraph :: GraphState -> SquareSelection -> Graph Info Info -> MergeMapping -> Maybe (Double,Double) -> Render ()
drawNACGraph state sq tg (nM,eM) mRect = drawGraph state sq nodeColors edgeColors M.empty M.empty mRect
  where
    g = stateGetGraph state
    vg = correctTypeGraph g tg
    nodeColors =  (M.fromList $ map (\n -> (nodeId n, errorColor)) $ filter (not . nodeInfo) (nodes vg))
                  `M.union`
                  (M.fromList $ map (\n -> (nodeId n, if isNodeMerged n then mergedColor else lockedColor)) $ filter (infoLocked . nodeInfo) (nodes g))
    edgeColors =  (M.fromList $ map (\e -> (edgeId e, errorColor)) $ filter (not . edgeInfo) (edges vg))
                  `M.union`
                  (M.fromList $ map (\e -> (edgeId e, if isEdgeMerged e then mergedColor else lockedColor)) $ filter (infoLocked . edgeInfo) (edges g))
    mergedColor = (0.28,0.70,0.09)
    isNodeMerged n = M.size (M.filter (== (nodeId n)) nM) > 1
    isEdgeMerged e = M.size (M.filter (== (edgeId e)) eM) > 1

-- draw a graph highlighting elements of the lhs part of the rule with yellow shadows
drawGraphHighlighting :: GraphState -> SquareSelection -> ([NodeId],[EdgeId]) -> Maybe (Double,Double) -> Render ()
drawGraphHighlighting state sq (nList,eList) mRect = drawGraph state sq nodeColors edgeColors M.empty M.empty mRect
  where
    g = stateGetGraph state
    nodeColors = M.fromList $ map (\n -> (n, lockedColor)) $ filter (\n -> n `elem` nList) (nodeIds g)
    edgeColors = M.fromList $ map (\e -> (e, lockedColor)) $ filter (\e -> e `elem` eList) (edgeIds g)
