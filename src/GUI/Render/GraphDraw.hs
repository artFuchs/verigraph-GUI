module GUI.Render.GraphDraw (
  drawGraph
, drawTypeGraph
, drawHostGraph
, drawRuleGraph
, drawRuleSideGraph
, drawNACGraph
, drawHostGraphWithMatches
) where

import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import Control.Monad
import Graphics.Rendering.Cairo

import Data.Graphs hiding (null)

import GUI.Data.Info
import GUI.Data.GraphState
import GUI.Data.GraphicalInfo
import GUI.Data.Nac
import GUI.Render.Render
import GUI.Helper.GraphValidation
import GUI.Helper.Geometry

import qualified Graphics.Rendering.Pango as GRP
import qualified Graphics.Rendering.Pango.Cairo as GRPC
import qualified Graphics.Rendering.Pango.Layout as GRPL

-- TODO: reorganize functions drawHostGraphWithMatches and drawRuleSideGraph to use the drawGraph base function

type Color = (Double,Double,Double)
type SquareSelection = Maybe (Double,Double,Double,Double)

-- highlight colors for basic situations
selectColor = (0.29,0.56,0.85)
errorColor = (0.9,0.2,0.2)
bothColor = mixColors selectColor errorColor

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

drawGraph :: GraphState -> SquareSelection -> M.Map NodeId Color -> M.Map EdgeId Color -> M.Map NodeId Color -> M.Map EdgeId Color -> Render ()
drawGraph state sq nodeColors edgeColors nodeTextColors edgeTextColors = do
  let g = stateGetGraph state
      (nGI,eGI) = stateGetGI state
      (sNodes,sEdges) = stateGetSelected state
      z = stateGetZoom state
      (px,py) = stateGetPan state

  scale z z
  translate px py

  -- draw the edges
  forM (edges g) $ \e -> do
    let dstN = M.lookup (fromEnum . targetId $ e) nGI
        srcN = M.lookup (fromEnum . sourceId $ e) nGI
        egi  = M.lookup (fromEnum . edgeId   $ e) eGI
        info = infoVisible $ edgeInfo e
        (highlight, color) = case (edgeId e `elem` sEdges, M.lookup (edgeId e) edgeColors) of
            (False,Nothing) -> (False, (0,0,0))
            (False,Just c)  -> (True, c)
            (True, Nothing) -> (True, selectColor)
            (True, Just c)  -> (True, mixColors selectColor c)
        (highlightText, textColor) = Maybe.fromMaybe (False,(0,0,0)) $ (\a -> (True, a)) <$> M.lookup (edgeId e) edgeTextColors
    case (egi, srcN, dstN) of
      (Just gi, Just src, Just dst) -> renderEdge gi info src dst highlight color highlightText textColor
      _ -> return ()

  -- draw the nodes
  forM (nodes g) $ \n -> do
    let ngi = M.lookup (fromEnum . nodeId $ n) nGI
        info = infoVisible $ nodeInfo n
        (highlight, color) = case (nodeId n `elem` sNodes, M.lookup (nodeId n) nodeColors) of
            (False,Nothing) -> (False, (0,0,0))
            (False, Just c) -> (True, c)
            (True, Nothing) -> (True, selectColor)
            (True, Just c)  -> (True, mixColors selectColor c)
        (highlightText, textColor) = Maybe.fromMaybe (False,(0,0,0)) $ (\a -> (True, a)) <$> M.lookup (nodeId n) nodeTextColors
    case (ngi) of
      Just gi -> renderNode gi info highlight color highlightText textColor
      Nothing -> return ()

  drawSelectionBox sq

-- draw a typegraph in the canvas
-- if there are nodes or edges with same names, then highlight them as errors
-- if any element is selected, highlight it as selected
drawTypeGraph :: GraphState -> SquareSelection -> Render ()
drawTypeGraph state sq = drawGraph state sq nodeColors edgeColors M.empty M.empty
  where
    g = stateGetGraph state
    cg = nameConflictGraph g
    edgeColors = M.fromList $ map (\e -> (edgeId e, errorColor)) $ filter (not . edgeInfo) (edges cg)
    nodeColors = M.fromList $ map (\n -> (nodeId n, errorColor)) $ filter (not . nodeInfo) (nodes cg)

-- draw a typed graph
-- if there are nodes or edges not correctly typed, highlight them as errors
-- if any element is selected, highlight it as selected
drawHostGraph :: GraphState -> SquareSelection -> Graph Info Info -> Render ()
drawHostGraph state sq tg = drawGraph state sq nodeColors edgeColors M.empty M.empty
  where
    g = stateGetGraph state
    vg = correctTypeGraph g tg
    edgeColors = M.fromList $ map (\e -> (edgeId e, errorColor)) $ filter (not . edgeInfo) (edges vg)
    nodeColors = M.fromList $ map (\n -> (nodeId n, errorColor)) $ filter (not . nodeInfo) (nodes vg)

-- draw a rulegraph
-- similar to drawHostGraph, but draws bold texts to indicate operations
drawRuleGraph :: GraphState -> SquareSelection -> Graph Info Info -> Render ()
drawRuleGraph state sq tg = drawGraph state sq nodeColors edgeColors nodeTextColors edgeTextColors
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


-- draw a single side of a rule
--
drawRuleSideGraph :: GraphState -> SquareSelection -> Graph Info Info -> Render ()
drawRuleSideGraph state sq k = do
  let g = stateGetGraph state
      (nGI,eGI) = stateGetGI state
      (sNodes,sEdges) = stateGetSelected state
      z = stateGetZoom state
      (px,py) = stateGetPan state

  scale z z
  translate px py

  let (idr, idg, idb) = (0.5,0.5,0.5) --(0.57, 0.47, 0)

  -- draw the edges
  forM (edges g) (\e -> do
    let dstN = M.lookup (fromEnum . targetId $ e) nGI
        srcN = M.lookup (fromEnum . sourceId $ e) nGI
        egi  = M.lookup (fromEnum . edgeId   $ e) eGI
        label = infoLabelStr (edgeInfo e)
        selected = (edgeId e) `elem` sEdges
        shouldDrawId = case lookupEdge (edgeId e) k of
                        Just e' -> True
                        Nothing -> False
    case (egi, srcN, dstN) of
      (Just gi, Just src, Just dst) -> do
          renderEdge gi label src dst selected selectColor False (0,0,0)
          -- draw the IDs of the edges to identify the graph morphism
          if shouldDrawId
            then drawEdgeId gi (edgeId e) src dst
            else return ()
      _ -> return ())

  -- draw the nodes
  forM (nodes g) (\n -> do
    let ngi = M.lookup (fromEnum . nodeId $ n) nGI
        label = infoLabelStr (nodeInfo n)
        selected = (nodeId n) `elem` (sNodes)
        shouldDrawId = case lookupNode (nodeId n) k of
                          Just _ -> True
                          Nothing -> False
    case (ngi) of
      Just gi -> do
          renderNode gi label selected selectColor False (0,0,0)
          -- draw the IDs of the nodes to identify the graph morphism
          if shouldDrawId
            then drawNodeId gi (nodeId n)
            else return ()
      Nothing -> return ())

  drawSelectionBox sq
  return ()

-- draw a nac
-- it highlights elements of the lhs part of the rule with yellow shadows
-- and highlights merged elements of the lhs part of the rule with green shadows
drawNACGraph :: GraphState -> SquareSelection -> Graph Info Info -> MergeMapping -> Render ()
drawNACGraph state sq tg (nM,eM) = drawGraph state sq nodeColors edgeColors M.empty M.empty
  where
    g = stateGetGraph state
    vg = correctTypeGraph g tg
    (nGI,eGI) = stateGetGI state
    nodeColors =  (M.fromList $ map (\n -> (nodeId n, errorColor)) $ filter (not . nodeInfo) (nodes vg))
                  `M.union`
                  (M.fromList $ map (\n -> (nodeId n, if isNodeMerged n then mergedColor else lockedColor)) $ filter (infoLocked . nodeInfo) (nodes g))
    edgeColors =  (M.fromList $ map (\e -> (edgeId e, errorColor)) $ filter (not . edgeInfo) (edges vg))
                  `M.union`
                  (M.fromList $ map (\e -> (edgeId e, if isEdgeMerged e then mergedColor else lockedColor)) $ filter (infoLocked . edgeInfo) (edges g))
    lockedColor = (0.90,0.75,0.05)
    mergedColor = (0.28,0.70,0.09)
    isNodeMerged n = M.size (M.filter (== (nodeId n)) nM) > 1
    isEdgeMerged e = M.size (M.filter (== (edgeId e)) eM) > 1

-- draw a hostGraph highlighting some match with yellow
drawHostGraphWithMatches :: GraphState -> SquareSelection -> Graph Info Info -> (M.Map NodeId [NodeId],M.Map EdgeId [EdgeId]) -> Render ()
drawHostGraphWithMatches state sq tg (nodeMap, edgeMap) = do
  let g = stateGetGraph state
      (nGI,eGI) = stateGetGI state
      (sNodes,sEdges) = stateGetSelected state
      z = stateGetZoom state
      (px,py) = stateGetPan state

  scale z z
  translate px py

  let matchedColor = (0.90,0.75,0.05)
      bothColor' = (0.28,0.70,0.09)
  let (idr, idg, idb) = (0.5,0.5,0.5) --(0.57, 0.47, 0)

  -- draw the edges
  forM (edges g) $ \e -> do
    let dstN = M.lookup (fromEnum . targetId $ e) nGI
        srcN = M.lookup (fromEnum . sourceId $ e) nGI
        egi  = M.lookup (fromEnum . edgeId   $ e) eGI
        selected = (edgeId e) `elem` sEdges
        matchId = head <$> M.lookup (edgeId e) edgeMap
        matched = not (null matchId)
        color = case (selected,matched) of
                 (False,False) -> (0,0,0)
                 (False,True) -> matchedColor
                 (True,False) -> selectColor
                 (True,True) -> bothColor'
    case (egi, srcN, dstN) of
      (Just gi, Just src, Just dst) -> do
          renderEdge gi (infoLabelStr (edgeInfo e)) src dst (selected || matched) color False (0,0,0)
          case matchId of
            Just id ->  drawEdgeId gi id src dst
            _ -> return ()
      _ -> return ()

  -- draw the nodes
  forM (nodes g) $ \n -> do
    let ngi = M.lookup (fromEnum . nodeId $ n) nGI
        info = nodeInfo n
        label = infoLabelStr info
        selected = (nodeId n) `elem` (sNodes)
        matchId = head <$> M.lookup (nodeId n) nodeMap
        matched = not (null matchId)
        color = case (selected,matched) of
                  (False,False) -> (0,0,0)
                  (False,True) -> matchedColor
                  (True,False) -> selectColor
                  (True,True) -> bothColor'
    case (ngi) of
      Just gi -> do
        renderNode gi label (selected || matched) color False (0,0,0)
        case matchId of
          Just id ->  drawNodeId gi id
          _ -> return ()
      Nothing -> return ()


  -- draw the selectionBox
  drawSelectionBox sq
  return ()




drawEdgeId :: EdgeGI -> EdgeId -> NodeGI -> NodeGI -> Render ()
drawEdgeId gi eid src dst = do
  let
    (idr,idb,idg) = (0.5,0.5,0.5)
    (ae,de) = cPosition gi
    (x1, y1) = position src
    (x2, y2) = position dst
    ang = angle (x1,y1) (x2,y2)
    pos = pointAt (ae+ang) de (midPoint (x1,y1) (x2,y2))
  pL <- GRPC.createLayout (show (fromEnum eid))
  desc <- liftIO $ GRP.fontDescriptionFromString "Sans Bold 10"
  liftIO $ GRPL.layoutSetFontDescription pL (Just desc)
  setSourceRGB idr idg idb
  moveTo (fst pos) (snd pos)
  GRPC.showLayout pL

drawNodeId :: NodeGI -> NodeId -> Render ()
drawNodeId gi nid = do
  let
    (idr,idb,idg) = (0.5,0.5,0.5)
    pos = case shape gi of
              NCircle -> let diam = maximum [fst . dims $ gi, snd . dims $ gi]
                         in addPoint (position gi) ((-diam/2), 0)
              NRect   -> addPoint (position gi) (-(fst . dims $ gi),0)
              NSquare -> let a = maximum [fst . dims $ gi, snd . dims $ gi]
                         in addPoint (position gi) ((-a/2),0)
  pL <- GRPC.createLayout (show (fromEnum nid))
  desc <- liftIO $ GRP.fontDescriptionFromString "Sans Bold 10"
  liftIO $ GRPL.layoutSetFontDescription pL (Just desc)
  setSourceRGB idr idg idb
  moveTo (fst pos) (snd pos)
  GRPC.showLayout pL
