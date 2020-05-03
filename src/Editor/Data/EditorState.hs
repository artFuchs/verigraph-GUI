-- this module contains the definition of the Editor State, a structure
-- containing all the informations needed to draw a graph in the canvas
-- it also contain functions to modify the editor state

module Editor.Data.EditorState(
  EditorState
, emptyES
, editorGetGraph
, editorSetGraph
, editorGetGI
, editorSetGI
, editorGetSelected
, editorSetSelected
, editorGetZoom
, editorSetZoom
, editorGetPan
, editorSetPan
, selectNodeInPosition
, selectEdgeInPosition
, getEdgePosition
, createNode
, createEdge
, createEdges
, deleteSelected
, edgesFromTo
, moveNodes
, moveEdges
, changeNodeShape
, changeEdgeStyle
) where

import qualified Data.Map as M
import Data.Maybe
import Data.Graphs hiding (null, empty)
import qualified Data.Graphs as G
import Editor.Data.GraphicalInfo
import Editor.Helper.Geometry
import Data.List
import Editor.Data.Info hiding (empty)
import qualified Editor.Data.Info as I

-- | Graph Editor State
-- A tuple containing all the informations needed to draw the graph in the canvas
-- (graph, GraphicalInfo, elected nodes and edges, zoom, pan)
type EditorState = (Graph Info Info, GraphicalInfo, ([NodeId],[EdgeId]) , Double, (Double,Double))

-- constructor
emptyES :: EditorState
emptyES = (G.empty, (M.empty, M.empty), ([], []), 1.0, (0.0,0.0))

-- getters and setters
editorGetGraph :: EditorState -> Graph Info Info
editorGetGraph (g,_,_,_,_) = g

editorSetGraph :: Graph Info Info -> EditorState -> EditorState
editorSetGraph g (_,gi,s,z,p) = (g,gi,s,z,p)

editorGetGI :: EditorState -> GraphicalInfo
editorGetGI (_,gi,_,_,_) = gi

editorSetGI :: GraphicalInfo -> EditorState -> EditorState
editorSetGI gi (g,_,s,z,p) = (g,gi,s,z,p)

editorGetSelected :: EditorState -> ([NodeId], [EdgeId])
editorGetSelected (_,_,s,_,_) = s

editorSetSelected :: ([NodeId], [EdgeId]) -> EditorState -> EditorState
editorSetSelected s (g,gi,_,z,p) = (g,gi,s,z,p)

editorGetZoom :: EditorState -> Double
editorGetZoom (_,_,_,z,_) = z

editorSetZoom :: Double -> EditorState -> EditorState
editorSetZoom z (g,gi,s,_,p) = (g,gi,s,z,p)

editorGetPan :: EditorState -> (Double,Double)
editorGetPan (_,_,_,_,p) = p

editorSetPan :: (Double,Double) -> EditorState -> EditorState
editorSetPan p (g,gi,s,z,_) = (g,gi,s,z,p)


-- node/edge intersection functions
-- check if a given point is inside a node
selectNodeInPosition:: GraphicalInfo -> (Double,Double) -> Maybe NodeId
selectNodeInPosition (nodesG,_) (x,y) =
  case find (\n -> isSelected (snd n)) $ (M.toList nodesG) of
    Nothing -> Nothing
    Just (k,a) -> Just $ NodeId k
  where isSelected = (\n -> let (nx,ny) = position  n
                                (w,h) = dims n
                                l = max w h
                            in case shape n of
                              NCircle -> pointDistance (x,y) (nx,ny) < l/2
                              NRect -> pointInsideRectangle (x,y) (nx,ny,w,h)
                              NSquare -> pointInsideRectangle (x,y) (nx,ny,l,l) )


-- check if a given point is close of an edge control point
selectEdgeInPosition:: Graph Info Info -> GraphicalInfo -> (Double,Double) -> Maybe EdgeId
selectEdgeInPosition g gi (x,y) =
  case find (\e -> isSelected e) $ edges g of
    Nothing -> Nothing
    Just e -> Just $ edgeId e
  where
    isSelected = (\e -> pointDistance (x,y) (edgePos e) < 5)
    edgePos e = getEdgePosition g gi e

-- get edge position in cartesian coordinate system
getEdgePosition:: Graph Info Info -> GraphicalInfo -> Edge Info -> (Double,Double)
getEdgePosition g (nodesGi, edgesGi) e = pos
  where
    eid = edgeId e
    gi = getEdgeGI (fromEnum $ eid) edgesGi
    srcPos = position . getNodeGI (fromEnum $ sourceId e) $ nodesGi
    dstPos = position . getNodeGI (fromEnum $ targetId e) $ nodesGi
    (ae, de) = cPosition gi
    pos = if sourceId e /= targetId e
      then
        let pmid = midPoint srcPos dstPos
            (ang, dist) = toPolarFrom srcPos dstPos
        in pointAt (ae+ang) de pmid
      else
        pointAt ae de srcPos

-- create/delete operations ----------------------------------------------------
-- create a new node with it's default Info and GraphicalInfo
createNode :: EditorState -> GIPos -> GIDim -> Info -> NodeShape -> GIColor -> GIColor -> EditorState
createNode es pos dim info nshape color lcolor = editorSetGraph newGraph . editorSetGI newGI . editorSetSelected ([nid], []) $ es
  where
    graph = editorGetGraph es
    nid = head $ newNodes graph
    newGraph = insertNodeWithPayload nid info graph
    newNgi = NodeGI {position = pos, fillColor = color, lineColor = lcolor, dims = dim, shape = nshape}
    newGI = (M.insert (fromEnum nid) newNgi $ fst (editorGetGI es) , snd (editorGetGI es))


-- create a single edge between a src node and a target node
createEdge :: EditorState -> NodeId -> NodeId -> Info -> Bool -> EdgeStyle -> (Double,Double,Double) -> EditorState
createEdge es srcNode tgtNode info autoNaming estyle ecolor = editorSetGraph newGraph . editorSetGI (ngiM, newEgiM) . editorSetSelected ([],[eid]) $ es
  where 
    graph = editorGetGraph es
    (ngiM,egiM) = editorGetGI es
    eid = head $ newEdges graph
    info' = if infoLabel info == (Label "") && autoNaming then infoSetLabel info (show eid) else info
    newGraph = insertEdgeWithPayload eid srcNode tgtNode info' graph
    newPos = if (tgtNode == srcNode) then newLoopPos srcNode (graph,(ngiM,egiM)) else newEdgePos srcNode tgtNode (graph,(ngiM,egiM))
    negi = EdgeGI {cPosition = newPos, color = ecolor, style = estyle}
    newEgiM = M.insert (fromEnum eid) negi egiM

-- create edges between the selected nodes and a target node
createEdges:: EditorState -> NodeId -> Info -> Bool -> EdgeStyle -> (Double,Double,Double) -> EditorState
createEdges es tgtNode info autoNaming estyle ecolor = editorSetGraph newGraph . editorSetGI (ngiM, newegiM) . editorSetSelected ([],createdEdges) $ es
  where selectedNodes = fst $ editorGetSelected es
        graph = editorGetGraph es
        (ngiM,egiM) = editorGetGI es
        (newGraph, newegiM, createdEdges) = foldr create (graph, egiM, []) selectedNodes
        create = (\nid (g,giM,eids) -> let
                                    eid = head $ newEdges g
                                    info' = if infoLabel info == (Label "") && autoNaming then infoSetLabel info (show eid) else info
                                    ng = insertEdgeWithPayload eid nid tgtNode info' g
                                    newPos = if (tgtNode == nid) then newLoopPos nid (g,(ngiM,egiM)) else newEdgePos nid tgtNode (g,(ngiM,egiM))
                                    negi = EdgeGI {cPosition = newPos, color = ecolor, style = estyle}
                                  in (ng, M.insert (fromEnum eid) negi giM, eid:eids))

-- delete the selection
deleteSelected:: EditorState -> EditorState
deleteSelected es = editorSetSelected ([],[]) . editorSetGI (newngiM, newegiM) . editorSetGraph newGraph $ es
  where graph = editorGetGraph es
        (nids,eids) = editorGetSelected es
        foundNodes = map (\nid -> fromMaybe (Node (NodeId 0) I.empty) $ lookupNode nid graph) nids
        foundEdges = map (\eid -> fromMaybe (Edge (EdgeId 0) (NodeId 0) (NodeId 0) I.empty) $ lookupEdge eid graph) eids
        nids' = map nodeId $ filter (\n -> not $ infoLocked $ nodeInfo n) foundNodes
        eids' = map edgeId $ filter (\e -> not $ infoLocked $ edgeInfo e) foundEdges
        (ngiM, egiM) = editorGetGI es
        newngiM = foldl (\giM n -> M.delete n giM) ngiM (map fromEnum nids')
        newegiM = foldl (\giM n -> M.delete n giM) egiM (map fromEnum eids')
        graph' = foldl (\g e -> removeEdge e g) graph eids'
        newGraph = foldl (\g n -> removeNodeAndIncidentEdges n g) graph' nids'

-- auxiliar functions to createEdges
-- return a list of edges
edgesFromTo :: NodeInContext n e -> NodeInContext n e -> [Edge e]
edgesFromTo (n, context) (n', _) = foldl edgesTo [] econtexts
  where
    econtexts  = outgoingEdges context
    nid' = nodeId n'
    edgesTo = \l (_ , e, (tgt,_)) -> case nodeId tgt == nid' of
      True -> e:l
      False -> l


-- calculate a position for the new edge
newEdgePos :: NodeId -> NodeId -> (Graph a b, GraphicalInfo) -> (Double,Double)
newEdgePos nid nid' (g, giM)= (-pi/2,30*k)
  where
    mContextSrc = lookupNodeInContext nid g
    mContextTgt = lookupNodeInContext nid' g
    k = case (mContextSrc,mContextTgt) of
      (Just csrc, Just ctgt) -> let thisLength = genericLength (edgesFromTo csrc ctgt)
                                    otherLength = length (edgesFromTo ctgt csrc)
                                in thisLength + if otherLength > 0 then 1 else 0
      _ -> 0


-- calculate a position for the new loop
newLoopPos :: NodeId -> (Graph a b, GraphicalInfo) -> (Double,Double)
newLoopPos nid (g, giM)= (-pi/2,50+30*k)
 where
   k = case lookupNodeInContext nid g of
     Just c -> genericLength $ edgesFromTo c c
     _ -> 0

-- | Mode selected nodes
moveNodes:: EditorState -> (Double,Double) -> (Double,Double) -> EditorState
moveNodes es (xold,yold) (xnew,ynew) = editorSetGI (movedNGIs,egiM)  es
  where
      (sNodes, sEdges) = editorGetSelected es
      graph = editorGetGraph es
      (ngiM,egiM) = editorGetGI es
      (deltaX, deltaY) = (xnew-xold, ynew-yold)
      -- move the nodes
      moveN = \giMap (NodeId nid) -> let gi = getNodeGI nid giMap
                                         (ox, oy) = position gi
                                     in M.insert nid (nodeGiSetPosition (addPoint (position gi) (deltaX,deltaY)) gi) giMap
      movedNGIs = foldl moveN ngiM sNodes

-- | Move selected edges
moveEdges:: EditorState -> (Double,Double) -> (Double,Double) -> EditorState
moveEdges es (xold,yold) (xnew,ynew) = editorSetGI (ngi,newegi) es
  where graph = editorGetGraph es
        (sNodes,sEdges) = editorGetSelected es
        delta = (xnew-xold,ynew-yold)
        (ngi,egi) = editorGetGI es
        moveE = (\egiM eid -> case lookupEdge eid graph of
          Just edge -> if sourceId edge /= targetId edge
            then
              let srcPos = position . getNodeGI (fromEnum $ sourceId edge) $ ngi
                  dstPos = position . getNodeGI (fromEnum $ targetId edge) $ ngi
                  pmid = midPoint srcPos dstPos
                  (ang, dist) = toPolarFrom srcPos dstPos
                  gi = getEdgeGI (fromEnum $ eid) egi
                  (ae, de) = cPosition gi
                  edgePos = pointAt (ae+ang) de pmid
                  edgePos' = addPoint delta edgePos
                  a' = angle pmid edgePos' - ang
                  d' = pointDistance pmid edgePos'
              in M.insert (fromEnum eid) (edgeGiSetPosition (a',d') $ gi) egiM
            else
              let nodePos = position . getNodeGI (fromEnum $ sourceId edge) $ ngi
                  gi = getEdgeGI (fromEnum $ eid) egi
                  (ae, de) = cPosition gi
                  edgePos = addPoint delta $ pointAt ae de nodePos
                  (a,d) = toPolarFrom nodePos edgePos
              in M.insert (fromEnum eid) (edgeGiSetPosition (a,d) gi) egiM
          Nothing -> egiM)
        newegi = foldl moveE egi sEdges

-- | Change the selected nodes shape
changeNodeShape :: EditorState -> NodeShape -> EditorState
changeNodeShape es s = editorSetGI (newNgiM, egiM) es
  where
      nids = fst $ editorGetSelected es
      (ngiM, egiM) = editorGetGI es
      newNgiM = M.mapWithKey (\k gi -> if NodeId k `elem` nids then nodeGiSetShape s gi else gi) ngiM

-- | Change the selected edges style
changeEdgeStyle :: EditorState -> EdgeStyle -> EditorState
changeEdgeStyle es s = editorSetGI (ngiM, newEgiM) es
  where
    eids = snd $ editorGetSelected es
    (ngiM, egiM) = editorGetGI es
    newEgiM = M.mapWithKey (\k gi -> if EdgeId k `elem` eids then edgeGiSetStyle s gi else gi) egiM
