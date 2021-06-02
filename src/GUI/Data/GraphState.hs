-- this module contains the definition of a GraphState, a structure
-- containing all the informations needed to draw a graph in the canvas
-- it also contain functions to modify a GraphState

module GUI.Data.GraphState(
  GraphState (..)
, emptyState
, stateSetGraph
, stateSetGI
, stateSetSelected
, stateSetZoom
, stateSetPan
, selectNodeInPosition
, selectEdgeInPosition
, createNode
, createEdge
, createEdges
, deleteSelected
, newEdgePos
, newLoopPos
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

import GUI.Data.DiaGraph hiding (empty)
import GUI.Data.GraphicalInfo
import GUI.Helper.Geometry
import Data.List
import GUI.Data.Info hiding (empty)
import qualified GUI.Data.Info as I

-- | Graph Editor State
-- A tuple containing all the informations needed to draw the graph in the canvas
-- (graph, GraphicalInfo, elected nodes and edges, zoom, pan)
data GraphState = State { stateGetGraph :: Graph Info Info
                        , stateGetGI :: GraphicalInfo
                        , stateGetSelected :: ([NodeId],[EdgeId])
                        , stateGetZoom :: Double
                        , stateGetPan :: (Double,Double)}
                 deriving (Show,Read)

-- empty constructor
emptyState :: GraphState
emptyState = State G.empty (M.empty, M.empty) ([], []) 1.0 (0.0,0.0)

-- setters for graphState
stateSetGraph :: Graph Info Info -> GraphState -> GraphState
stateSetGraph g state = state {stateGetGraph = g}

stateSetGI :: GraphicalInfo -> GraphState -> GraphState
stateSetGI gi state = state {stateGetGI = gi}

stateSetSelected :: ([NodeId], [EdgeId]) -> GraphState -> GraphState
stateSetSelected s state = state {stateGetSelected = s}

stateSetZoom :: Double -> GraphState -> GraphState
stateSetZoom z state = state {stateGetZoom = z}

stateSetPan :: (Double,Double) -> GraphState -> GraphState
stateSetPan p state = state {stateGetPan = p}


-- node/edge intersection functions
-- | check if a given point is inside a node
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


-- | check if a given point is close of an edge control point
selectEdgeInPosition:: Graph Info Info -> GraphicalInfo -> (Double,Double) -> Maybe EdgeId
selectEdgeInPosition g gi (x,y) =
  case find (\e -> isSelected e) $ edges g of
    Nothing -> Nothing
    Just e -> Just $ edgeId e
  where
    isSelected = (\e -> pointDistance (x,y) (edgePos e) < 5)
    edgePos e = getEdgePosition (g,gi) e



-- create/delete operations ----------------------------------------------------
-- | create a new node with it's default Info and GraphicalInfo
createNode :: GraphState -> GIPos -> GIDim -> Info -> NodeShape -> GIColor -> GIColor -> GraphState
createNode es pos dim info nshape color lcolor = stateSetGraph newGraph . stateSetGI newGI . stateSetSelected ([nid], []) $ es
  where
    graph = stateGetGraph es
    nid = head $ newNodes graph
    newGraph = insertNodeWithPayload nid info graph
    newNgi = NodeGI {position = pos, fillColor = color, lineColor = lcolor, dims = dim, shape = nshape}
    newGI = (M.insert (fromEnum nid) newNgi $ fst (stateGetGI es) , snd (stateGetGI es))


-- | create a single edge between a source node and a target node
-- If desired, an id can be specified for the edge. In this case, the edge will have the specified id if there is no other edge with the same id
createEdge :: GraphState -> (Maybe EdgeId) -> NodeId -> NodeId -> Info -> Bool -> EdgeStyle -> (Double,Double,Double) -> GraphState
createEdge es mEid srcNode tgtNode info autoNaming estyle ecolor = stateSetGraph newGraph . stateSetGI (ngiM, newEgiM) . stateSetSelected ([],[eid]) $ es
  where
    graph = stateGetGraph es
    (ngiM,egiM) = stateGetGI es
    eid = let next = head $ newEdges graph
          in case mEid of
            Nothing -> next
            Just e -> if e `elem` edgeIds graph then next else e
    info' = if infoLabel info == (Label "") && autoNaming then infoSetLabel info (show $ fromEnum eid) else info
    newGraph = insertEdgeWithPayload eid srcNode tgtNode info' graph
    newPos = if (tgtNode == srcNode) then newLoopPos srcNode graph else newEdgePos srcNode tgtNode graph
    negi = EdgeGI {cPosition = newPos, color = ecolor, style = estyle}
    newEgiM = M.insert (fromEnum eid) negi egiM

-- | create edges between the selected nodes and a target node
createEdges:: GraphState -> NodeId -> Info -> Bool -> EdgeStyle -> (Double,Double,Double) -> GraphState
createEdges es tgtNode info autoNaming estyle ecolor = stateSetGraph newGraph . stateSetGI (ngiM, newegiM) . stateSetSelected ([],createdEdges) $ es
  where selectedNodes = fst $ stateGetSelected es
        graph = stateGetGraph es
        (ngiM,egiM) = stateGetGI es
        (newGraph, newegiM, createdEdges) = foldr create (graph, egiM, []) selectedNodes
        create = (\nid (g,giM,eids) -> let
                                    eid = head $ newEdges g
                                    info' = if infoLabel info == (Label "") && autoNaming then infoSetLabel info (show $ fromEnum eid) else info
                                    ng = insertEdgeWithPayload eid nid tgtNode info' g
                                    newPos = if (tgtNode == nid) then newLoopPos nid g else newEdgePos nid tgtNode g
                                    negi = EdgeGI {cPosition = newPos, color = ecolor, style = estyle}
                                  in (ng, M.insert (fromEnum eid) negi giM, eid:eids))

-- | delete the selection
deleteSelected:: GraphState -> GraphState
deleteSelected es = stateSetSelected ([],[]) . stateSetGI (newngiM, newegiM) . stateSetGraph newGraph $ es
  where graph = stateGetGraph es
        (nids,eids) = stateGetSelected es
        foundNodes = map (\nid -> fromMaybe (Node (NodeId 0) I.empty) $ lookupNode nid graph) nids
        foundEdges = map (\eid -> fromMaybe (Edge (EdgeId 0) (NodeId 0) (NodeId 0) I.empty) $ lookupEdge eid graph) eids
        nids' = map nodeId $ filter (\n -> not $ infoLocked $ nodeInfo n) foundNodes
        eids' = map edgeId $ filter (\e -> not $ infoLocked $ edgeInfo e) foundEdges
        (ngiM, egiM) = stateGetGI es
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
newEdgePos :: NodeId -> NodeId -> Graph a b -> (Double,Double)
newEdgePos nid nid' g = (-pi/2,30*k)
  where
    mContextSrc = lookupNodeInContext nid g
    mContextTgt = lookupNodeInContext nid' g
    k = case (mContextSrc,mContextTgt) of
      (Just csrc, Just ctgt) -> let thisLength = genericLength (edgesFromTo csrc ctgt)
                                    otherLength = genericLength (edgesFromTo ctgt csrc)
                                in thisLength + if otherLength > 0 then 1 else 0
      _ -> 0


-- calculate a position for the new loop
newLoopPos :: NodeId -> Graph a b -> (Double,Double)
newLoopPos nid g = (-pi/2,50+30*k)
 where
   k = case lookupNodeInContext nid g of
     Just c -> genericLength $ edgesFromTo c c
     _ -> 0

-- | Mode selected nodes
moveNodes:: GraphState -> (Double,Double) -> (Double,Double) -> GraphState
moveNodes es (xold,yold) (xnew,ynew) = stateSetGI (movedNGIs,egiM)  es
  where
      (sNodes, sEdges) = stateGetSelected es
      graph = stateGetGraph es
      (ngiM,egiM) = stateGetGI es
      (deltaX, deltaY) = (xnew-xold, ynew-yold)
      -- move the nodes
      moveN = \giMap (NodeId nid) -> let gi = getNodeGI nid giMap
                                         (ox, oy) = position gi
                                     in M.insert nid (gi {position = (addPoint (position gi) (deltaX,deltaY))}) giMap
      movedNGIs = foldl moveN ngiM sNodes

-- | Move selected edges
moveEdges:: GraphState -> (Double,Double) -> (Double,Double) -> GraphState
moveEdges es (xold,yold) (xnew,ynew) = stateSetGI (ngi,newegi) es
  where graph = stateGetGraph es
        (sNodes,sEdges) = stateGetSelected es
        delta = (xnew-xold,ynew-yold)
        (ngi,egi) = stateGetGI es
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
              in M.insert (fromEnum eid) ( gi {cPosition = (a',d')} ) egiM
            else
              let nodePos = position . getNodeGI (fromEnum $ sourceId edge) $ ngi
                  gi = getEdgeGI (fromEnum $ eid) egi
                  (ae, de) = cPosition gi
                  edgePos = addPoint delta $ pointAt ae de nodePos
                  (a,d) = toPolarFrom nodePos edgePos
              in M.insert (fromEnum eid) ( gi {cPosition = (a,d)} ) egiM
          Nothing -> egiM)
        newegi = foldl moveE egi sEdges

-- | Change the selected nodes shape
changeNodeShape :: GraphState -> NodeShape -> GraphState
changeNodeShape es s = stateSetGI (newNgiM, egiM) es
  where
      nids = fst $ stateGetSelected es
      (ngiM, egiM) = stateGetGI es
      newNgiM = M.mapWithKey (\k gi -> if NodeId k `elem` nids then gi {shape = s} else gi) ngiM

-- | Change the selected edges style
changeEdgeStyle :: GraphState -> EdgeStyle -> GraphState
changeEdgeStyle es s = stateSetGI (ngiM, newEgiM) es
  where
    eids = snd $ stateGetSelected es
    (ngiM, egiM) = stateGetGI es
    newEgiM = M.mapWithKey (\k gi -> if EdgeId k `elem` eids then gi {style = s} else gi) egiM
