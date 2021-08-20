{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

{-|
    Functions to help handling the graphical information of elements of the graph,
      especially concerning the label of nodes and edges.
-}
module GUI.Helper.GraphicalInfo(
  getStringDims
, updateNodesGiDims
, renameSelected
, setInfoExtra
, updateLoopPositions
)where

import Data.GI.Base
import qualified GI.Pango as P

import Control.Monad
import qualified Data.Text as T
import Data.IORef
import Data.Maybe
import qualified Data.Map as M

import Data.Graphs

import GUI.Data.GraphicalInfo
import GUI.Data.GraphState
import GUI.Data.Info

-- | given a text, compute the size of it's bounding box
-- uses the pango lib
getStringDims :: String -> P.Context -> Maybe T.Text -> IO (Double, Double)
getStringDims str context font = do
  desc <- case font of
    Just f -> P.fontDescriptionFromString f
    Nothing -> P.fontDescriptionFromString "Sans Regular 10"
  pL <- P.layoutNew context
  P.layoutSetFontDescription pL (Just desc)
  P.layoutSetText pL (T.pack str) (fromIntegral . length $ str)
  (_,rect) <- P.layoutGetExtents pL
  w <- get rect #width >>= (\n -> return . fromIntegral . quot n $ P.SCALE)
  h <- get rect #height >>= (\n -> return . fromIntegral . quot n $ P.SCALE)
  return (w +4, h + 4)

-- | given a nodeGI Map and a graph, update the sizes of the elements in the nodeGI Map
updateNodesGiDims :: M.Map Int NodeGI -> Graph Info Info -> P.Context -> IO (M.Map Int NodeGI)
updateNodesGiDims ngiM g context = do
  listOfNGIs <- forM (M.toList $ ngiM) $ \(n, gi) -> do
              let mNode = lookupNode (NodeId n) g
              case mNode of
                Nothing -> return (n,gi)
                Just node -> do
                  let info = nodeInfo node
                      label = infoVisible info
                  dim <- getStringDims label context Nothing
                  return (n, gi {dims = dim})
  return $ M.fromList listOfNGIs

-- rename the selected itens, modifying the sizes of nodes according to the text
renameSelected:: GraphState -> String -> P.Context -> IO GraphState
renameSelected es content context = do
  -- auxiliar function rename
  let newInfo = str2Info content
  let rename oldInfo = if infoLocked oldInfo
                       then oldInfo
                       else infoSetOperation (infoSetType newInfo t) op
                          where
                            op = case (infoOperation newInfo, content) of
                              (Preserve, ':':cs) -> Preserve
                              (Preserve, _) -> infoOperation oldInfo
                              (op,_) -> op
                            t = case infoType newInfo of
                                "" -> infoType oldInfo
                                it -> it
  let graph = stateGetGraph es
      (nids,eids) = stateGetSelected es
      -- apply rename in the graph elements to get newGraph
      graph' = foldl (\g nid -> updateNodePayload nid g rename) graph nids
      newGraph  = foldl (\g eid -> updateEdgePayload eid g rename) graph' eids
  -- change the GraphicalInfo of the renamed elements
  let (ngiM,egiM) = stateGetGI es
  dims <- forM (filter (\n -> nodeId n `elem` nids) (nodes newGraph))
               (\n -> do
                     let info = nodeInfo n
                         op = infoOperation info
                         fontdesc = if op == Preserve then Nothing else Just "Sans Bold 10"
                     dim <- getStringDims (infoVisible info) context fontdesc
                     return (nodeId n, dim)
               )
  let newNgiM = M.mapWithKey (\k gi -> case lookup (NodeId k) dims of
                                        Just dim -> gi {dims = dim}
                                        Nothing -> gi)
                             ngiM
      newEs   = stateSetGI (newNgiM,egiM) . stateSetGraph newGraph $ es
  return newEs


-- given a graph state and a mapping of elements to extra information, set the extra information to info
-- if the element id is not on the mapping, it has the extra information cleaned
setInfoExtra :: GraphState -> (M.Map NodeId String, M.Map EdgeId String) -> P.Context -> IO GraphState
setInfoExtra st (nm,em) context =
  do
    let g = stateGetGraph st
        layouts = stateGetGI st
        addExtraNode n = let info = nodeInfo n
                         in case M.lookup (nodeId n) nm of
                           Nothing -> n { nodeInfo = info {infoExtra = ""}}
                           Just extra -> n { nodeInfo = info { infoExtra = extra } }
        addExtraEdge e =  let info = edgeInfo e
                          in case M.lookup (edgeId e) em of
                            Nothing -> e { edgeInfo = info {infoExtra = ""}}
                            Just extra -> e { edgeInfo = info {infoExtra = extra }}
        nds = map addExtraNode (nodes g)
        edgs = map addExtraEdge (edges g)
        g' = fromNodesAndEdges nds edgs
        (nls,els) = stateGetGI st
    nls' <- updateNodesGiDims nls g' context
    let layouts = (nls', els)
    return (stateSetGraph g' . stateSetGI layouts $ st)


-- given
updateLoopPositions :: GraphicalInfo -> Graph Info Info -> IO GraphicalInfo
updateLoopPositions gi g =
  do
    let loops = filter (\e -> sourceId e == targetId e ) (edges g)
        loopsByNodes = foldr (\e m -> M.insertWith (++) (sourceId e) [e] m ) (M.empty) loops
    loopsgis <- forM (M.toList loopsByNodes) $ \(nid,es) ->
      do
        let ngi = getNodeGI (fromEnum nid) (fst gi)
            h = case (shape ngi,dims ngi) of
                  (NCircle,(w,h)) -> max w h
                  (NSquare,(w,h)) -> max w h
                  (NRect,(w,h)) -> h
            es' = zip (map (fromEnum . edgeId) es) [1..]
            egis = map (\(eid,k) ->
                          let egi = getEdgeGI eid (snd gi)
                              egi' = egi {cPosition = (-pi/2,h/2+30*k)}
                          in (eid,egi')) es'
        return egis
    let loopsgis' = concat loopsgis
        edgesgis = foldr (\(eid,egi) m -> M.insert eid egi m) (snd gi) loopsgis'
    return (fst gi, edgesgis)
