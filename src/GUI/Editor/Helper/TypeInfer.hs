module GUI.Editor.Helper.TypeInfer (
  listPossibleEdgeTypes
, infereEdgeType
, infereEdgesTypesAfterNodeChange
)where

import qualified Data.Map as M
import Data.Maybe

import Data.Graphs

import GUI.Data.GraphState
import GUI.Data.Info
import GUI.Data.GraphicalInfo

-- get a list of the possible types an edge can have based on its source and target nodes
listPossibleEdgeTypes :: Graph Info Info -> Node Info -> Node Info -> [String]
listPossibleEdgeTypes tg src tgt = possibleTypes
  where
    srcT = infoType $ nodeInfo src
    tgtT = infoType $ nodeInfo tgt
    getId t = case filter (\n -> infoLabelStr (nodeInfo n) == t) (nodes tg) of 
                [] -> Nothing
                [n] -> Just (nodeId n)
    srcId = getId srcT
    tgtId = getId tgtT
    possibleTypes = case (srcId,tgtId) of
      (Nothing,_) -> []
      (_,Nothing) -> []
      (Just s, Just t) -> 
        let possibleEdges = filter (\e -> sourceId e == s && targetId e == t) (edges tg)
        in  map (infoLabelStr . edgeInfo) possibleEdges

-- | Infere a type for a edge based on its ending nodes. 
--   The function gives preference to a specified type.
infereEdgeType :: Graph Info Info -> Node Info -> Node Info -> Maybe String -> Maybe String
infereEdgeType tg src tgt preferedType = inferedType
  where 
    possibleTypes = listPossibleEdgeTypes tg src tgt
    inferedType = case (possibleTypes, preferedType) of
      ([],_) -> Nothing
      (t:ts,Nothing) -> Just t
      (t:ts,Just pt) -> if pt `elem` (t:ts) then preferedType else Just t

-- | Infere edges types based on the selected nodes in the editorState
infereEdgesTypesAfterNodeChange :: GraphState -> Graph Info Info -> M.Map String (M.Map (String,String) EdgeGI) -> GraphState
infereEdgesTypesAfterNodeChange es tg typesE = stateSetGraph newGraph . stateSetGI newGIM  $ es
  where 
    g = stateGetGraph es
    (sNIds,sEIds) = stateGetSelected es
    giM = stateGetGI es
    nodesInContext = map (\nid -> fromJust $ lookupNodeInContext nid g) sNIds
    incidentEdgesInContext = concat $ map (incidentEdges . snd) nodesInContext
    edgesWithEndings = foldr (\((src,srcC),e,(tgt,tgtC)) l -> 
                                  if edgeId e `elem` map (\(_,e,_) -> edgeId e) l
                                    then l
                                    else (src,e,tgt):l) 
                                [] incidentEdgesInContext                    
    edgesIdsAndTypes = map
                      (\(src,e,tgt) -> 
                        let t = infoLabelStr $ edgeInfo e
                            t' = case infereEdgeType tg src tgt (Just t) of
                                  Nothing -> t 
                                  Just it -> it
                            srcT = infoType $ nodeInfo src
                            tgtT = infoType $ nodeInfo tgt
                        in (edgeId e, t', (srcT, tgtT))
                      )
                      edgesWithEndings
    newGraph = foldr (\(eid, t, _) g -> updateEdgePayload eid g (\info -> infoSetType info t)) g edgesIdsAndTypes

    changeEGI (eid,t,ndsT) giM = 
      case M.lookup t typesE of
        Nothing -> giM
        Just sm -> case M.lookup ndsT sm of
          Nothing -> giM
          Just typeEGI -> M.insert (fromEnum eid) (typeEGI {cPosition = cPosition egi}) giM
            where egi = getEdgeGI (fromEnum eid) giM

    newEGI = foldr changeEGI (snd giM) edgesIdsAndTypes
    newGIM = ((fst $ stateGetGI es),newEGI)