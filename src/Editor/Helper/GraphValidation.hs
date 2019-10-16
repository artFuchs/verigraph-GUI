module Editor.Helper.GraphValidation (
  nameConflictGraph
, correctTypeGraph
, isGraphValid
, opValidationGraph
)where

import Data.Maybe
import Data.Graphs
import qualified Data.Graphs.Morphism as Morph
import qualified Data.TypedGraph as TG
import Editor.Data.Info
import Editor.Helper.Helper

-- validation ------------------------------------------------------------------
-- generate a mask graph that informs if a node/edge has a unique name
-- True -> element has unique  name
-- False -> element has conflict
nameConflictGraph :: Graph String String -> Graph Bool Bool
nameConflictGraph g = fromNodesAndEdges vn ve
  where
    vn = map uniqueN ns
    ve = map uniqueE es
    ns = nodes g
    es = edges g
    uniqueN n = Node (nodeId n) $ infoLabel (nodeInfo n) /= "" && (notElem (infoLabel . nodeInfo $ n) $ map (infoLabel . nodeInfo) . filter (\n' -> nodeId n' /= nodeId n) $ ns)
    uniqueE e = Edge (edgeId e) (sourceId e) (targetId e) $ infoLabel (edgeInfo e) /= "" && (notElem (infoLabel . edgeInfo $ e) $ map edgeInfo . filter (\e' -> edgeId e' /= edgeId e) $ es)

-- Auxiliar function: apply a function in a pair
applyPair :: (a->b) -> (a,a) -> (b,b)
applyPair f (a,b) = (f a, f b)

-- generate a mask graph that says if a node/edge is valid according to a typeGraph or not
correctTypeGraph :: Graph String String -> Graph String String -> Graph Bool Bool
correctTypeGraph g tg = fromNodesAndEdges vn ve
  where
    vn = map nodeIsValid $ nodes g
    ve = map edgeIsValid $ edges g
    nodeToJust = \n -> Node (nodeId n) (Just $ nodeInfo n)
    edgeToJust = \e -> Edge (edgeId e) (sourceId e) (targetId e) (Just $ edgeInfo e)

    -- auxiliar structs to define the typedGraph
    epairs = map (applyPair edgeId) . filter (\(e,et) -> infoLabel (edgeInfo et) == infoType (edgeInfo e)) $ mkpairs (edges g) (edges tg)
    npairs = map (applyPair nodeId) . filter (\(n,nt) -> infoLabel (nodeInfo nt) == infoType (nodeInfo n)) $ mkpairs (nodes g) (nodes tg)
    g' = fromNodesAndEdges (map nodeToJust (nodes g)) (map edgeToJust (edges g))
    tg' = fromNodesAndEdges (map nodeToJust (nodes tg)) (map edgeToJust (edges tg))

    -- typedGraph
    typedG = TG.fromGraphMorphism $ Morph.fromGraphsAndLists g' tg' npairs epairs

    -- functions to create the nodes and edges of the correctTypeGraph
    nodeIsValid n = Node (nodeId n) $ case Morph.applyNodeId typedG (nodeId n) of
                      Just _ -> True
                      Nothing -> False

    edgeIsValid e = Edge (edgeId e) (sourceId e) (targetId e) nodesTypesAreValid
      where
        tge = Morph.applyEdgeId typedG (edgeId e)
        tgtgt = Morph.applyNodeId typedG (targetId e)
        tgsrc = Morph.applyNodeId typedG (sourceId e)
        nodesTypesAreValid = case (tge, tgsrc, tgtgt) of
                              (Just eid, Just srcid, Just tgtid) -> srcid == srce' && tgtid == tgte'
                                where
                                  e' = fromJust . lookupEdge eid $ tg
                                  tgte' = nodeId . fromJust . lookupNode (targetId e') $ tg
                                  srce' = nodeId . fromJust . lookupNode (sourceId e') $ tg
                              _ -> False


isGraphValid :: Graph String String -> Graph String String -> Bool
isGraphValid g tg = and $ concat [map nodeInfo $ nodes validG, map edgeInfo $ edges validG]
      where
        validG = correctTypeGraph g tg


opValidationGraph :: Graph String String -> Graph Bool Bool
opValidationGraph g = fromNodesAndEdges nodes' edges'
  where
    nodes' = map (\n -> Node (nodeId n) True) $ nodes g
    edges' = map edgeValidation $ edges g
    edgeValidation e = Edge (edgeId e) (sourceId e) (targetId e) edgeIsValid
      where
        eop = infoOperation . edgeInfo $ e
        (srcop, dstop) = applyPair
                          (Just (infoOperation . nodeInfo) <*>)
                          (lookupNode (sourceId e) g, lookupNode (targetId e) g)
        edgeIsValid = case sourceId e == targetId e of
          True -> case (eop, srcop) of
                    ("new", Just "del") -> False
                    ("del", Just "new") -> False
                    _ -> True
          False -> case (srcop, eop, dstop) of
                    (Just _, "new", Just "del") -> False
                    (Just _, "del", Just "new") -> False
                    (Just "new", "del", Just _) -> False
                    (Just "del", "new", Just _) -> False
                    (Just "new", _, Just "del") -> False
                    (Just "del", _, Just "new") -> False
                    _ -> True
