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
import Editor.Helper.List

-- | generate a mask graph that informs if a node/edge has a unique name
-- True -> element has unique  name
-- False -> element has conflict
nameConflictGraph :: Graph Info Info -> Graph Bool Bool
nameConflictGraph g = fromNodesAndEdges vn ve
  where
    vn = map uniqueN ns
    ve = map uniqueE es
    ns = nodes g
    es = edges g
    nameIsValid name listOfNames = name /= "" && (name `notElem` listOfNames)
    uniqueN n = Node (nodeId n) $ nameIsValid (infoLabelStr $ nodeInfo n) (map (infoLabelStr . nodeInfo) $ filter (\n' -> nodeId n' /= nodeId n) ns)
    uniqueE e = Edge (edgeId e) (sourceId e) (targetId e) $ nameIsValid (infoLabelStr $ edgeInfo e) (map (infoLabelStr . edgeInfo) $ filter (\e' -> edgeId e' /= edgeId e) es)

-- auxiliar function: apply a function in a pair
applyPair :: (a->b) -> (a,a) -> (b,b)
applyPair f (a,b) = (f a, f b)

-- | generate a mask graph that says if a node/edge is valid according to a typeGraph
correctTypeGraph :: Graph Info Info -> Graph Info Info -> Graph Bool Bool
correctTypeGraph g tg = fromNodesAndEdges vn ve
  where
    vn = map nodeIsValid $ nodes g
    ve = map edgeIsValid $ edges g
    nodeToJust = \n -> Node (nodeId n) (Just $ nodeInfo n)
    edgeToJust = \e -> Edge (edgeId e) (sourceId e) (targetId e) (Just $ edgeInfo e)

    -- auxiliar structs to define the typedGraph
    epairs = map (applyPair edgeId) . filter (\(e,et) -> infoLabelStr (edgeInfo et) == infoType (edgeInfo e)) $ mkpairs (edges g) (edges tg)
    npairs = map (applyPair nodeId) . filter (\(n,nt) -> infoLabelStr (nodeInfo nt) == infoType (nodeInfo n)) $ mkpairs (nodes g) (nodes tg)
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

-- | check if a graph G is valid according to a typegraph TG
isGraphValid :: Graph Info Info -> Graph Info Info -> Bool
isGraphValid g tg = and $ concat [map nodeInfo $ nodes validG, map edgeInfo $ edges validG]
      where
        validG = correctTypeGraph g tg

-- | generate a mask graph that informs if the operation applied to a node/edge 
--   of a graph G (rule) are valid
opValidationGraph :: Graph Info Info -> Graph Bool Bool
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
                    (Create, Just Delete) -> False
                    (Delete, Just Create) -> False
                    _ -> True
          False -> case (srcop, eop, dstop) of
                    (Just _, Create, Just Delete) -> False
                    (Just _, Delete, Just Create) -> False
                    (Just Create, Delete, Just _) -> False
                    (Just Delete, Create, Just _) -> False
                    (Just Create, _, Just Delete) -> False
                    (Just Delete, _, Just Create) -> False
                    _ -> True
