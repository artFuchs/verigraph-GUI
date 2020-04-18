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

-- | Generate a mask graph that informs if each node has a unique name
--    and if each edge has a unique combination of name and endings
-- True -> element has unique  name
-- False -> element has conflict
nameConflictGraph :: Graph Info Info -> Graph Bool Bool
nameConflictGraph g = fromNodesAndEdges vn ve
  where
    vn = map uniqueN ns
    ve = map uniqueE es
    ns = nodes g
    es = edges g

    nodesNames = map (\n -> (nodeId n, infoLabelStr $ nodeInfo n)) ns
    nameIsValid name listOfNames = 
      name /= "" && (name `notElem` listOfNames)
    uniqueN n = n { nodeInfo = nameIsValid (infoLabelStr $ nodeInfo n) (map snd $ filter (\(k,_) -> nodeId n /= k) nodesNames) }

    edgesNamesN'Endings = map (\e -> (edgeId e, (infoLabelStr $ edgeInfo e, sourceId e, targetId e))) es
    edgeNameIsValid (name,src,tgt) listOfNames = 
      name /= "" && (name,src,tgt) `notElem` listOfNames
    uniqueE e = e { edgeInfo = edgeNameIsValid ((infoLabelStr $ edgeInfo e),sourceId e, targetId e) (map snd $ filter (\(k,_) -> edgeId e /= k) edgesNamesN'Endings) }

-- auxiliar function: apply a function in a pair
applyPair :: (a->b) -> (a,a) -> (b,b)
applyPair f (a,b) = (f a, f b)

-- | generate a mask graph that says if a node/edge is valid according to a typeGraph
correctTypeGraph :: Graph Info Info -> Graph Info Info -> (Graph Bool Bool)
correctTypeGraph g tg = fromNodesAndEdges vn ve
  where
    vn = map nodeIsValid $ nodes g
    ve = map edgeIsValid $ edges g

    -- define the morphism between the graph and the type graph
    npairs = mkpairs (nodes g) (nodes tg)
    npairs' = map (applyPair nodeId) . filter (\(n,nt) -> infoLabelStr (nodeInfo nt) == infoType (nodeInfo n)) $ npairs 

    epairs = mkpairs (edges g) (edges tg)
    epairs' = filter (\(e,et) ->  let srcType = infoType . nodeInfo . fromJust $ lookupNode (sourceId e) g
                                      tgtType = infoType . nodeInfo . fromJust $ lookupNode (targetId e) g
                                      srctLbl = infoLabelStr . nodeInfo . fromJust $ lookupNode (sourceId et) tg
                                      tgttLbl = infoLabelStr . nodeInfo . fromJust $ lookupNode (targetId et) tg
                                  in infoLabelStr (edgeInfo et) == infoType (edgeInfo e) &&
                                      srcType == srctLbl && tgtType == tgttLbl)
              epairs
    epairs'' = map (applyPair edgeId) epairs'
             
    morph = Morph.fromGraphsAndLists g tg npairs' epairs''

    -- functions to modify the informations of nodes and edges of g to generate the mask
    nodeIsValid n = Node (nodeId n) $ case Morph.applyNodeId morph (nodeId n) of
                      Just _ -> True
                      Nothing -> False
    edgeIsValid e = e {edgeInfo = nodesTypesAreValid}
      where
        tge = Morph.applyEdge morph e
        tgtgt = Morph.applyNodeId morph (targetId e)
        tgsrc = Morph.applyNodeId morph (sourceId e)
        nodesTypesAreValid = case (tge, tgsrc, tgtgt) of
                              (Just e', Just srcid, Just tgtid) -> srcid == (sourceId e') && (tgtid == targetId e')
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
