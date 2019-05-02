module Editor.DiaGraph
( DiaGraph
, isDiaGraphEqual
, diagrDisjointUnion
, empty
)where

import qualified Data.Map as M
import Data.Graphs hiding (null, empty)
import qualified Data.Graphs as G
import Editor.GraphicalInfo

-- |DiaGraph
-- A pair containing a graph and it's graphical information
type DiaGraph = (Graph String String ,GraphicalInfo)

isDiaGraphEqual :: DiaGraph -> DiaGraph -> Bool
isDiaGraphEqual (g1,gi1) (g2,gi2) = g1 == g2 && nodesGiEq && edgesGiEq
  where
    nodesGiEq = sameLength (M.elems $ fst gi1) (M.elems $ fst gi2) && all (\(x,y) -> x == y) (zip (M.elems $ fst gi1) (M.elems $ fst gi2))
    edgesGiEq = sameLength (M.elems $ snd gi1) (M.elems $ snd gi2) && all (\(x,y) -> x == y) (zip (M.elems $ snd gi1) (M.elems $ snd gi2))
    sameLength l1 l2 = length l1 == length l2


-- converts and association list to the respective function
assocToFunc :: (Eq a) => [(a,a)] -> a -> a
assocToFunc l = foldr (\(x,y) m -> (\z -> if z == x then y else (m z)) ) id l

-- disjoint union between two DiaGraphs
diagrDisjointUnion :: DiaGraph -> DiaGraph -> DiaGraph
diagrDisjointUnion diagr1@(g1,(ngiM1,egiM1)) (g2,(ngiM2,egiM2)) = diagrUnion diagr1 (g2',(ngiM2',egiM2'))
  where
    -- associar novos ids aos elementos de g2
    newNids = zipWith (\n nid-> (nodeId n, nid)) (nodes g2) $ take (length $ nodes g2) (newNodes g1)
    newEids = zipWith (\e eid-> (edgeId e, eid)) (edges g2) $ take (length $ edges g2) (newEdges g1)
    fn = assocToFunc newNids
    fe = assocToFunc newEids
    ns2' = foldr (\n ns -> (Node (fn $ nodeId n) (nodeInfo n)):ns ) [] $ nodes g2
    es2' = foldr (\e es -> (Edge (fe $ edgeId e) (fn $ sourceId e) (fn $ targetId e) (edgeInfo e)):es ) [] $ edges g2
    g2' = fromNodesAndEdges ns2' es2'
    ngiM2' = M.foldrWithKey (\k a m -> M.insert (fromEnum . fn . toEnum $ k) a m) M.empty ngiM2
    egiM2' = M.foldrWithKey (\k a m -> M.insert (fromEnum . fe . toEnum $ k) a m) M.empty egiM2




diagrUnion :: DiaGraph -> DiaGraph -> DiaGraph
diagrUnion (g1,(ngiM1,egiM1)) (g2,(ngiM2,egiM2)) = (g3,(ngiM3,egiM3))
  where
    ns3 = concat [nodes g1, nodes g2]
    es3 = concat [edges g1, edges g2]
    g3 = fromNodesAndEdges ns3 es3
    ngiM3 = M.union ngiM1 ngiM2
    egiM3 = M.union egiM1 egiM2




empty :: DiaGraph
empty = (G.empty, (M.empty, M.empty))
