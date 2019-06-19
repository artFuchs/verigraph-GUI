module Editor.GrammarMaker (
  graphToRuleGraphs
, makeGrammar
)where

import Data.Maybe
import Data.Graphs
import qualified Data.List as L
import Editor.Info
import Base.Valid
import Abstract.Rewriting.DPO
import Rewriting.DPO.TypedGraph
import Data.Graphs.Morphism
import qualified Data.TypedGraph as TG
import qualified Data.TypedGraph.Morphism as TGM
import XML.GGXReader.Span
import Editor.Helper

type RuleGraphs = (Graph String String, Graph String String, Graph String String)

graphToRuleGraphs :: Graph String String -> RuleGraphs
graphToRuleGraphs g = (lhs, k, rhs)
  where
    nods = nodes g
    edgs = edges g

    kNodes = filter (\n -> infoOperation (nodeInfo n) == "") nods
    kEdges = filter (\e -> infoOperation (edgeInfo e) == "") edgs
    k = fromNodesAndEdges kNodes kEdges

    lhsNodes = filter (\n -> infoOperation (nodeInfo n) == "del") nods ++ kNodes
    lhsEdges = filter (\e -> infoOperation (edgeInfo e) == "del") edgs ++ kEdges
    lhs = fromNodesAndEdges lhsNodes lhsEdges

    rhsNodes = filter (\n -> infoOperation (nodeInfo n) == "new") nods ++ kNodes
    rhsEdges = filter (\e -> infoOperation (edgeInfo e) == "new") edgs ++ kEdges
    rhs = fromNodesAndEdges rhsNodes rhsEdges

mkpairs :: [a] -> [b] -> [(a,b)]
mkpairs xs ys = do x <- xs
                   y <- ys
                   return (x,y)

makeTypedGraph :: Graph String String -> Graph (Maybe String) (Maybe String) -> TG.TypedGraph String String
makeTypedGraph g tg = fromGraphsAndLists g' tg npairs epairs
  where
    -- auxiliar functions to define the typedGraph
    nodeToJust = \n -> Node (nodeId n) (Just $ nodeInfo n)
    edgeToJust = \e -> Edge (edgeId e) (sourceId e) (targetId e) (Just $ edgeInfo e)

    -- auxiliar structs to define the typedGraph
    epairs = map (\(e, et) -> (edgeId e, edgeId et)) .
            filter (\(e,et) -> infoLabel (fromJust $ edgeInfo et) == infoType (edgeInfo e)) $ mkpairs (edges g) (edges tg)
    npairs = map (\(n, nt) -> (nodeId n, nodeId nt)) .
            filter (\(n,nt) -> infoLabel (fromJust $ nodeInfo nt) == infoType (nodeInfo n)) $ mkpairs (nodes g) (nodes tg)
    g' = fromNodesAndEdges (map nodeToJust (nodes g)) (map edgeToJust (edges g))



graphToRule :: Graph String String -> TypeGraph String String -> TypedGraphRule String String
graphToRule ruleGraph typeGraph = Production lhsTgm rhsTgm []
  where
    (lhs, k, rhs) = graphToRuleGraphs ruleGraph
    lm = makeTypedGraph lhs typeGraph
    rm = makeTypedGraph rhs typeGraph
    (lhsTgm,rhsTgm) = instantiateSpan lm rm maps

    nmapping = map (\p -> let (l,r) = applyPair (show . nodeId) p in (r, Nothing, l)) . filter (\(n,nt) -> infoLabel (nodeInfo nt) == infoType (nodeInfo n)) $ mkpairs (nodes lhs) (nodes rhs)
    emapping = map (\p -> let (l,r) = applyPair (show . edgeId) p in (r, Nothing, l)) . filter (\(e,et) -> infoLabel (edgeInfo et) == infoType (edgeInfo e)) $ mkpairs (edges lhs) (edges rhs)
    maps = nmapping ++ emapping


graphToTypeGraph :: Graph a b -> TypeGraph a b
graphToTypeGraph g = fromNodesAndEdges nds edgs
  where
    nds = map (\n -> Node (nodeId n) $ Just (nodeInfo n)) $ nodes g
    edgs = map (\e -> Edge (edgeId e) (sourceId e) (targetId e) (Just $ edgeInfo e)) $ edges g


makeGrammar :: Graph String String -> Graph String String -> [Graph String String] -> IO (Grammar (TGM.TypedGraphMorphism String String))
makeGrammar tg hg rgs = do

  let typegraph = graphToTypeGraph tg
      rulesNames = map show [1..]
      initGraph = makeTypedGraph hg typegraph
      productions = map (\r -> graphToRule r typegraph) rgs

  ensureValid $ validateNamed (\name -> "Rule '"++name++"'") (zip rulesNames productions)
  _ <- (L.null productions && error "No first-order productions were found, at least one is needed.") `seq` return ()

  return $ grammar initGraph [] (zip rulesNames productions)
