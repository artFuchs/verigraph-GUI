module Editor.GrammarMaker (
  graphToRuleGraphs
, makeTypeGraph
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

type RuleGraphs = (Graph String String, Graph String String, Graph String String)
type TypeGraph a b = Graph (Maybe a) (Maybe b) -- may delete this if import span

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
    km = makeTypedGraph k typeGraph

    lhsTgm = TGM.fromGraphsAndLists km lm leftNodeMapping leftEdgeMapping
    rhsTgm = TGM.fromGraphsAndLists km rm rightNodeMapping rightEdgeMapping

    leftNodeMapping = filter (\(n1,n2) -> n1 == n2) $ mkpairs (map nodeId $ nodes k) (map nodeId $ nodes lhs)
    leftEdgeMapping = filter (\(n1,n2) -> n1 == n2) $ mkpairs (map edgeId $ edges k) (map edgeId $ edges lhs)

    rightNodeMapping = filter (\(n1,n2) -> n1 == n2) $ mkpairs (map nodeId $ nodes k) (map nodeId $ nodes rhs)
    rightEdgeMapping = filter (\(n1,n2) -> n1 == n2) $ mkpairs (map edgeId $ edges k) (map edgeId $ edges rhs)


makeTypeGraph :: Graph a b -> TypeGraph a b
makeTypeGraph g = fromNodesAndEdges nds edgs
  where
    nds = map (\n -> Node (nodeId n) $ Just (nodeInfo n)) $ nodes g
    edgs = map (\e -> Edge (edgeId e) (sourceId e) (targetId e) (Just $ edgeInfo e)) $ edges g


makeGrammar :: Graph String String -> Graph String String -> [Graph String String] -> [String] -> IO (Maybe (Grammar (TGM.TypedGraphMorphism String String)))
makeGrammar tg hg rgs rulesNames = do

  let typegraph = makeTypeGraph tg
      initGraph = makeTypedGraph hg typegraph
      productions = map (\r -> graphToRule r typegraph) rgs

  ensureValid $ validateNamed (\name -> "Rule '"++name++"'") (zip rulesNames productions)
  if (L.null productions)
    then return Nothing
    else return $ Just $ grammar initGraph [] (zip rulesNames productions)
