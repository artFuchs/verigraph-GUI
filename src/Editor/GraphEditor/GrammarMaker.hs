module Editor.GraphEditor.GrammarMaker (
  makeGrammar
, graphToRuleGraphs
, makeTypeGraph
, makeTypedGraph
, formatNac
, getNacPushout
, nodeToJust
, edgeToJust
, nodeFromJust
, edgeFromJust
)where

import Data.Maybe
import Data.Graphs
import Category.TypedGraph
import Category.TypedGraph.Limit
import qualified Data.List as L
import qualified Data.Map as M
import Editor.Data.Info
import Editor.Helper.Helper
import Base.Valid
import Abstract.Rewriting.DPO
import Rewriting.DPO.TypedGraph
import Data.Graphs.Morphism
import Abstract.Category.Limit
import qualified Data.TypedGraph as TG
import qualified Data.TypedGraph.Morphism as TGM


type RuleGraphs = (Graph String String, Graph String String, Graph String String)
type TypeGraph a b = Graph (Maybe a) (Maybe b) -- may delete this if import span
type NAC = (Graph Info Info, (M.Map NodeId NodeId, M.Map EdgeId EdgeId))

nodeToJust :: Node a -> Node (Maybe a)
nodeToJust = \n -> Node (nodeId n) (Just $ nodeInfo n)

edgeToJust :: Edge a -> Edge (Maybe a)
edgeToJust = \e -> Edge (edgeId e) (sourceId e) (targetId e) (Just $ edgeInfo e)

nodeFromJust :: Node (Maybe a) -> Node a
nodeFromJust = \n -> Node (nodeId n) (fromJust $ nodeInfo n)

edgeFromJust :: Edge (Maybe a) -> Edge a
edgeFromJust = \e -> Edge (edgeId e) (sourceId e) (targetId e) (fromJust $ edgeInfo e)


graphToRuleGraphs :: Graph String String
                  -> RuleGraphs
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


formatNac :: TypedGraph String String
          -> TypedGraph String String
          -> (M.Map NodeId NodeId, M.Map EdgeId EdgeId)
          -> IO (TypedGraphMorphism String String)
formatNac nac lhs (nmap, emap) = do
  putStrLn "---------\nK"
  print k
  putStrLn "---------\nnacTgm"
  print nacTgm
  putStrLn "---------\nlhsTgm"
  print lhsTgm
  return nacTgm'
  where
    -- get the mapped elements from lhs
    knodeIds = M.keys nmap
    kedgeIds = M.keys emap
    knodes = map nodeFromJust $ filter (\n -> nodeId n `elem` knodeIds) $ nodes $ domainGraph lhs
    kedges = map edgeFromJust $ filter (\e -> edgeId e `elem` kedgeIds) $ edges $ domainGraph lhs
    -- generate a interface typed graph k
    k = makeTypedGraph (fromNodesAndEdges knodes kedges) (TG.typeGraph lhs)
    -- generate two morphisms
    lhsTgm = TGM.fromGraphsAndLists k lhs (map (\n -> (n,n)) knodeIds) (map (\n -> (n,n)) kedgeIds)
    nacTgm = TGM.fromGraphsAndLists k nac (M.toList nmap) (M.toList emap)
    -- calculate the pushout between them
    (nacTgm',_) = calculatePushout nacTgm lhsTgm

-- getNacPushout nac lhs (nmap, emap)
-- given two TypedGraphs - nac and lhs
-- and a tuple of maps of ids - (nmap, emap),
-- calculate the pushout of nac <- k -> lhs, where
--  k is the interface between nac and lhs
--  the pushout is given by two typedGraphMorphisms (tgmNac',tgmLhs')
getNacPushout :: TypedGraph String String
              -> TypedGraph String String
              -> (M.Map NodeId NodeId, M.Map EdgeId EdgeId)
              -> (TypedGraphMorphism String String, TypedGraphMorphism String String)
getNacPushout nac lhs (nmap, emap) = calculatePushout nacTgm lhsTgm
  where
    -- get the mapped elements from lhs
    knodeIds = M.keys nmap
    kedgeIds = M.keys emap
    knodes = map nodeFromJust $ filter (\n -> nodeId n `elem` knodeIds) $ nodes $ domainGraph lhs
    kedges = map edgeFromJust $ filter (\e -> edgeId e `elem` kedgeIds) $ edges $ domainGraph lhs
    -- generate a interface typed graph k
    k = makeTypedGraph (fromNodesAndEdges knodes kedges) (TG.typeGraph lhs)
    -- generate two morphisms
    lhsTgm = TGM.fromGraphsAndLists k lhs (map (\n -> (n,n)) knodeIds) (map (\n -> (n,n)) kedgeIds)
    nacTgm = TGM.fromGraphsAndLists k nac (M.toList nmap) (M.toList emap)


makeTypedGraph :: Graph String String
               -> Graph (Maybe String) (Maybe String)
               -> TG.TypedGraph String String
makeTypedGraph g tg = fromGraphsAndLists g' tg npairs epairs
  where
    -- auxiliar structs to define the typedGraph
    epairs = map (\(e, et) -> (edgeId e, edgeId et)) .
            filter (\(e,et) -> infoLabel (fromJust $ edgeInfo et) == infoType (edgeInfo e)) $ mkpairs (edges g) (edges tg)
    npairs = map (\(n, nt) -> (nodeId n, nodeId nt)) .
            filter (\(n,nt) -> infoLabel (fromJust $ nodeInfo nt) == infoType (nodeInfo n)) $ mkpairs (nodes g) (nodes tg)
    g' = fromNodesAndEdges (map nodeToJust (nodes g)) (map edgeToJust (edges g))


graphToRule :: Graph String String -- rule graph
            -> [NAC] -- nac graphs for rules
            -> TypeGraph String String
            -> IO (TypedGraphRule String String)
graphToRule ruleGraph nacs typeGraph = do
  nmsTgm <- mapM (\(n,m) -> formatNac n lm m) nms
  return $ Production lhsTgm rhsTgm nmsTgm
  where
    (lhs, k, rhs) = graphToRuleGraphs ruleGraph
    lm = makeTypedGraph lhs typeGraph
    rm = makeTypedGraph rhs typeGraph
    km = makeTypedGraph k typeGraph
    nms = map (\(n,m) -> (makeTypedGraph n typeGraph, m)) nacs

    lhsTgm = TGM.fromGraphsAndLists km lm leftNodeMapping leftEdgeMapping
    rhsTgm = TGM.fromGraphsAndLists km rm rightNodeMapping rightEdgeMapping

    leftNodeMapping = filter (\(n1,n2) -> n1 == n2) $ mkpairs (nodeIds k) (nodeIds lhs)
    leftEdgeMapping = filter (\(n1,n2) -> n1 == n2) $ mkpairs (edgeIds k) (edgeIds lhs)

    rightNodeMapping = filter (\(n1,n2) -> n1 == n2) $ mkpairs (nodeIds k) (nodeIds rhs)
    rightEdgeMapping = filter (\(n1,n2) -> n1 == n2) $ mkpairs (edgeIds k) (edgeIds rhs)



makeTypeGraph :: Graph a b -> TypeGraph a b
makeTypeGraph g = fromNodesAndEdges nds edgs
  where
    nds = map (\n -> Node (nodeId n) $ Just (nodeInfo n)) $ nodes g
    edgs = map (\e -> Edge (edgeId e) (sourceId e) (targetId e) (Just $ edgeInfo e)) $ edges g


makeGrammar :: Graph Info Info -> Graph Info Info -> [(Graph Info Info, [NAC])] -> [String] -> IO (Maybe (Grammar (TGM.TypedGraphMorphism String String)))
makeGrammar tg hg rgs rulesNames = do

  let typegraph = makeTypeGraph tg
      initGraph = makeTypedGraph hg typegraph
  productions <- mapM (\(r,ns) -> graphToRule r ns typegraph) rgs

  ensureValid $ validateNamed (\name -> "Rule '"++name++"'") (zip rulesNames productions)
  if (L.null productions)
    then return Nothing
    else return $ Just $ grammar initGraph [] (zip rulesNames productions)
