module GUI.Editor.Helper.GrammarMaker (
  makeGrammar
, graphToRuleGraphs
, makeTypeGraph
, makeTypedGraph
, getNacPushout
, nodeToJust
, edgeToJust
, nodeFromJust
, edgeFromJust
)where

import Data.Maybe
import Data.Either
import Data.Graphs
import qualified Data.List as L
import qualified Data.Map as M

import Abstract.Category.Limit
import Abstract.Rewriting.DPO
import Base.Valid
import Category.TypedGraph
import Category.TypedGraph.Limit
import Rewriting.DPO.TypedGraph
import qualified Data.TypedGraph as TG
import qualified Data.TypedGraph.Morphism as TGM

import GUI.Data.Info
import GUI.Helper.List
import Data.Graphs.Morphism
import GUI.Data.Nac

type RuleGraphs = (Graph Info Info, Graph Info Info, Graph Info Info)
type TypeGraph a b = Graph (Maybe a) (Maybe b) -- may delete this if import span
type NAC = (Graph Info Info, MergeMapping)

nodeToJust :: Node a -> Node (Maybe a)
nodeToJust n = Node (nodeId n) (Just $ nodeInfo n)

edgeToJust :: Edge a -> Edge (Maybe a)
edgeToJust e = Edge (edgeId e) (sourceId e) (targetId e) (Just $ edgeInfo e)

nodeFromJust :: Node (Maybe a) -> Node a
nodeFromJust n = Node (nodeId n) (fromJust $ nodeInfo n)

edgeFromJust :: Edge (Maybe a) -> Edge a
edgeFromJust e = Edge (edgeId e) (sourceId e) (targetId e) (fromJust $ edgeInfo e)


graphToRuleGraphs :: Graph Info Info
                  -> RuleGraphs
graphToRuleGraphs g = (lhs, k, rhs)
  where
    nods = nodes g
    edgs = edges g

    kNodes = filter (\n -> infoOperation (nodeInfo n) == Preserve) nods
    kEdges = filter (\e -> infoOperation (edgeInfo e) == Preserve) edgs
    k = fromNodesAndEdges kNodes kEdges

    lhsNodes = filter (\n -> infoOperation (nodeInfo n) == Delete) nods ++ kNodes
    lhsEdges = filter (\e -> infoOperation (edgeInfo e) == Delete) edgs ++ kEdges
    lhs = fromNodesAndEdges lhsNodes lhsEdges

    rhsNodes = filter (\n -> infoOperation (nodeInfo n) == Create) nods ++ kNodes
    rhsEdges = filter (\e -> infoOperation (edgeInfo e) == Create) edgs ++ kEdges
    rhs = fromNodesAndEdges rhsNodes rhsEdges

-- | given two TypedGraphs - nac and lhs
-- and a tuple of maps of ids - (nmap, emap),
-- calculate the pushout of nac <- k -> lhs, where
--  k is the interface between nac and lhs
--  the pushout is given by two typedGraphMorphisms (tgmNac',tgmLhs')
getNacPushout :: TypedGraph Info Info
              -> TypedGraph Info Info
              -> (M.Map NodeId NodeId, M.Map EdgeId EdgeId)
              -> (TypedGraphMorphism Info Info, TypedGraphMorphism Info Info)
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


makeTypedGraph :: Graph Info Info
               -> TypeGraph Info Info
               -> TG.TypedGraph Info Info
makeTypedGraph g tg = fromGraphsAndLists g' tg npairs epairs
  where
    -- auxiliar structs to define the typedGraph
    epairs = map (\(e, et) -> (edgeId e, edgeId et)) .
            filter (\(e,et) -> let Label eLbl = infoLabel (fromJust $ edgeInfo et)
                                   eType = infoType (edgeInfo e)
                                   Label srcLbl = infoLabel . fromJust . nodeInfo . fromJust $ lookupNode (sourceId et) tg
                                   Label tgtLbl = infoLabel . fromJust . nodeInfo . fromJust $ lookupNode (targetId et) tg
                                   srcType = infoType . nodeInfo . fromJust $ lookupNode (sourceId e) g
                                   tgtType = infoType . nodeInfo . fromJust $ lookupNode (targetId e) g
                               in eLbl == eType && srcLbl == srcType && tgtLbl == tgtType)
            $ mkpairs (edges g) (edges tg)
    npairs = map (\(n, nt) -> (nodeId n, nodeId nt)) .
            filter (\(n,nt) -> let Label lbl = infoLabel (fromJust $ nodeInfo nt)
                               in lbl == infoType (nodeInfo n))
            $ mkpairs (nodes g) (nodes tg)
    g' = fromNodesAndEdges (map nodeToJust (nodes g)) (map edgeToJust (edges g))


graphToRule :: Graph Info Info -- rule graph
            -> [NAC] -- nac graphs for rules
            -> TypeGraph Info Info
            -> TypedGraphRule Info Info
graphToRule ruleGraph nacs typeGraph = Production lhsTgm rhsTgm nmsTgm
  where
    (lhs, k, rhs) = graphToRuleGraphs ruleGraph
    lm = makeTypedGraph lhs typeGraph
    rm = makeTypedGraph rhs typeGraph
    km = makeTypedGraph k typeGraph
    nms = map (\(n,m) -> (makeTypedGraph n typeGraph, m)) nacs

    lhsTgm = TGM.fromGraphsAndLists km lm leftNodeMapping leftEdgeMapping
    rhsTgm = TGM.fromGraphsAndLists km rm rightNodeMapping rightEdgeMapping
    nmsTgm = map (\(n,m) -> fst $ getNacPushout n lm m) nms

    leftNodeMapping = filter (\(n1,n2) -> n1 == n2) $ mkpairs (nodeIds k) (nodeIds lhs)
    leftEdgeMapping = filter (\(n1,n2) -> n1 == n2) $ mkpairs (edgeIds k) (edgeIds lhs)

    rightNodeMapping = filter (\(n1,n2) -> n1 == n2) $ mkpairs (nodeIds k) (nodeIds rhs)
    rightEdgeMapping = filter (\(n1,n2) -> n1 == n2) $ mkpairs (edgeIds k) (edgeIds rhs)



makeTypeGraph :: Graph a b -> TypeGraph a b
makeTypeGraph g = fromNodesAndEdges nds edgs
  where
    nds = map (\n -> Node (nodeId n) $ Just (nodeInfo n)) $ nodes g
    edgs = map (\e -> Edge (edgeId e) (sourceId e) (targetId e) (Just $ edgeInfo e)) $ edges g


makeGrammar :: Graph Info Info
            -> Graph Info Info
            -> [(Graph Info Info, [NAC])]
            -> [String]
            -> Either String (Grammar (TGM.TypedGraphMorphism Info Info))
makeGrammar tg hg rgs rulesNames =
  case eGrammar of
    Left msgs -> Left $ L.intercalate "\n" msgs
    Right grammar -> case validate grammar of
      IsValid -> Right grammar
      IsInvalid msgs -> Left $ L.intercalate "\n" msgs
  where
    typegraph = makeTypeGraph tg
    initGraph = makeTypedGraph hg typegraph
    productions = map (\(r,nacs) -> graphToRule r nacs typegraph) rgs

    vProductions = validateNamed (\name -> "Rule '"++name++"'") (zip rulesNames productions)

    eGrammar = case (L.null productions, vProductions) of
      (True,_) -> Left ["No rules"]
      (False, IsValid) -> Right $ grammar initGraph [] (zip rulesNames productions)
      (False, IsInvalid msg) -> Left msg
