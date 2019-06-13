module Editor.GrammarMaker (
  graphToRuleGraphs
)where

import Data.Graphs
import Editor.Info

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
