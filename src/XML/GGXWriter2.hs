module XML.GGXWriter2 (
  writeGGX
)where

import           Data.Maybe
import           Text.XML.HXT.Core
import           Data.List
import qualified Data.Map as M
import qualified Data.Tree as Tree
import qualified Data.Graphs as G
import           XML.Utilities
import qualified System.FilePath as FilePath

import           GUI.Data.Info
import           GUI.Data.SaveInfo
import           GUI.Data.GraphState
import           GUI.Data.GraphicalInfo
import           GUI.Data.Nac (NacInfo)

import           GUI.Helper.GrammarMaker

type TypeMapping = (M.Map String String, M.Map (String,String,String) String)

writeGGX :: Tree.Forest SaveInfo -> String -> IO ()
writeGGX saveInfo fileName = do
  let ggname = FilePath.takeBaseName fileName
  let mGGTypes = getTypes saveInfo
  case mGGTypes of
    Just ggTypes -> do
      runX $ root [] [writeRoot saveInfo ggTypes ggname] >>> writeDocument [withIndent yes] fileName
      putStrLn $ "saved ggx in " ++ fileName
      return ()
    Nothing -> putStrLn "Error: could not infer types of the grammar"
  return ()


getTypes :: Tree.Forest SaveInfo -> Maybe TypeMapping
getTypes [] = Nothing
getTypes ((Tree.Node (TypeGraph gid name gst) _):_) =
  let g = stateGetGraph gst
  in Just (getNodeTypes (G.nodes g), getEdgeTypes (G.edgesInContext g))
getTypes (_:ls) = getTypes ls

-- given a node list, creates a Map that relates their label to their ids
-- node labels -> "N{node ID}"
getNodeTypes :: [G.Node Info] -> (M.Map String String)
getNodeTypes nodes = M.fromList $ map (\n -> (infoLabelStr $ G.nodeInfo n, 'N':(show . fromEnum $ G.nodeId n))) nodes

-- given a node and a edge list, creates a Map that relates their and their connecting nodels labels to their ids
-- (src label, edge label, tgt label) -> "E{edge ID}"
getEdgeTypes :: [G.EdgeInContext Info Info] -> (M.Map (String,String,String) String)
getEdgeTypes edges =
  M.fromList
  $ map (\((src,_),e,(tgt,_)) ->
      let srcLabel = infoLabelStr $ G.nodeInfo src
          tgtLabel = infoLabelStr $ G.nodeInfo tgt
          edgeLabel = infoLabelStr $ G.edgeInfo e
          edgeIdStr = 'E':(show . fromEnum $ G.edgeId e)
      in ( (srcLabel, edgeLabel, tgtLabel) , edgeIdStr))
  edges

writeRoot :: ArrowXml a => Tree.Forest SaveInfo -> TypeMapping -> String -> a XmlTree XmlTree
writeRoot saveInfo ggTypes ggname = mkelem "Document" [sattr "version" "1.0"] [writeGts saveInfo ggTypes ggname]

writeGts :: ArrowXml a => Tree.Forest SaveInfo -> TypeMapping -> String -> a XmlTree XmlTree
writeGts saveInfo ggTypes ggname = mkelem "GraphTransformationSystem" (sattr "name" ggname : defaultGtsAttributes) $ writeGrammar saveInfo ggTypes

defaultGtsAttributes :: ArrowXml a => [a n XmlTree]
defaultGtsAttributes = [ sattr "ID" "I1", sattr "directed" "true", sattr "parallel" "true" ]

writeGrammar :: ArrowXml a => Tree.Forest SaveInfo -> TypeMapping -> [a XmlTree XmlTree]
writeGrammar saveInfo ggTypes = writeAggProperties ++ writeSIForest saveInfo ggTypes

writeAggProperties :: ArrowXml a => [a XmlTree XmlTree]
writeAggProperties = writeAttrHandler : writeTaggedValues defaultProperties

defaultProperties :: [(String, String)]
defaultProperties = [("CSP","true"),("dangling","true"),("identification","true"),
  ("NACs","true"),("PACs","true"),("GACs","true"),("breakAllLayer","true"),
  ("showGraphAfterStep","true"),("TypeGraphLevel","ENABLED")]

writeTaggedValues :: ArrowXml a => [(String, String)] -> [a XmlTree XmlTree]
writeTaggedValues = map writeTaggedValue

writeTaggedValue :: ArrowXml a => (String, String) -> a XmlTree XmlTree
writeTaggedValue (tag, value) = mkelem "TaggedValue" [sattr "Tag" tag,
                              sattr "TagValue" value] []

writeAttrHandler :: ArrowXml a => a XmlTree XmlTree
writeAttrHandler =
  mkelem "TaggedValue" [sattr "Tag" "AttrHandler", sattr "TagValue" "Java Expr"]
  $ writeTaggedValues [("Package", "java.lang"), ("Package", "java.util")]

writeSIForest :: ArrowXml a => Tree.Forest SaveInfo -> TypeMapping -> [a XmlTree XmlTree]
writeSIForest [] _ = []
writeSIForest (t:fs) ggTypes =
  case t of
    (Tree.Node (Topic _) tfs) -> writeSIForest (tfs ++ fs) ggTypes
    _ -> (writeSITree t ggTypes):(writeSIForest fs ggTypes)

writeSITree :: ArrowXml a => Tree.Tree SaveInfo -> TypeMapping -> a XmlTree XmlTree
writeSITree (Tree.Node (TypeGraph gid name gst) _) ggTypes = writeTypes ("G" ++ (show gid)) name (stateGetGraph gst) (stateGetGI gst) ggTypes
writeSITree (Tree.Node (HostGraph gid name gst) _) ggTypes = writeGraph ("G" ++ (show gid)) name "HOST" (stateGetGraph gst) (stateGetGI gst) ggTypes
writeSITree (Tree.Node (RuleGraph gid name gst act) fs) ggTypes = writeRule ("R" ++ (show gid)) name (stateGetGraph gst) (stateGetGI gst) ggTypes fs
writeSITree _ _ = mkelem "error" [] []

writeTypes :: ArrowXml a => String -> String -> G.Graph Info Info -> GraphicalInfo -> TypeMapping -> a XmlTree XmlTree
writeTypes gid name graph layouts ggTypes =
  mkelem "Types" []
  $ map (\n -> writeNodeType n (fst layouts)) (G.nodes graph) ++ map (\e -> writeEdgeType e (snd layouts)) (G.edges graph)
  ++ [writeGraph gid name "TG" graph layouts ggTypes]

writeNodeType :: ArrowXml a => G.Node Info -> M.Map Int NodeGI -> a XmlTree XmlTree
writeNodeType node layouts = mkelem "NodeType" [sattr "ID" ('N' : (show . fromEnum . G.nodeId $ node )), sattr "abstract" "false", sattr "name" name] []
  where
    layout = fromMaybe newNodeGI $ M.lookup (fromEnum $ G.nodeId node) layouts
    shapeStr = case shape layout of
                  NCircle -> ":CIRCLE:"
                  NSquare -> ":SQUARE:"
                  NRect -> ":ROUNDRECT:"
    (r,g,b) = (\f (a,b,c) -> (f a, f b, f c)) (show . round . (*255)) (if fillColor layout == (1,1,1) then (0,0,0) else fillColor layout)
    colorStr = "java.awt.Color[r=" ++ r ++ ",g=" ++ g ++ ",b=" ++ b ++ "]"
    name = (infoLabelStr (G.nodeInfo node)) ++ "%" ++ shapeStr ++ colorStr ++ ":[NODE]:"

writeEdgeType :: ArrowXml a => G.Edge Info -> M.Map Int EdgeGI -> a XmlTree XmlTree
writeEdgeType edge layouts = mkelem "EdgeType" [sattr "ID" ('E': (show . fromEnum . G.edgeId $ edge)), sattr "abstract" "false", sattr "name" name] []
  where
    layout = fromMaybe newEdgeGI $ M.lookup (fromEnum $ G.edgeId edge) layouts
    styleStr = case style layout of
                ENormal -> ":SOLID_LINE:"
                ESlashed -> ":DASH_LINE:"
                EPointed -> ":DOT_LINE:"
    (r,g,b) = (\f (a,b,c) -> (f a, f b, f c)) (show . round . (*255)) (color layout)
    colorStr = "java.awt.Color[r=" ++ r ++ ",g=" ++ g ++ ",b=" ++ b ++ "]"
    name = (infoLabelStr (G.edgeInfo edge)) ++ "%" ++ styleStr ++ colorStr ++ ":[EDGE]:"

writeRule :: ArrowXml a => String -> String -> G.Graph Info Info -> GraphicalInfo -> TypeMapping -> Tree.Forest SaveInfo -> a XmlTree XmlTree
writeRule ruleId ruleName ruleGraph layouts ggTypes subForest =
  mkelem "Rule" [sattr "ID" ruleId, sattr "formula" "true", sattr "name" ruleName]
        $ [ writeGraph (ruleId++"_LHS") "Left" "LHS" lhs lhslayouts ggTypes
         , writeGraph (ruleId++"_RHS") "Right" "RHS" rhs rhslayouts ggTypes
         , writeMorphismFromK ruleName (ruleId++"_LHS") (ruleId++"_RHS") k ]
        ++ writeApplConditions (ruleId++"_LHS") subForest ggTypes
  where
    (lhs,k,rhs) = graphToRuleGraphs ruleGraph
    filterLayouts g (nl,el) = (M.filterWithKey (\k _ -> G.NodeId k `elem` (G.nodeIds g)) nl, M.filterWithKey (\k _ -> G.EdgeId k `elem` (G.edgeIds g)) el)
    lhslayouts = filterLayouts lhs layouts
    rhslayouts = filterLayouts rhs layouts

writeApplConditions :: ArrowXml a => String -> Tree.Forest SaveInfo -> TypeMapping -> [a XmlTree XmlTree]
writeApplConditions lhsPrefix [] ggTypes = []
writeApplConditions lhsPrefix saveInfo ggTypes =
  [mkelem "ApplCondition" [] (map (writeApplCondition lhsPrefix ggTypes) saveInfo)]

writeApplCondition :: ArrowXml a => String -> TypeMapping -> Tree.Tree SaveInfo -> a XmlTree XmlTree
writeApplCondition lhsPrefix ggTypes (Tree.Node (NacGraph gid name nacInfo) _) = writeNac lhsPrefix ("N"++(show gid)) name nacInfo ggTypes
writeApplCondition lhsPrefix ggTypes _ = mkelem "error" [] []

writeNac :: ArrowXml a => String -> String -> String -> NacInfo -> TypeMapping -> a XmlTree XmlTree
writeNac lhsPrefix nacID name ((g,l),mergeMapping) ggTypes =
  mkelem "NAC" [] [writeGraph nacID name "NAC" g l ggTypes, writeMorphismFromMapping name lhsPrefix nacID mergeMapping]

writeMorphismFromMapping :: ArrowXml a => String -> String -> String -> (M.Map G.NodeId G.NodeId, M.Map G.EdgeId G.EdgeId) -> a XmlTree XmlTree
writeMorphismFromMapping name prefixOrig prefixImg (nodeMapping, edgeMapping) =
  mkelem "Morphism" [sattr "name" name] $ map writeMapping mapping
  where
    nID = show . fromEnum
    eID = show . fromEnum
    nodeMapping' = map (\(k,n) -> (prefixOrig ++ "_n" ++ (nID k), prefixImg ++ "_n" ++ (nID n))) $ M.toList nodeMapping
    edgeMapping' = map (\(k,e) -> (prefixOrig ++ "_e" ++ (eID k), prefixImg ++ "_e" ++ (eID e))) $ M.toList edgeMapping
    mapping = nodeMapping' ++ edgeMapping'

writeMorphismFromK :: ArrowXml a => String -> String -> String -> G.Graph Info Info -> a XmlTree XmlTree
writeMorphismFromK name prefixOrig prefixImg k =
  mkelem "Morphism" [sattr "name" name] $ map writeMapping mapping
  where
    nID = show . fromEnum
    eID = show . fromEnum
    nodeMapping = map (\n -> (prefixOrig ++ "_n" ++ (nID n), prefixImg ++ "_n" ++ (nID n))) $ G.nodeIds k
    edgeMapping = map (\e -> (prefixOrig ++ "_e" ++ (eID e), prefixImg ++ "_e" ++ (eID e))) $ G.edgeIds k
    mapping = nodeMapping ++ edgeMapping

writeMapping :: ArrowXml a => (String,String) -> a XmlTree XmlTree
writeMapping (orig,image) = mkelem "Mapping" [sattr "orig" orig, sattr "image" image] []


writeGraph :: ArrowXml a => String -> String -> String -> G.Graph Info Info -> GraphicalInfo -> TypeMapping -> a XmlTree XmlTree
writeGraph graphId name graphType graph layouts ggTypes =
  mkelem "Graph" [sattr "ID" graphId, sattr "kind" graphType, sattr "name" name]
  $ (map (\n -> writeNode graphId n (fst layouts) ggTypes) (G.nodes graph)) ++ (map (\e -> writeEdge graphId e ggTypes) (G.edgesInContext graph))

writeNode :: ArrowXml a => String -> G.Node Info -> M.Map Int NodeGI -> TypeMapping -> a XmlTree XmlTree
writeNode prefix node layouts ggTypes = mkelem "Node" [sattr "ID" idString, sattr "type" ntype] $ [writeNodeLayout layout, writeNodeAdditionalLayout]
  where nid = show . fromEnum $ G.nodeId node
        idString = prefix ++ "_n" ++ nid
        ntype = fromMaybe ('N':nid) $ M.lookup (infoType (G.nodeInfo node)) (fst ggTypes)
        layout = fromMaybe newNodeGI $ M.lookup (fromEnum $ G.nodeId node) layouts

writeNodeLayout :: ArrowXml a => NodeGI -> a XmlTree XmlTree
writeNodeLayout layout = mkelem "NodeLayout" [sattr "X" x, sattr "Y" y] []
  where
    x = show . round . fst $ position layout
    y = show . round . snd $ position layout

writeNodeAdditionalLayout :: ArrowXml a => a XmlTree XmlTree
writeNodeAdditionalLayout = mkelem "additionalLayout" [sattr "age" "0", sattr "force" "10", sattr "frozen" "true", sattr "zone" "50"] []

writeEdge :: ArrowXml a => String -> G.EdgeInContext Info Info -> TypeMapping -> a XmlTree XmlTree
writeEdge prefix ((src,_), e, (tgt,_)) ggTypes = mkelem "Edge" [sattr "ID" idStr, sattr "source" srcStr, sattr "target" tgtStr, sattr "type" etype] []
  where eid = show . fromEnum $ G.edgeId e
        idStr = prefix ++ "_e" ++ eid
        srcStr = prefix ++ "_n" ++ (show . fromEnum $ G.nodeId src)
        tgtStr = prefix ++ "_n" ++ (show . fromEnum $ G.nodeId tgt)
        srcTypeLabel = infoType $ G.nodeInfo src
        tgtTypeLabel = infoType $ G.nodeInfo tgt
        eTypeLabel = infoType $ G.edgeInfo e
        etype = fromMaybe ('E':eid) $ M.lookup (srcTypeLabel,eTypeLabel,tgtTypeLabel) (snd ggTypes)
