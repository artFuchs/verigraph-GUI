module GUI.XML.VGGXWritter (
    writeVGGX
)
where

import           Text.XML.HXT.Core

import qualified Data.Tree as Tree
import qualified Data.Map as M

import qualified Data.Graphs as G

import           GUI.Data.SaveInfo
import           GUI.Data.GraphicalInfo
import           GUI.Data.GraphState
import           GUI.Data.Info
import           GUI.Data.Nac (MergeMapping)

writeVGGX :: Tree.Forest SaveInfo -> String -> IO ()
writeVGGX saveInfo fileName = do
    runX $ root [] [writeRoot saveInfo] >>> writeDocument [withIndent yes] fileName
    return ()

writeRoot :: ArrowXml a => Tree.Forest SaveInfo -> a XmlTree XmlTree
writeRoot saveInfo = mkelem "Document" [sattr "version" "0.2.5"] [writeGrammar saveInfo]

writeGrammar :: ArrowXml a => Tree.Forest SaveInfo -> a XmlTree XmlTree
writeGrammar saveInfo = mkelem "Grammar" [] $ writeSIForest saveInfo

writeSIForest :: ArrowXml a => Tree.Forest SaveInfo -> [a XmlTree XmlTree]
writeSIForest [] = []
writeSIForest (tree:forest) = (writeSITree tree):(writeSIForest forest)

writeSITree :: ArrowXml a => Tree.Tree SaveInfo -> a XmlTree XmlTree
writeSITree (Tree.Node (Topic name) fs) = mkelem name [] $ writeSIForest fs
writeSITree (Tree.Node (TypeGraph id name gst) fs) = mkelem "TypeGraph" [sattr "id" (show id), sattr "name" name] [writeGraphState gst]
writeSITree (Tree.Node (HostGraph id name gst) fs) = mkelem "HostGraph" [sattr "id" (show id), sattr "name" name] [writeGraphState gst]
writeSITree (Tree.Node (RuleGraph id name gst active) fs) =
    mkelem "Rule"
    [sattr "id" (show id), sattr "name" name, sattr "active" (show active)]
    [writeGraphState gst, mkelem "NACs" [] $ writeSIForest fs]
writeSITree (Tree.Node (NacGraph id name ((g,gi),mergeMap) ) fs) =
    mkelem "NAC"
    [sattr "id" (show id), sattr "name" name]
    [writeGraph g gi, writeMergeMapping mergeMap]

writeGraphState :: ArrowXml a => GraphState -> a XmlTree XmlTree
writeGraphState gst = writeGraph (stateGetGraph gst) (stateGetGI gst)

writeGraph :: ArrowXml a => G.Graph Info Info -> GraphicalInfo -> a XmlTree XmlTree
writeGraph graph (ngiM, egiM) = mkelem "Graph" [] [writeNodes (G.nodes graph) ngiM, writeEdges (G.edges graph) egiM]

writeNodes :: ArrowXml a => [G.Node Info] -> M.Map Int NodeGI -> a XmlTree XmlTree
writeNodes nodes giM = mkelem "Nodes" [] $ map (\n -> writeNode n (getNodeGI (fromEnum $ G.nodeId n) giM) ) nodes

writeNode :: ArrowXml a => G.Node Info -> NodeGI -> a XmlTree XmlTree
writeNode node gi = mkelem "Node" [sattr "id" (show . fromEnum $ G.nodeId node)] [writeInfo (G.nodeInfo node), writeNodeGI gi]

writeNodeGI :: ArrowXml a => NodeGI -> a XmlTree XmlTree
writeNodeGI gi = mkelem "NodeLayout" [ sattr "position" (show $ position gi)
                                 , sattr "fillColor" (show $ fillColor gi)
                                 , sattr "lineColor" (show $ lineColor gi)
                                 , sattr "dimension" (show $ dims gi)
                                 , sattr "shape" (show $ shape gi)
                                 ]
                                 []

writeEdges :: ArrowXml a => [G.Edge Info] -> M.Map Int EdgeGI -> a XmlTree XmlTree
writeEdges edges giM = mkelem "Edges" [] $ map (\e -> writeEdge e (getEdgeGI (fromEnum $ G.edgeId e) giM) ) edges

writeEdge :: ArrowXml a => G.Edge Info -> EdgeGI -> a XmlTree XmlTree
writeEdge edge gi = mkelem "Edge" [sattr "id" eid, sattr "source" src, sattr "target" tgt] [writeInfo (G.edgeInfo edge), writeEdgeGI gi]
    where
        eid = show . fromEnum $ G.edgeId edge
        src = show . fromEnum $ G.sourceId edge
        tgt = show . fromEnum $ G.targetId edge

writeEdgeGI :: ArrowXml a => EdgeGI -> a XmlTree XmlTree
writeEdgeGI gi = mkelem "EdgeLayout" [ sattr "position" (show $ cPosition gi)
                                 , sattr "color" (show $ color gi)
                                 , sattr "style" (show $ style gi)
                                 ]
                                 []

writeInfo :: ArrowXml a => Info -> a XmlTree XmlTree
writeInfo info = mkelem "Info" [ sattr "locked" (show $ infoLocked info)
                               , sattr "type" (infoType info)
                               , sattr "operation" (show $ infoOperation info)]
                               [ writeInfoLabel (infoLabel info) ]

writeInfoLabel :: ArrowXml a => InfoLabel -> a XmlTree XmlTree
writeInfoLabel (Label lbl) = mkelem "Label" [sattr "text" lbl, sattr "group" "False"] []
writeInfoLabel (LabelGroup group) = mkelem "Label" [sattr "group" "True"] $ map writeElement group
    where writeElement (id,str) = mkelem "LabelElement" [sattr "id" (show id), sattr "text" str] []

writeMergeMapping :: ArrowXml a => MergeMapping -> a XmlTree XmlTree
writeMergeMapping (nodeMapping,edgeMapping) =
    mkelem "MergeMapping" []
        [ mkelem "Nodes" [] $ map writeNodeMapping (M.toList nodeMapping)
        , mkelem "Edges" [] $ map writeEdgeMapping (M.toList edgeMapping)
        ]

writeNodeMapping :: ArrowXml a => (G.NodeId,G.NodeId) -> a XmlTree XmlTree
writeNodeMapping (k,a) = mkelem "NodeMapping" [sattr "src" (show $ fromEnum k), sattr "tgt" (show $ fromEnum a)] []

writeEdgeMapping :: ArrowXml a => (G.EdgeId,G.EdgeId) -> a XmlTree XmlTree
writeEdgeMapping (k,a) = mkelem "EdgeMapping" [sattr "src" (show $ fromEnum k), sattr "tgt" (show $ fromEnum a)] []
