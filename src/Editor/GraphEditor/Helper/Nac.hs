module Editor.GraphEditor.Helper.Nac(
  joinNAC
, remapElementsWithConflict
, applyLhsChangesToNac
)where

import qualified GI.Pango as P
import Data.Maybe
import qualified Data.Map as M

import Data.Graphs hiding (null)
import qualified Data.Graphs.Morphism as Morph
import qualified Data.Relation as R
import qualified Data.TypedGraph as TG
import qualified Data.TypedGraph.Morphism as TGM

import Editor.Data.DiaGraph
import Editor.Data.EditorState
import Editor.Data.Info
import Editor.Data.Nac
import Editor.GraphEditor.Helper.GrammarMaker
import Editor.GraphEditor.Helper.GraphicalInfo
import Editor.Helper.List

-- for debugging
import Control.Monad
import Control.Monad.IO.Class
import Editor.Data.GraphicalInfo


-- | Join the part of the nac and the lhs of the rule, showing the complete nac.
joinNAC :: NacInfo -> DiaGraph -> Graph Info Info -> IO DiaGraph
joinNAC (nacdg, (nM,eM)) lhsdg@(ruleLG,ruleLGI) tg = do
  return (nG, nGI')
  where
    -- change ids of elements of nacs so that those elements don't have conflicts
    -- with elements from lhs
    (nacG,nacGI) = remapElementsWithConflict lhsdg nacdg (nM,eM)
    -- apply a pushout in  elements of nac
    tg' = makeTypeGraph tg
    lm = makeTypedGraph ruleLG tg'
    nm = makeTypedGraph nacG tg'
    (nacTgmLhs,nacTgmNac) = getNacPushout nm lm (nM, eM)
    -- inverse relations from graphs resulting of pushout to change their elements ids to the correct ones
    nIRLHS = R.inverseRelation $ Morph.nodeRelation $ TGM.mapping nacTgmLhs
    eIRLHS = R.inverseRelation $ Morph.edgeRelation $ TGM.mapping nacTgmLhs
    nIRNAC = R.inverseRelation $ Morph.nodeRelation $ TGM.mapping nacTgmNac
    eIRNAC = R.inverseRelation $ Morph.edgeRelation $ TGM.mapping nacTgmNac
    -- get nac graph with info packed into Just
    nGJust = TG.toUntypedGraph $ TGM.codomainGraph nacTgmLhs
    -- change Ids of elements of nGJust and extract the info from Just
    swapNodeId' n = case (R.apply nIRLHS n ++ R.apply nIRNAC n) of
                      []       -> n
                      nids -> maximum (nids)
    swapNodeId n = Node (swapNodeId' (nodeId n)) (nodeInfo n)
    swapEdgeId e = case (R.apply eIRLHS (edgeId e) ++ R.apply eIRNAC (edgeId e)) of
                      []       -> e
                      eids -> Edge (maximum eids) (swapNodeId' $ sourceId e) (swapNodeId' $ targetId e) (edgeInfo e)
    nGnodes = map swapNodeId . map nodeFromJust $ nodes nGJust
    nGedges = map swapEdgeId . map edgeFromJust $ edges nGJust
    nG = fromNodesAndEdges nGnodes nGedges
    -- join the GraphicalInfos
    nGI = (M.union (fst nacGI) (fst ruleLGI), M.union (snd nacGI) (snd ruleLGI))

    -- modify the position of the loops of merged nodes
    ----------------------------------------------------------------------------------------------                
    -- auxiliar function
    -- (EdgeId, Double) -> Double -> (Int, EdgeGI)
    updEdgeY (eid,yRel) minY = let k = (fromEnum eid)
                                   egi = (getEdgeGI k (snd nGI)) {cPosition = (-pi/2,yRel + minY)}
                               in (k,egi)

    -- foreach merged node, check if there are loops pointing to it
    mergeNodesInContext = map (\nid -> fromJust $ lookupNodeInContext nid nG) $ removeDuplicates (M.elems nM)
    loops = map (\nc -> (nodeId (fst nc), edgesFromTo nc nc)) mergeNodesInContext -- :: [(NodeId,[Edge])]
    
    -- foreach loop in the list, increment the distance from the node
    -- first, calculate the relative distance foreach (Y here means the distance)
    calcRelY l = foldr (\e lPos -> (edgeId e, 30 * (1 + (fromIntegral $ length lPos))::Double):lPos) [] l
    loopsRelY = map (\(nid,e) -> (nid,calcRelY e)) loops -- :: [(NodeId,[(EdgeId,Double)])]

    -- then update the edges GraphicalInfos with this value + the height of the node            
    loopsGIs =  concat $ 
                map (\(nid,loopsInfos) -> let ngi = fromJust $ M.lookup (fromEnum nid) (fst nGI)
                                              ndims = dims ngi
                                              minY = case shape ngi of
                                                      NCircle -> (max (fst ndims) (snd ndims)/2)
                                                      NSquare -> (max (fst ndims) (snd ndims)/2)
                                                      NRect -> snd ndims
                                          in map (\lInfo -> updEdgeY lInfo minY) loopsInfos)
                loopsRelY

    nacEgi' = foldr (\(k,gi) giM -> M.insert k gi giM) (snd nGI) loopsGIs
    nGI' = (fst nGI, nacEgi')
    ----------------------------------------------------------------------------------------------

-- | Modify ids of nac nodes that aren't in mapping and have id conflict with elems from lhs.
-- (g1,(ngi1,egi1)) must be the lhs diagraph, while (g2,(ngi2,egi2)) must be the nac diagraph
remapElementsWithConflict :: DiaGraph -> DiaGraph -> MergeMapping -> DiaGraph
remapElementsWithConflict (g1,(ngi1,egi1)) (g2,(ngi2,egi2)) (nodeMapping, edgeMapping) = (g3,gi3)
  where
    g3 = fromNodesAndEdges g3Nodes g3Edges
    gi3 = (ngi3,egi3)
    egi3 = M.fromList $ map (\(k,a) -> let k' = fromEnum . fromMaybe (EdgeId k) $ M.lookup (EdgeId k) newEids
                                        in (k',a))
                            (M.toList egi2)
    ngi3 = M.fromList $ map (\(k,a) -> let k' = fromEnum . fromMaybe (NodeId k) $ M.lookup (NodeId k) newNids
                                        in (k',a))
                            (M.toList ngi2)
    possibleNConflicts1 = filter (\n -> notElem n (M.keys nodeMapping)) $ nodeIds g1
    possibleNConflicts2 = filter (\n -> notElem n (M.elems nodeMapping)) $ nodeIds g2
    possibleEConflicts1 = filter (\e -> notElem e (M.keys edgeMapping)) $ edgeIds g1
    possibleEConflicts2 = filter (\e -> notElem e (M.elems edgeMapping)) $ edgeIds g2
    nConflicts = filter (\(a,b) -> a == b) $ mkpairs possibleNConflicts1 possibleNConflicts2
    eConflicts = filter (\(a,b) -> a == b) $ mkpairs possibleEConflicts1 possibleEConflicts2
    newNids = M.fromList $ zipWith (\(a,b) n -> (a, n)) nConflicts (newNodes g1)
    newEids = M.fromList $ zipWith (\(a,b) n -> (a, n)) eConflicts (newEdges g1)
    g3Nodes = map (\n -> let nid = fromMaybe (nodeId n) $ M.lookup (nodeId n) newNids
                          in Node nid (nodeInfo n))
                  (nodes g2)
    g3Edges = map (\e -> let srcNid = fromMaybe (sourceId e) $ M.lookup (sourceId e) newNids
                             tgtNid = fromMaybe (targetId e) $ M.lookup (targetId e) newNids
                             eid = fromMaybe (edgeId e) $ M.lookup (edgeId e) newEids
                          in Edge eid srcNid tgtNid (edgeInfo e))
                  (edges g2)

-- | update the nacGraph according to the lhs
-- first delete the elements that should be in the LHS but are not.
-- then change the types of elements of nacg to avoid type errors
applyLhsChangesToNac :: Graph Info Info -> NacInfo -> Maybe P.Context -> IO NacInfo
applyLhsChangesToNac lhs nacInfo mContext = do
  -- if there are elements that where deleted from lhs, then remove them from the nac too
  let nacInfoD = removeDeletedFromNAC lhs nacInfo
  -- ensure that elements of nacg that are mapped from lhs have the same type as of lhs
  let nacInfoDT = updateNacTypes lhs nacInfoD
  -- ensure that elements of nacg have the same labels of lhs elements
  let ((nacg,nacgi),(nM,eM)) = updateNacLabels lhs nacInfoDT
  -- if has canvas context, then update the graphical informations of nodes
  nacdg <- case mContext of
    Nothing -> return (nacg,nacgi)
    Just context -> do
      nacGiNodes <- updateNodesGiDims (fst nacgi) nacg context
      return (nacg,(nacGiNodes,(snd nacgi)))
  return (nacdg,(nM,eM))

-- if there are elements in the mergeMapping that are not in the LHS,
-- then remove them from the mapping and from the nacgraph
removeDeletedFromNAC :: Graph Info Info -> NacInfo -> NacInfo
removeDeletedFromNAC lhs ((g,gi),(nM,eM)) =
  if (null delNodes && null delEdges)
    then ((g,gi),(nM,eM))
    else ((newG,newGI),(newNM,newEM))
  where
    delNodes = filter (\k -> k `notElem` (nodeIds lhs)) (M.keys nM)
    delEdges = filter (\k -> k `notElem` (edgeIds lhs)) (M.keys eM)

    insertGroup group m = foldr (\k m -> M.insert k elem m) m group
        where elem = maximum group

    lhs' = foldr (\n g -> removeNodeAndIncidentEdges n g) lhs delNodes
    lhs'' = foldr (\e g -> removeEdge e g) lhs' delEdges

    -- change merge mapping, removing deleted elements ids and replacing them if they're the target in the mapping
    -- example: nM = [(1,3),(2,3),(3,3)]; delete node 3 -> newNM = [(1,2),(2,2)]
    nM' = M.filterWithKey (\k a -> k `notElem` delNodes) nM
    nGroups = M.elems $ foldr (addToGroup nM id) M.empty (M.keys nM')
    newNM = foldr insertGroup M.empty nGroups

    eM' = M.filterWithKey (\k a -> k `elem` edgeIds lhs'') eM
    eGroups = M.elems $ foldr (addToGroup eM id) M.empty (M.keys eM')
    newEM = foldr insertGroup M.empty eGroups

    -- functions to update Ids based on the old and the new mergeMapping
    fnm = foldr (\n f -> let n' = fromJust $ M.lookup n nM
                        in (\nid -> if nid == n' then n else f nid))
                (id)
                (removeDuplicates $ M.elems newNM)

    fem = foldr (\e f -> let e' = fromJust $ M.lookup e eM
                        in (\eid -> if eid == e' then e else f eid))
                (id)
                (removeDuplicates $ M.elems newEM)

    -- modify the elements in nacg, changing their ids and updating their labels
    lhsNodes'' = map (\n ->  n { nodeInfo = infoSetLocked (nodeInfo n) True} ) (nodes lhs'')
    lhsEdges'' = map (\e ->  e { edgeInfo = infoSetLocked (edgeInfo e) True} ) (edges lhs'')
    lhsAux = fromNodesAndEdges lhsNodes'' lhsEdges''
    gAux = extractNacGraph lhsAux  (newNM,newEM)

    nodesg' = map (\n -> let nid' = fnm $ nodeId n
                             info = case lookupNode nid' gAux of
                               Nothing -> nodeInfo n
                               Just n' -> infoSetLocked (nodeInfo n') True
                        in Node nid' info) $
                  filter (\n -> let nid = nodeId n
                                 in (fnm nid `elem` (nodeIds lhsAux)) || (nid `notElem` (M.elems nM)) )
                          (nodes g)
    edgesg' = map (\e -> let eid = fem $ edgeId e
                             s = fnm $ sourceId e
                             t = fnm $ targetId e
                             info = case lookupEdge eid gAux of
                               Nothing -> edgeInfo e
                               Just e' -> infoSetLocked (edgeInfo e') True
                        in Edge eid s t info)
                  $ filter (\e -> let eid = edgeId e
                                 in (fem eid `elem` (edgeIds lhsAux)) || (eid `notElem` (M.elems eM)) )
                          (edges g)
    newG = fromNodesAndEdges nodesg' edgesg'

    nodegi' = M.filterWithKey (\k _ -> NodeId k `elem` (nodeIds newG))
                              $ M.mapKeys (\k -> fromEnum . fnm $ NodeId k) (fst gi)
    edgegi' = M.filterWithKey (\k _ -> EdgeId k `elem` (edgeIds newG))
                              $ M.mapKeys (\k -> fromEnum . fem $ EdgeId k) (snd gi)
    newGI = (nodegi',edgegi')

-- Update nac elements types, assuring that it preserve typing according to the corresponding lhs.
updateNacTypes :: Graph Info Info -> NacInfo -> NacInfo
updateNacTypes lhs ((nacg,nacgi),(nM, eM)) =
  if (and $ map (\(id,id') -> haveSameType lookupNode nodeInfo lhs nacg id id') $ M.toList nM) &&
     (and $ map (\(id,id') -> haveSameType lookupEdge edgeInfo lhs nacg id id') $ M.toList eM)
    then ((nacg,nacgi),(nM, eM))
    else ((newNacG,newNacGI),(nM'',eM''))
  where
    -- make sure that the elements of the nacg preserve the type of the elements of lhs
    -- PNT = preserving node typing
    nacgPNT = foldr (\n g -> updateNodePayload (nodeId n) g (\info -> infoSetType info (infoType $ nodeInfo n)))
                    nacg (nodes lhs)
    nacgPT = foldr (\e g -> updateEdgePayload (edgeId e) g (\info -> infoSetType info (infoType $ edgeInfo e)))
                   nacgPNT (edges lhs)

    -- if a element is mapped to a element of different type, remove the pair of the mapping
    haveSameType lookupF getInfo lhs nacg id id' =
      let mt = Just (infoType . getInfo) <*> (lookupF id lhs)
          mt' = Just (infoType . getInfo) <*> (lookupF id' nacg)
          in case (mt,mt') of
            (Just t, Just t') -> t == t'
            _ -> False
    nM' = M.filterWithKey (haveSameType lookupNode nodeInfo lhs nacgPT) nM
    eM' = M.filterWithKey (haveSameType lookupEdge edgeInfo lhs nacgPT) eM

    -- remove elements that appear only one time from edges mapping
    count id m = case M.lookup id m of
                  Nothing -> M.insert id 1 m
                  Just c -> M.insert id (c+1) m
    eMCounting = M.foldr count M.empty eM'
    eM'' = M.filter (\eid -> fromJust (M.lookup eid eMCounting) > 1) eM'

    -- remove elements that appear only one time AND have no incident edge in nacg from nodes mapping
    nMCounting = M.foldr count M.empty nM'
    hasIncidentEdges nid = let  incidents = case lookupNodeInContext nid nacgPT of
                                  Nothing -> []
                                  Just n -> incidentEdges (snd n)
                                incidents' = filter (\(_,e,_) -> not $ infoLocked (edgeInfo e)) incidents
                            in length incidents' > 0
    nM'' = M.filter (\nid -> (fromJust (M.lookup nid nMCounting) > 1) || hasIncidentEdges nid ) nM'

    -- remove from nac graph the edges and vertices removed from the edges mapping
    nacg' = foldr (\e g -> if (edgeId e `elem` eM) && (edgeId e `notElem` eM'')
                            then removeEdge (edgeId e) g
                            else g)
                  nacgPT (edges nacgPT)
    nacg'' = foldr (\n g -> if (nodeId n `elem` nM) && (nodeId n `notElem` nM'')
                              then removeNodeAndIncidentEdges (nodeId n) g
                              else g)
                  nacg' (nodes nacgPT)

    -- create a auxiliar graph with the lhs elements merged
    lhsNodes' = map (\n ->  n { nodeInfo = infoSetLocked (nodeInfo n) True} ) (nodes lhs)
    lhsEdges' = map (\e ->  e { edgeInfo = infoSetLocked (edgeInfo e) True} ) (edges lhs)
    lhsAux = fromNodesAndEdges lhsNodes' lhsEdges'
    gAux = extractNacGraph lhsAux  (nM'',eM'')

    -- update nac graph' elements information with the auxiliar graph
    nacnodes'' = map (\n -> case lookupNode (nodeId n) gAux of
                                  Nothing -> n
                                  Just n' -> n')
                      (nodes nacg'')
    nacedges'' = map (\e -> case lookupEdge (edgeId e) gAux of
                                  Nothing -> e
                                  Just e' -> e')
                      (edges nacg'')
    newNacG = fromNodesAndEdges nacnodes'' nacedges''

    -- remove deleted elements from gi
    newNacNGI = M.filterWithKey (\id _ -> NodeId id `elem` (nodeIds newNacG)) (fst nacgi)
    newNacEGI = M.filterWithKey (\id _ -> EdgeId id `elem` (edgeIds newNacG)) (snd nacgi)
    newNacGI = (newNacNGI,newNacEGI)


updateNacLabels :: Graph Info Info -> NacInfo -> NacInfo
updateNacLabels lhs ((nacg, nacgi), (nM,eM)) = ((nacg'',nacgi),(nM,eM))
  where
    changeInfoLabel id' lbl' i =
      case infoLabel i of
        Label _ -> infoSetLabel i lbl'
        LabelGroup lbls -> 
          let lbls' = map (\(id,lbl) -> if id == id' then (id,lbl') else (id,lbl)) lbls
          in i {infoLabel = LabelGroup lbls'}

    nodesWithMapping = foldr (\n l -> case M.lookup (nodeId n) nM of
                                        Nothing -> l
                                        Just id -> (n,id):l) [] (nodes lhs)
    edgesWithMapping = foldr (\e l -> case M.lookup (edgeId e) eM of
                                        Nothing -> l
                                        Just id -> (e,id):l) [] (edges lhs)

    nacg' = foldr (\(n,id) g -> let id' = if nodeId n == id then 0 else fromEnum $ nodeId n
                                in updateNodePayload  id
                                                      g
                                                      (changeInfoLabel id' (infoLabelStr $ nodeInfo n)))
                  nacg 
                  nodesWithMapping
    nacg'' = foldr (\(e,id) g -> let id' = if edgeId e == id then 0 else fromEnum $ edgeId e 
                                 in updateEdgePayload id
                                                      g
                                                      (changeInfoLabel id' (infoLabelStr $ edgeInfo e)))
                  nacg'
                  edgesWithMapping