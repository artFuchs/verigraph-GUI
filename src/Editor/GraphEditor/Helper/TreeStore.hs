{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

-- | This module contains the definition of GraphStore
-- and auxiliar functions to manipulate the treeStore associated with the TreeView in the main window

module Editor.GraphEditor.Helper.TreeStore(
  GraphStore
, ChangeStack
, initStore
, storeSetGraphStore
, getTreeStoreValues
, getStructsToSave
, getParentDiaGraph
, getNacList
, getRuleList
, getRules
, updateNacs
, updateRuleNacs
, updateAllNacs
, applyLhsChangesToNac
, updateNacTypes
)where

import qualified GI.Gtk as Gtk
import qualified GI.Pango as P
import Data.GI.Base
import Data.GI.Base.GValue

import Control.Monad
import Data.IORef
import Data.Maybe
import Data.Int
import qualified Data.Map as M
import qualified Data.Tree as Tree
import qualified Data.Text as T


import Data.Graphs hiding (empty, null)
import qualified Data.Graphs as G

import Editor.Data.Info1
import Editor.Data.Nac
import Editor.Data.DiaGraph hiding (empty)
import qualified Editor.Data.DiaGraph as DG
import Editor.Data.GraphicalInfo
import Editor.Data.EditorState
import Editor.GraphEditor.Helper.GrammarMaker
import Editor.GraphEditor.Helper.GraphicalInfo
import Editor.GraphEditor.Helper.SaveLoad
import Editor.Helper.List

{- |GraphStore
 A tuple representing what is showed in each node of the tree in the treeview
 It contains the informations:
 * name,
 * graph changed (0 - no, 1 - yes),
 * graph id,
 * type (0 - topic, 1 - typeGraph, 2 - hostGraph, 3 - ruleGraph, 4 - NAC) and
 * active (valid for rules only)
 * valid (if the current graph is correctly mapped to the typegraph)
-}
type GraphStore = (String, Int32, Int32, Int32, Bool, Bool)
type NAC = (Graph Info Info, (MergeMapping))
type ChangeStack = [(DiaGraph,Maybe MergeMapping)]


-- Gtk.treeStore manipulation
initStore :: Gtk.TreeStore -> Gtk.TreeView ->  IO ()
initStore store treeview = do
  fstIter <- Gtk.treeStoreAppend store Nothing
  storeSetGraphStore store fstIter ("TypeGraph", 0, 0, 1, False, True)
  sndIter <- Gtk.treeStoreAppend store Nothing
  storeSetGraphStore store sndIter ("InitialGraph", 0, 1, 2, False, True)
  rulesIter <- Gtk.treeStoreAppend store Nothing
  storeSetGraphStore store rulesIter ("Rules", 0, 0, 0, False, True)
  fstRuleIter <- Gtk.treeStoreAppend store (Just rulesIter)
  storeSetGraphStore store fstRuleIter ("Rule0", 0, 2, 3, True, True)
  path <- Gtk.treeModelGetPath store fstIter
  Gtk.treeViewSetCursor treeview path (Nothing :: Maybe Gtk.TreeViewColumn) False
  rulesPath <- Gtk.treeModelGetPath store rulesIter
  Gtk.treeViewExpandRow treeview rulesPath True
  return ()

storeSetGraphStore :: Gtk.TreeStore -> Gtk.TreeIter -> GraphStore -> IO ()
storeSetGraphStore store iter (n,c,i,t,a,v) = do
  gv0 <- toGValue (Just n)
  gv1 <- toGValue c
  gv2 <- toGValue i
  gv3 <- toGValue t
  gv4 <- toGValue a
  gv5 <- toGValue v
  #set store iter [0,1,2,3,4,5] [gv0,gv1,gv2,gv3,gv4,gv5]

getTreeStoreValues :: Gtk.TreeStore -> Gtk.TreeIter -> IO (Tree.Forest (Int32,(String,Int32,Bool)))
getTreeStoreValues store iter = do
  valT <- Gtk.treeModelGetValue store iter 3 >>= fromGValue :: IO Int32
  valN <- Gtk.treeModelGetValue store iter 0 >>= (\n -> fromGValue n :: IO (Maybe String)) >>= return . fromJust
  valI <- Gtk.treeModelGetValue store iter 2 >>= fromGValue :: IO Int32
  valA <- Gtk.treeModelGetValue store iter 4 >>= fromGValue :: IO Bool
  (valid, childIter) <- Gtk.treeModelIterChildren store (Just iter)
  subForest <- case valid of
                True -> getTreeStoreValues store childIter
                False -> return []
  continue <- Gtk.treeModelIterNext store iter
  if continue
    then do
      newVals <- getTreeStoreValues store iter
      return $ (Tree.Node (valT, (valN, valI, valA)) subForest) : newVals
    else return $ (Tree.Node (valT, (valN, valI, valA)) subForest) : []

getStructsToSave :: Gtk.TreeStore -> IORef (M.Map Int32 (EditorState, ChangeStack, ChangeStack))-> IORef (M.Map Int32 (DiaGraph,MergeMapping))-> IO (Tree.Forest SaveInfo)
getStructsToSave store graphStates nacInfoMapIORef = do
  (valid, fstIter) <- Gtk.treeModelGetIterFirst store
  if not valid
    then return []
    else do
      treeNodeList <- getTreeStoreValues store fstIter
      states <- readIORef graphStates
      nacInfoMap <- readIORef nacInfoMapIORef
      let structs = map
                    (fmap (\(t, (name, nid, active)) -> case t of
                                0 -> Topic name
                                1 -> let (es,_,_) = fromJust $ M.lookup nid states
                                     in TypeGraph name es
                                2 -> let (es,_,_) = fromJust $ M.lookup nid states
                                     in HostGraph name es
                                3 -> let (es,_,_) = fromJust $ M.lookup nid states
                                     in RuleGraph name es active
                                4 -> let (nacdg,mapping) = fromJust $ M.lookup nid nacInfoMap
                                     in NacGraph name (nacdg,mapping)
                    )) treeNodeList
      return structs

-- auxiliar function to load parent graph
getParentDiaGraph :: Gtk.TreeStore -> [Int32] -> IORef (M.Map Int32 (EditorState, ChangeStack, ChangeStack)) -> IO DiaGraph
getParentDiaGraph store pathIndices graphStates = do
  path <- Gtk.treePathNewFromIndices pathIndices
  (validIter, iter) <- Gtk.treeModelGetIter store path
  (valid, parent) <- if validIter
                        then Gtk.treeModelIterParent store iter
                        else return (False,iter)
  if valid
    then do
      index <- Gtk.treeModelGetValue store parent 2 >>= fromGValue :: IO Int32
      states <- readIORef graphStates
      state <- return $ M.lookup index states
      case state of
        Nothing -> return DG.empty
        Just (es,_,_) -> return (lhs, (ngi,egi))
                    where (lhs,_,_) = graphToRuleGraphs $ editorGetGraph es
                          ngi = M.filterWithKey (\k a -> (NodeId k) `elem` (nodeIds lhs)) $ fst (editorGetGI es)
                          egi = M.filterWithKey (\k a -> (EdgeId k) `elem` (edgeIds lhs)) $ snd (editorGetGI es)
    else return DG.empty

-- given a treeStore, a treeIter and a map of nacInfos, returns a list of NACs if the iter given corresponds to a nacGraph
getNacList :: Gtk.TreeStore
           -> Gtk.TreeIter
           -> M.Map Int32 (DiaGraph, MergeMapping)
           -> Graph Info Info
           -> IO [NAC]
getNacList model iter nacInfoMap lhs = do
  index <- Gtk.treeModelGetValue model iter 2 >>= fromGValue :: IO Int32
  ans <- case M.lookup index nacInfoMap of
    Nothing -> return []
    Just nInfo -> do
      (diag,(nM,eM)) <- applyLhsChangesToNac lhs nInfo Nothing
      return [(fst diag, (nM,eM))]
  continue <- Gtk.treeModelIterNext model iter
  if continue
    then do
      rest <- getNacList model iter nacInfoMap lhs
      return $ ans ++ rest
    else return ans

getRuleList :: Gtk.TreeStore
            ->  Gtk.TreeIter
            -> M.Map Int32 (EditorState, ChangeStack, ChangeStack)
            -> IORef (M.Map Int32 (DiaGraph, MergeMapping))
            -> IO [(Graph Info Info, [NAC], String)]
getRuleList model iter gStates nacInfoMapIORef = do
  name <- Gtk.treeModelGetValue model iter 0 >>= fromGValue >>= return . fromJust :: IO String
  index <- Gtk.treeModelGetValue model iter 2 >>= fromGValue :: IO Int32
  active <- Gtk.treeModelGetValue model iter 4 >>= fromGValue :: IO Bool
  ans <- case (active, M.lookup index gStates) of
    (False, _) -> return []
    (True, Nothing) -> return []
    (True, Just (es, _, _)) -> do
      (hasNac,nacIter) <- Gtk.treeModelIterChildren model (Just iter)
      let rule = editorGetGraph es
      nacs <- case hasNac of
        True -> do
          let (lhs,_,_) = graphToRuleGraphs rule
          nacInfoMap <- readIORef nacInfoMapIORef
          getNacList model nacIter nacInfoMap lhs
        False -> return []
      return $ [(rule, nacs, name)]
  continue <- Gtk.treeModelIterNext model iter
  if continue
    then do
      rest <- getRuleList model iter gStates nacInfoMapIORef
      return $ ans ++ rest
    else return ans

getRules :: Gtk.TreeStore
         -> IORef (M.Map Int32 (EditorState, ChangeStack, ChangeStack))
         -> IORef (M.Map Int32 (DiaGraph, MergeMapping))
         -> IO [(Graph Info Info, [NAC], String)]
getRules model gStatesIORef nacInfoMapIORef = do
  (valid, iter) <- Gtk.treeModelGetIterFromString model "2:0"
  if not valid
    then return []
    else do
      gStates <- readIORef gStatesIORef
      getRuleList model iter gStates nacInfoMapIORef

updateNacs :: Gtk.TreeStore -> Gtk.TreeIter -> Graph Info Info -> IORef (M.Map Int32 NacInfo) -> P.Context -> IO ()
updateNacs store iter lhs nacInfoMapIORef context = do
  nacInfoMap <- readIORef nacInfoMapIORef
  index <- Gtk.treeModelGetValue store iter 2 >>= Gtk.fromGValue :: IO Int32
  case M.lookup index nacInfoMap of
    Nothing -> return ()
    Just nacInfo -> do
      nacInfo' <- applyLhsChangesToNac lhs nacInfo (Just context)
      modifyIORef nacInfoMapIORef $ M.insert index nacInfo'
      continue <- Gtk.treeModelIterNext store iter
      if continue
        then do updateNacs store iter lhs nacInfoMapIORef context
        else return ()

updateRuleNacs :: Gtk.TreeStore
           -> Gtk.TreeIter
           -> IORef (M.Map Int32 (EditorState, ChangeStack, ChangeStack))
           -> IORef (M.Map Int32 NacInfo)
           -> P.Context
           -> IO ()
updateRuleNacs store iter graphStatesIORef nacInfoMapIORef context = do
  (hasNacs,nacIter) <- Gtk.treeModelIterChildren store (Just iter)
  if not hasNacs
    then return ()
    else do
      ruleChanged <- Gtk.treeModelGetValue store iter 1 >>= Gtk.fromGValue :: IO Int32
      if ruleChanged == 0
        then return ()
        else do
          ruleIndex <- Gtk.treeModelGetValue store iter 2 >>= Gtk.fromGValue :: IO Int32
          states <- readIORef graphStatesIORef
          let (error,rule) = case M.lookup ruleIndex states of
                  Nothing -> (True,G.empty)
                  Just (es,_,_) -> (False, editorGetGraph es)
              (lhs,_,_) = graphToRuleGraphs rule
          if error
            then return ()
            else updateNacs store nacIter lhs nacInfoMapIORef context
  continue <- Gtk.treeModelIterNext store iter
  if continue
    then updateRuleNacs store iter graphStatesIORef nacInfoMapIORef context
    else return ()

updateAllNacs :: Gtk.TreeStore
              -> IORef (M.Map Int32 (EditorState, ChangeStack, ChangeStack))
              -> IORef (M.Map Int32 NacInfo)
              -> P.Context
              -> IO ()
updateAllNacs store graphStatesIORef nacInfoMapIORef context = do
  (valid, iter) <- Gtk.treeModelGetIterFromString store "2:0"
  if not valid
    then return ()
    else updateRuleNacs store iter graphStatesIORef nacInfoMapIORef context

-- update the nacGraph according to the lhs
applyLhsChangesToNac :: Graph Info Info -> NacInfo -> Maybe P.Context -> IO NacInfo
applyLhsChangesToNac lhs nacInfo mContext = do
  -- if there are elements that where deleted from lhs, then remove them from the nac too
  let nacInfoD = removeDeletedFromNAC lhs nacInfo
  -- ensure that elements of nacg that are mapped from lhs have the same type as of lhs
  let ((nacg,nacgi),(nM,eM)) = updateNacTypes lhs nacInfoD
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

-- | Update nac elements types, assuring that it preserve typing according to the corresponding lhs.
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
