{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

-- | This module contains the definition of GraphStore
-- and auxiliar functions to manipulate the treeStore associated with the TreeView in the main window

module Editor.GraphEditor.Helper.TreeStore(
  GraphStore
, initStore
, storeSetGraphStore
, getTreeStoreValues
, getStructsToSave
, getParentDiaGraph
, getRules
, indicateProjChanged
, indicateGraphChanged
, setChangeFlags
, setValidFlags
, setCurrentValidFlag
, updateAllNacs
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

import Editor.Data.Info
import Editor.Data.Nac
import Editor.Data.DiaGraph hiding (empty)
import qualified Editor.Data.DiaGraph as DG
import Editor.Data.GraphicalInfo
import Editor.Data.EditorState
import Editor.GraphEditor.Helper.GrammarMaker
import Editor.GraphEditor.Helper.GraphicalInfo
import Editor.GraphEditor.Helper.Nac
import Editor.GraphEditor.Helper.SaveLoad
import Editor.Helper.GraphValidation
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

----------------------------------------------------------
-- init and set
----------------------------------------------------------

-- | initializes the treeStore with an empty project setup
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

-- | set the GraphStore in a position given by an iter in the TreeStore
storeSetGraphStore :: Gtk.TreeStore -> Gtk.TreeIter -> GraphStore -> IO ()
storeSetGraphStore store iter (n,c,i,t,a,v) = do
  gv0 <- toGValue (Just n)
  gv1 <- toGValue c
  gv2 <- toGValue i
  gv3 <- toGValue t
  gv4 <- toGValue a
  gv5 <- toGValue v
  #set store iter [0,1,2,3,4,5] [gv0,gv1,gv2,gv3,gv4,gv5]

----------------------------------------------------------
-- gets
----------------------------------------------------------

-- | walk in a TreeStore recovering the essential information of the GraphStore stored in each position
-- must be given the TreeStore and a initial TreeIter to start a recursive walk in the treeStore.
getTreeStoreValues :: Gtk.TreeStore 
                   -> Gtk.TreeIter 
                   -> IO (Tree.Forest (Int32,(String,Int32,Bool)))
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

-- | gets a tree of SaveInfo out of a TreeStore
getStructsToSave :: Gtk.TreeStore 
                 -> IORef (M.Map Int32 (EditorState, a, a))
                 -> IORef (M.Map Int32 (DiaGraph,MergeMapping))
                 -> IO (Tree.Forest SaveInfo)
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

-- | Get the Diagraph stored in the parent position relative to the given path
getParentDiaGraph :: Gtk.TreeStore 
                  -> [Int32] 
                  -> IORef (M.Map Int32 (EditorState, a, a)) 
                  -> IO DiaGraph
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

-- Returns a list of NAC from a TreeStore.
-- The iter must be set to the first GraphStore containing a Nac
getNacList :: Gtk.TreeStore
           -> Gtk.TreeIter
           -> M.Map Int32 (DiaGraph, MergeMapping)
           -> Graph Info Info
           -> IO [NAC]
getNacList store iter nacInfoMap lhs = do
  index <- Gtk.treeModelGetValue store iter 2 >>= fromGValue :: IO Int32
  ans <- case M.lookup index nacInfoMap of
    Nothing -> return []
    Just nInfo -> do
      (diag,(nM,eM)) <- applyLhsChangesToNac lhs nInfo Nothing
      return [(fst diag, (nM,eM))]
  continue <- Gtk.treeModelIterNext store iter
  if continue
    then do
      rest <- getNacList store iter nacInfoMap lhs
      return $ ans ++ rest
    else return ans


-- Returns a list of graphs relative to rules, along with their NACs and names, from the TreeStore.
-- The iter points to the first rule from the list
getRuleList :: Gtk.TreeStore
            ->  Gtk.TreeIter
            -> M.Map Int32 (EditorState, a, a)
            -> IORef (M.Map Int32 (DiaGraph, MergeMapping))
            -> IO [(Graph Info Info, [NAC], String)]
getRuleList store iter gStates nacInfoMapIORef = do
  name <- Gtk.treeModelGetValue store iter 0 >>= fromGValue >>= return . fromJust :: IO String
  index <- Gtk.treeModelGetValue store iter 2 >>= fromGValue :: IO Int32
  active <- Gtk.treeModelGetValue store iter 4 >>= fromGValue :: IO Bool
  ans <- case (active, M.lookup index gStates) of
    (False, _) -> return []
    (True, Nothing) -> return []
    (True, Just (es, _, _)) -> do
      (hasNac,nacIter) <- Gtk.treeModelIterChildren store (Just iter)
      let rule = editorGetGraph es
      nacs <- case hasNac of
        True -> do
          let (lhs,_,_) = graphToRuleGraphs rule
          nacInfoMap <- readIORef nacInfoMapIORef
          getNacList store nacIter nacInfoMap lhs
        False -> return []
      return $ [(rule, nacs, name)]
  continue <- Gtk.treeModelIterNext store iter
  if continue
    then do
      rest <- getRuleList store iter gStates nacInfoMapIORef
      return $ ans ++ rest
    else return ans

-- | Returns the rules stored in a TreeStore.
getRules :: Gtk.TreeStore
         -> IORef (M.Map Int32 (EditorState, a, a))
         -> IORef (M.Map Int32 (DiaGraph, MergeMapping))
         -> IO [(Graph Info Info, [NAC], String)]
getRules store gStatesIORef nacInfoMapIORef = do
  (valid, iter) <- Gtk.treeModelGetIterFromString store "2:0"
  if not valid
    then return []
    else do
      gStates <- readIORef gStatesIORef
      getRuleList store iter gStates nacInfoMapIORef

----------------------------------------------------------
-- flags
----------------------------------------------------------

-- | change window name to indicate if the project was modified
indicateProjChanged :: Gtk.Window -> Bool -> IO ()
indicateProjChanged window True = do
  ttitle <- get window #title
  let title = T.unpack . fromJust $ ttitle
  if title!!0 == '*'
    then return ()
    else set window [#title := T.pack('*':title)]

indicateProjChanged window False = do
  ttitle <- get window #title
  let i:title = T.unpack . fromJust $ ttitle
  if i == '*'
    then set window [#title := T.pack title]
    else return ()

-- | write in the treestore that the current graph was modified
indicateGraphChanged :: Gtk.TreeStore -> Gtk.TreeIter -> Bool -> IO ()
indicateGraphChanged store iter True = do
  gtype <- Gtk.treeModelGetValue store iter 3 >>= fromGValue :: IO Int32
  if gtype == 0
    then return ()
    else do
      gvchanged <- toGValue (1::Int32)
      Gtk.treeStoreSetValue store iter 1 gvchanged
      (valid, parentIter) <- Gtk.treeModelIterParent store iter
      if valid
        then Gtk.treeStoreSetValue store parentIter 1 gvchanged
        else return ()

indicateGraphChanged store iter False = do
  gvchanged <- toGValue (0::Int32)
  Gtk.treeStoreSetValue store iter 1 gvchanged
  (valid, parentIter) <- Gtk.treeModelIterParent store iter
  if valid
    then Gtk.treeStoreSetValue store parentIter 1 gvchanged
    else return ()

-- | change the flags that inform if the graphs and project were changed and indicate the changes
setChangeFlags :: Gtk.Window -> Gtk.TreeStore 
               -> IORef Bool -> IORef [Bool] -> IORef [Int32] -> IORef Int32 
               -> Bool 
               -> IO ()
setChangeFlags window store changedProject changedGraph currentPath currentGraph changed = do
  index <- readIORef currentGraph >>= return . fromIntegral
  xs <- readIORef changedGraph
  let xs' = take index xs ++ [changed] ++ drop (index+1) xs
      projChanged = or xs'
  writeIORef changedGraph xs'
  writeIORef changedProject $ projChanged
  indicateProjChanged window $ projChanged
  path <- readIORef currentPath >>= Gtk.treePathNewFromIndices
  (valid,iter) <- Gtk.treeModelGetIter store path
  if valid
    then indicateGraphChanged store iter changed
    else return ()

-- Analise a graph and change the flags that inform if a graph is valid/invalid
setValidFlag :: Gtk.TreeStore -> Gtk.TreeIter 
             -> M.Map Int32 (EditorState, a, a) 
             -> Graph Info Info 
             -> IO ()
setValidFlag store iter states tg = do
  index <- Gtk.treeModelGetValue store iter 2 >>= fromGValue :: IO Int32
  mst <- return $ M.lookup index states
  g <- case mst of
    Nothing -> return G.empty
    Just (es, u, r) -> return $ editorGetGraph es
  let valid = isGraphValid g tg
  gvValid <- toGValue valid
  Gtk.treeStoreSetValue store iter 5 gvValid

-- | walk in the treeStore, applying setValidFlag for all the hostGraphs and RuleGraphs
-- should be called when occur an update to the typeGraph
setValidFlags :: Gtk.TreeStore 
               -> Graph Info Info 
               -> M.Map Int32 (EditorState, a, a) 
               -> IO ()
setValidFlags store tg states = do
  Gtk.treeModelForeach store $ \model path iter -> do
    t <- Gtk.treeModelGetValue store iter 3 >>= fromGValue :: IO Int32
    case t of
      0 -> return ()
      1 -> return ()
      2 -> setValidFlag store iter states tg
      3 -> setValidFlag store iter states tg
      4 -> setValidFlag store iter states tg
    return False

-- Change the valid flag for the graph that is being edited
-- Needed because the graphStates IORef is not updated while the user is editing the graph
setCurrentValidFlag :: Gtk.TreeStore 
                    -> IORef EditorState 
                    -> IORef (Graph Info Info) 
                    -> IORef [Int32] 
                    -> IO ()
setCurrentValidFlag store st typeGraph currentPath = do
      es <- readIORef st
      tg <- readIORef typeGraph
      let valid = isGraphValid (editorGetGraph es) tg
      gvValid <- toGValue valid
      cpath <- readIORef currentPath >>= Gtk.treePathNewFromIndices
      (validIter, iter) <- Gtk.treeModelGetIter store cpath
      if validIter
        then Gtk.treeStoreSetValue store iter 5 gvValid
        else return ()



----------------------------------------------------------
-- update Nacs
----------------------------------------------------------

-- updating a list of nacs in the TreeStore, starting from the first nac
updateNacs :: Gtk.TreeStore 
           -> Gtk.TreeIter 
           -> Graph Info Info 
           -> IORef (M.Map Int32 NacInfo) 
           -> P.Context 
           -> IO ()
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

-- update the nacs of each rule stored in TreeStore.
-- if the GraphStore relative to the rule is not marked as changed, then just pass to the next rule.
updateRuleNacs :: Gtk.TreeStore
               -> Gtk.TreeIter
               -> IORef (M.Map Int32 (EditorState, a, a))
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

-- | apply applyLhsChangesToNac to the nacs of all rules in the given TreeStore
updateAllNacs :: Gtk.TreeStore
              -> IORef (M.Map Int32 (EditorState, a, a))
              -> IORef (M.Map Int32 NacInfo)
              -> P.Context
              -> IO ()
updateAllNacs store graphStatesIORef nacInfoMapIORef context = do
  (valid, iter) <- Gtk.treeModelGetIterFromString store "2:0"
  if not valid
    then return ()
    else updateRuleNacs store iter graphStatesIORef nacInfoMapIORef context
