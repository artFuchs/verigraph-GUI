{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

{- | Definition of GraphStore and auxiliar functions
     to manipulate the TreeStore associated with the TreeView of the Editor
-}
module GUI.Editor.Helper.TreeStore(
  GraphStore
, initStore
, initTreeView
, storeSetGraphStore
, getTreeStoreValues
, getStructsToSave
, getParentLHSDiaGraph
, getRules
, getRuleNacs
, indicateProjChanged
, indicateGraphChanged
, setChangeFlags
, setValidFlags
, setCurrentValidFlag
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

import GUI.Data.Info
import GUI.Data.Nac
import GUI.Data.DiaGraph hiding (empty)
import qualified GUI.Data.DiaGraph as DG
import GUI.Data.GraphicalInfo
import GUI.Data.GraphState
import GUI.Data.SaveInfo
import GUI.Helper.GrammarMaker
import GUI.Helper.GraphValidation
import GUI.Helper.List

{-| GraphStore
 A tuple representing what is showed in each node of the tree in the treeview
 It contains the informations:
 * name,
 * graph changed,
 * graph id,
 * type (0 - topic, 1 - typeGraph, 2 - hostGraph, 3 - ruleGraph, 4 - NAC) and
 * active (valid for rules only)
 * valid (if the current graph is correctly mapped to the typegraph)
-}
type GraphStore = (String, Bool, Int32, Int32, Bool, Bool)


----------------------------------------------------------
-- init and set
----------------------------------------------------------

-- | initializes the treeStore with an empty project setup
initStore :: Gtk.TreeStore ->  IO ()
initStore store = do
  fstIter <- Gtk.treeStoreAppend store Nothing
  storeSetGraphStore store fstIter ("TypeGraph", False, 0, 1, False, True)
  sndIter <- Gtk.treeStoreAppend store Nothing
  storeSetGraphStore store sndIter ("InitialGraph", False, 1, 2, False, True)
  rulesIter <- Gtk.treeStoreAppend store Nothing
  storeSetGraphStore store rulesIter ("Rules", False, 0, 0, False, True)
  fstRuleIter <- Gtk.treeStoreAppend store (Just rulesIter)
  storeSetGraphStore store fstRuleIter ("Rule0", False, 2, 3, True, True)
  return ()

initTreeView :: Gtk.TreeView -> IO ()
initTreeView treeview = do
  path <- Gtk.treePathNewFromIndices [0]
  Gtk.treeViewSetCursor treeview path (Nothing :: Maybe Gtk.TreeViewColumn) False
  rulesPath <- Gtk.treePathNewFromIndices [2]
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

{- |
  Walk in a TreeStore recovering the essential information of the GraphStore stored in each position.
  Must be given the TreeStore and a initial TreeIter to start a recursive walk in the treeStore.
  Returns a Tree.Forest containing (Type,(Name,Id,Active))
-}
getTreeStoreValues :: Gtk.TreeStore
                   -> Gtk.TreeIter
                   -> IO (Tree.Forest (String,Int32,Int32,Bool))
getTreeStoreValues store iter = do
  valN <- Gtk.treeModelGetValue store iter 0 >>= (\n -> fromGValue n :: IO (Maybe String)) >>= return . fromJust
  valI <- Gtk.treeModelGetValue store iter 2 >>= fromGValue :: IO Int32
  valT <- Gtk.treeModelGetValue store iter 3 >>= fromGValue :: IO Int32
  valA <- Gtk.treeModelGetValue store iter 4 >>= fromGValue :: IO Bool
  (valid, childIter) <- Gtk.treeModelIterChildren store (Just iter)
  subForest <- case valid of
                True -> getTreeStoreValues store childIter
                False -> return []
  continue <- Gtk.treeModelIterNext store iter
  if continue
    then do
      newVals <- getTreeStoreValues store iter
      return $ (Tree.Node (valN, valI, valT, valA) subForest) : newVals
    else return $ (Tree.Node (valN, valI, valT, valA) subForest) : []

-- | gets a tree of SaveInfo out of a TreeStore
getStructsToSave :: Gtk.TreeStore
                 -> IORef (M.Map Int32 GraphState)
                 -> IORef (M.Map Int32 MergeMapping)
                 -> IO (Tree.Forest SaveInfo)
getStructsToSave store graphStates nacsMergeMappingsIORef = do
  (valid, fstIter) <- Gtk.treeModelGetIterFirst store
  if not valid
    then return []
    else do
      treeNodeList <- getTreeStoreValues store fstIter
      states <- readIORef graphStates
      nacsMergeMappings <- readIORef nacsMergeMappingsIORef
      let structs = map
                    (fmap (\(name, nid, t, active) -> case t of
                                0 -> Topic name
                                1 -> let es = fromJust $ M.lookup nid states
                                     in TypeGraph nid name es
                                2 -> let es = fromJust $ M.lookup nid states
                                     in HostGraph nid name es
                                3 -> let es = fromJust $ M.lookup nid states
                                     in RuleGraph nid name es active
                                4 -> let mapping = fromJust $ M.lookup nid nacsMergeMappings
                                         es = fromJust $ M.lookup nid states
                                         nacdg = (stateGetGraph es, stateGetGI es)
                                     in NacGraph nid name (nacdg,mapping)
                    )) treeNodeList
      return structs

-- | Get the Diagraph stored in the parent position relative to the given path
getParentLHSDiaGraph :: Gtk.TreeStore
                  -> [Int32]
                  -> IORef (M.Map Int32 GraphState)
                  -> IO DiaGraph
getParentLHSDiaGraph store pathIndices graphStates = do
  (g,gi) <- getParentDiaGraph store pathIndices graphStates
  let (lhs,_,_) = graphToRuleGraphs g
      ngi = M.filterWithKey (\k a -> (NodeId k) `elem` (nodeIds lhs)) $ fst gi
      egi = M.filterWithKey (\k a -> (EdgeId k) `elem` (edgeIds lhs)) $ snd gi
  return (lhs,(ngi,egi))

getParentDiaGraph :: Gtk.TreeStore
                  -> [Int32]
                  -> IORef (M.Map Int32 GraphState)
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
        Just es -> return (stateGetGraph es, stateGetGI es)
    else return DG.empty



type NAC = (Graph Info Info, (MergeMapping))

-- Returns a list of NAC from a TreeStore.
-- The iter must be set to the first GraphStore containing a Nac
getNacList :: Gtk.TreeStore
           -> Gtk.TreeIter
           -> M.Map Int32 GraphState
           -> M.Map Int32 MergeMapping
           -> Graph Info Info
           -> IO [NAC]
getNacList store iter gStates nacsMergeMappings lhs = do
  index <- Gtk.treeModelGetValue store iter 2 >>= fromGValue :: IO Int32
  ans <- case (M.lookup index gStates, M.lookup index nacsMergeMappings) of
    (Just st, Just m) -> return [(stateGetGraph st,m)]
    _ -> return []
  continue <- Gtk.treeModelIterNext store iter
  if continue
    then do
      rest <- getNacList store iter gStates nacsMergeMappings lhs
      return $ ans ++ rest
    else return ans


-- Returns a list of graphs relative to rules, along with their NACs and names, from the TreeStore.
-- The iter points to the first rule from the list
getRuleList :: Gtk.TreeStore
            -> Gtk.TreeIter
            -> M.Map Int32 GraphState
            -> IORef (M.Map Int32 MergeMapping)
            -> IO [(Graph Info Info, [NAC], String)]
getRuleList store iter gStates nacsMergeMappingsIORef = do
  name <- Gtk.treeModelGetValue store iter 0 >>= fromGValue >>= return . fromJust :: IO String
  index <- Gtk.treeModelGetValue store iter 2 >>= fromGValue :: IO Int32
  active <- Gtk.treeModelGetValue store iter 4 >>= fromGValue :: IO Bool
  ans <- case (active, M.lookup index gStates) of
    (False, _) -> return []
    (True, Nothing) -> return []
    (True, Just es) -> do
      (hasNac,nacIter) <- Gtk.treeModelIterChildren store (Just iter)
      let rule = stateGetGraph es
      nacs <- case hasNac of
        True -> do
          let (lhs,_,_) = graphToRuleGraphs rule
          nacsMergeMappings <- readIORef nacsMergeMappingsIORef
          getNacList store nacIter gStates nacsMergeMappings lhs
        False -> return []
      return $ [(rule, nacs, name)]
  continue <- Gtk.treeModelIterNext store iter
  if continue
    then do
      rest <- getRuleList store iter gStates nacsMergeMappingsIORef
      return $ ans ++ rest
    else return ans

-- | Returns the rules stored in a TreeStore
getRules :: Gtk.TreeStore
         -> IORef (M.Map Int32 GraphState)
         -> IORef (M.Map Int32 MergeMapping)
         -> IO [(Graph Info Info, [NAC], String)]
getRules store gStatesIORef nacMergeMappingsIORef = do
  (valid, iter) <- Gtk.treeModelGetIterFromString store "2:0"
  if not valid
    then return []
    else do
      gStates <- readIORef gStatesIORef
      getRuleList store iter gStates nacMergeMappingsIORef




-- given a nacIter, return a list of nacs
getNacs :: Gtk.TreeStore
        -> Gtk.TreeIter
        -> M.Map Int32 GraphState
        -> M.Map Int32 MergeMapping
        -> IO [(Int32,NacInfo)]
getNacs store iter statesMap mergeMappings = do
  index <- Gtk.treeModelGetValue store iter 2 >>= fromGValue :: IO Int32
  case M.lookup index statesMap of
    Just st -> do
      let g = stateGetGraph st
          gi = stateGetGI st
          mergeM = fromMaybe (M.empty,M.empty) $ M.lookup index mergeMappings
          nacInfo = ((g,gi),mergeM)
      continue <- Gtk.treeModelIterNext store iter
      if continue then do
        rest <- getNacs store iter statesMap mergeMappings
        return $ (index,nacInfo):rest
      else
        return [(index,nacInfo)]
    Nothing -> return []


-- given a path to a rule, gets all the NACs of the rule
getRuleNacs :: Gtk.TreeStore
        -> IORef [Int32]
        -> IORef (M.Map Int32 GraphState)
        -> IORef (M.Map Int32 MergeMapping)
        -> IO [(Int32,NacInfo)]
getRuleNacs store pathIndicesIORef gStatesIORef nacsMergeMappings = do
  rulePathIndices <- readIORef pathIndicesIORef
  rulePath <- Gtk.treePathNewFromIndices rulePathIndices
  (valid,iter) <- Gtk.treeModelGetIter store rulePath
  if valid then do
    (hasNac,nacIter) <- Gtk.treeModelIterChildren store (Just iter)
    if hasNac then do
      statesMap <- readIORef gStatesIORef
      mergeMappings <- readIORef nacsMergeMappings
      getNacs store nacIter statesMap mergeMappings
    else
      return []
  else
    return []


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
indicateGraphChanged store iter changed = do
  gtype <- Gtk.treeModelGetValue store iter 3 >>= fromGValue :: IO Int32
  if gtype == 0
    then return ()
    else do
      gvchanged <- toGValue changed
      Gtk.treeStoreSetValue store iter 1 gvchanged
      (parentValid, parentIter) <- Gtk.treeModelIterParent store iter
      case (parentValid,changed) of
        (True,True) -> Gtk.treeStoreSetValue store parentIter 1 gvchanged
        (True,False) -> do
            let brothersChanged iter = do
                    brotherChanged <- Gtk.treeModelGetValue store iter 1 >>= fromGValue :: IO Bool
                    continue <- Gtk.treeModelIterNext store iter
                    case (brotherChanged, continue) of
                        (True,_) -> return True
                        (False,True) -> brothersChanged iter
                        (False,False) -> return False
            parentChangeVal <- brothersChanged iter
            gvparentchanged <- toGValue parentChangeVal
            Gtk.treeStoreSetValue store parentIter 1 gvparentchanged
        (False,_) -> return ()

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
             -> M.Map Int32 GraphState
             -> Graph Info Info
             -> IO ()
setValidFlag store iter states tg = do
  index <- Gtk.treeModelGetValue store iter 2 >>= fromGValue :: IO Int32
  mst <- return $ M.lookup index states
  g <- case mst of
    Nothing -> return G.empty
    Just es -> return $ stateGetGraph es
  let valid = isGraphValid g tg
  gvValid <- toGValue valid
  Gtk.treeStoreSetValue store iter 5 gvValid

-- | walk in the treeStore, applying setValidFlag for all the hostGraphs and RuleGraphs
-- should be called when occur an update to the typeGraph
setValidFlags :: Gtk.TreeStore
               -> Graph Info Info
               -> M.Map Int32 GraphState
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
                    -> IORef GraphState
                    -> IORef (Graph Info Info)
                    -> IORef [Int32]
                    -> IO ()
setCurrentValidFlag store st typeGraph currentPath = do
      es <- readIORef st
      tg <- readIORef typeGraph
      let valid = isGraphValid (stateGetGraph es) tg
      gvValid <- toGValue valid
      cpath <- readIORef currentPath >>= Gtk.treePathNewFromIndices
      (validIter, iter) <- Gtk.treeModelGetIter store cpath
      if validIter
        then Gtk.treeStoreSetValue store iter 5 gvValid
        else return ()
