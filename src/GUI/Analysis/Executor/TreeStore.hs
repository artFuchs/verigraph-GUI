{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

module GUI.Analysis.Executor.TreeStore (
  ExecGraphEntry
, storeSetGraphEntry
, getRuleIter
, updateTreeStore
, removeFromTreeStore
, removeTrashFromTreeStore
, treeStoreGetRules
, removeMatchesFromTreeStore
, treeStoreClearCurrrentLevel
, getRuleAndMatchIndex
) where

import qualified GI.Gtk                   as Gtk
import           Data.Int
import           Data.IORef
import qualified Data.Map                   as M
import           Data.GI.Base
import           Control.Monad

import           GUI.Data.GraphState



{-| ExecGraphEntry
    A tuple representing what is showed in each node of the tree in the treeview
    It contains the informations:
    * name, - what is displayed, can have aditional information
    * id (case type is 2) or offset (case type is 4 or 5)
    * type (0 - topic, 1 - rule, 2 - rule match, 4 - next, 5 - previous),
    * parent id (used if type is >= 2),
    * real name, - used for keeping the rule names and then getting it after
-}
type ExecGraphEntry = (String, Int32, Int32, Int32, String)

-- | set the ExecGraphStore in a position given by an iter in the TreeStore
storeSetGraphEntry :: Gtk.TreeStore -> Gtk.TreeIter -> ExecGraphEntry -> IO ()
storeSetGraphEntry store iter (n,i,t,p,rn) = do
    gvn <- toGValue (Just n)
    gvi <- toGValue i
    gvt <- toGValue t
    gvp <- toGValue p
    gvrn <- toGValue (Just rn)
    #set store iter [0,1,2,3,4] [gvn,gvi,gvt,gvp,gvrn]

getFirstRuleIter :: Gtk.TreeStore -> IO (Maybe Gtk.TreeIter)
getFirstRuleIter store = do
    (valid,rootIter) <- Gtk.treeModelGetIterFirst store
    if valid
        then do
            (valid,childIter) <- Gtk.treeModelIterChildren store (Just rootIter)
            if valid
                then return $ Just childIter
                else return Nothing
        else do
            rootIter <- Gtk.treeStoreAppend store Nothing
            storeSetGraphEntry store rootIter ("Grammar", (-1), 0, (-1), "Grammar")
            return Nothing

getRuleIter :: Gtk.TreeStore -> Int32 -> IO (Maybe Gtk.TreeIter)
getRuleIter store rid = do
    mIter <- getFirstRuleIter store
    case mIter of
        Nothing -> return Nothing
        Just iter -> getRuleIter' store rid iter


getRuleIter' :: Gtk.TreeStore -> Int32 -> Gtk.TreeIter -> IO (Maybe Gtk.TreeIter)
getRuleIter' store rid iter = do
    id <- Gtk.treeModelGetValue store iter 1 >>= fromGValue :: IO Int32
    if id == rid
        then return $ Just iter
        else do
            continue <- Gtk.treeModelIterNext store iter
            if continue
                then getRuleIter' store rid iter
                else return Nothing



-- search the treeStore for an entry. If the entry is found then update it, else create the entry.
updateTreeStore :: Gtk.TreeStore -> ExecGraphEntry -> IO ()
updateTreeStore store entry = do
    mIter <- getFirstRuleIter store
    case mIter of
        Nothing -> do
            (valid,rootIter) <- Gtk.treeModelGetIterFirst store
            iter <- Gtk.treeStoreAppend store (Just rootIter)
            storeSetGraphEntry store iter entry
        Just iter -> updateTreeStore' store iter entry

updateTreeStore' :: Gtk.TreeStore -> Gtk.TreeIter -> ExecGraphEntry -> IO ()
updateTreeStore' store iter entry@(n,i,t,p,rn) = do
    cid <- Gtk.treeModelGetValue store iter 1 >>= fromGValue :: IO Int32
    ct  <- Gtk.treeModelGetValue store iter 2 >>= fromGValue :: IO Int32
    if ct == t && cid == i
        then storeSetGraphEntry store iter entry
        else case t of
            1 -> do
                (valid,rootIter) <- Gtk.treeModelIterParent store iter
                when valid $ updateInList rootIter
            2 -> case (ct==1,cid==p) of
                (True,True) -> do
                    (valid,childIter) <- Gtk.treeModelIterChildren store (Just iter)
                    if valid then
                      updateTreeStore' store childIter entry
                    else do
                      newIter <- Gtk.treeStoreAppend store (Just iter)
                      storeSetGraphEntry store newIter entry
                (True,False) -> do
                    valid <- Gtk.treeModelIterNext store iter
                    when valid $ updateTreeStore' store iter entry
                _ -> do
                    (valid,parentIter) <- Gtk.treeModelIterParent store iter
                    when valid $ updateInList parentIter
            4 -> insertMiscInRule (cid==p) 1
            5 -> insertMiscInRule (cid==p) 1
            _ -> return ()

    where
        updateInList parentIter = do
            valid <- Gtk.treeModelIterNext store iter
            if valid
                then updateTreeStore' store iter entry
                else do
                    newIter <- Gtk.treeStoreAppend store (Just parentIter)
                    storeSetGraphEntry store newIter entry
        insertMiscInRule insertInThis pos = do
            if insertInThis
                then do
                    newIter <- Gtk.treeStoreInsert store (Just iter) pos
                    storeSetGraphEntry store newIter entry
                else do
                    valid <- Gtk.treeModelIterNext store iter
                    if valid
                        then updateTreeStore' store iter entry
                        else return ()


-- | remove a rule entry from treeStore.
removeFromTreeStore :: Gtk.TreeStore -> Int32 -> IO ()
removeFromTreeStore store index = do
    mIter <- getFirstRuleIter store
    case mIter of
        Nothing -> return ()
        Just iter -> removeFromTreeStore' store iter index

removeFromTreeStore' :: Gtk.TreeStore -> Gtk.TreeIter -> Int32 -> IO ()
removeFromTreeStore' store iter index = do
    cindex <- Gtk.treeModelGetValue store iter 1 >>= fromGValue :: IO Int32
    if cindex == index
        then do
            Gtk.treeStoreRemove store iter
            return ()
        else do
            continue <- Gtk.treeModelIterNext store iter
            if continue
                then removeFromTreeStore' store iter index
                else return ()


-- | remove entries that contains invalid indexes - must pass a list of valid indexes
removeTrashFromTreeStore :: Gtk.TreeStore -> [Int32] -> IO ()
removeTrashFromTreeStore store validIndexes = do
    mIter <- getFirstRuleIter store
    case mIter of
        Nothing -> return ()
        Just iter -> removeTrashFromTreeStore' store iter validIndexes

removeTrashFromTreeStore' :: Gtk.TreeStore -> Gtk.TreeIter -> [Int32] -> IO ()
removeTrashFromTreeStore' store iter validIndexes = do
    index <- Gtk.treeModelGetValue store iter 1 >>= fromGValue :: IO Int32
    continue <- case index `elem` validIndexes of
        True -> Gtk.treeModelIterNext store iter
        False -> Gtk.treeStoreRemove store iter
    if continue
        then removeTrashFromTreeStore' store iter validIndexes
        else return ()


-- | get the GraphStates that are referenciated by the treeStore
treeStoreGetRules :: Gtk.TreeStore -> IORef (M.Map Int32 GraphState) -> IO [(Int32,GraphState)]
treeStoreGetRules store statesMap = do
    mIter <- getFirstRuleIter store
    case mIter of
        Nothing -> return []
        Just iter -> do
            statesM <- readIORef statesMap
            treeStoreGetRules' store iter statesM

treeStoreGetRules' :: Gtk.TreeStore -> Gtk.TreeIter -> M.Map Int32 GraphState -> IO [(Int32,GraphState)]
treeStoreGetRules' store iter statesMap = do
    index <- Gtk.treeModelGetValue store iter 1 >>= fromGValue :: IO Int32
    let state = M.lookup index statesMap
    continue <- Gtk.treeModelIterNext store iter
    states <- case continue of
        True -> treeStoreGetRules' store iter statesMap
        False -> return []
    case state of
        Nothing -> return states
        Just st -> return $ (index,st) : states


-- | remove all entries of matches (and comments, next and previous commands) from the treeStore
removeMatchesFromTreeStore :: Gtk.TreeStore -> IO ()
removeMatchesFromTreeStore store = do
    mIter <- getFirstRuleIter store
    case mIter of
        Nothing -> return ()
        Just iter -> removeMatchesFromTreeStore' store iter

removeMatchesFromTreeStore' :: Gtk.TreeStore -> Gtk.TreeIter -> IO ()
removeMatchesFromTreeStore' store iter = do
    t <- Gtk.treeModelGetValue store iter 2 >>= fromGValue :: IO Int32
    continue <- case t of
        1 -> do
            (valid,childIter) <- Gtk.treeModelIterChildren store (Just iter)
            if valid
                then removeMatchesFromTreeStore' store childIter
                else return ()
            Gtk.treeModelIterNext store iter
        _ -> Gtk.treeStoreRemove store iter
    if continue
        then removeMatchesFromTreeStore' store iter
        else return ()

-- | clear current level of entries from the treeStore
treeStoreClearCurrrentLevel :: Gtk.TreeStore -> Gtk.TreeIter -> IO ()
treeStoreClearCurrrentLevel store iter = do
    continue <- Gtk.treeStoreRemove store iter
    if continue
        then treeStoreClearCurrrentLevel store iter
        else return ()

-- | Get the rule and match indexes pointed by iter
getRuleAndMatchIndex :: Gtk.TreeModel -> Gtk.TreeIter -> IO (Int32, Int32)
getRuleAndMatchIndex model iter =
  do
    t <- Gtk.treeModelGetValue model iter 2 >>= fromGValue :: IO Int32
    case t of
        1 -> do -- selected rule entry
            ri <- Gtk.treeModelGetValue model iter 1 >>= fromGValue :: IO Int32
            return (ri,-1)
        2 -> do -- selected match entry
            ri <- Gtk.treeModelGetValue model iter 3 >>= fromGValue :: IO Int32
            rm <- Gtk.treeModelGetValue model iter 1 >>= fromGValue :: IO Int32
            return (ri,rm)
        4 -> do
            ri <- Gtk.treeModelGetValue model iter 3 >>= fromGValue :: IO Int32
            return (ri,-1)
        5 -> do
            ri <- Gtk.treeModelGetValue model iter 3 >>= fromGValue :: IO Int32
            return (ri,-1)
        _ -> return (-1,-1)
