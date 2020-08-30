
module GUI.Editor.Helper.UndoRedo(
  ChangeStack
, stackUndo
, applyUndo
, applyRedo
)where

import Data.IORef
import Data.Int
import qualified Data.Map as M
import Data.Maybe

import GUI.Data.DiaGraph
import GUI.Data.GraphState
import GUI.Data.Nac

type ChangeStack = [(DiaGraph,Maybe MergeMapping)]

-- | Add a state to the undo stack
stackUndo :: IORef (M.Map Int32 ChangeStack)
          -> IORef (M.Map Int32 ChangeStack)
          -> IORef Int32
          -> GraphState
          -> Maybe MergeMapping
          -> IO ()
stackUndo undoStack redoStack indexIORef es mergeM = do
  let g = stateGetGraph es
      gi = stateGetGI es
  undoStackM <- readIORef undoStack
  index <- readIORef indexIORef
  modifyIORef undoStack $ M.insert index $ ((g,gi), mergeM):( fromMaybe [] $ M.lookup index undoStackM )
  modifyIORef redoStack $ M.insert index []


-- | restore the last state stored in the undo stack and add the current state to the redo stack
applyUndo :: IORef (M.Map Int32 ChangeStack)
          -> IORef (M.Map Int32 ChangeStack)
          -> IORef Int32
          -> IORef GraphState
          -> IORef (Maybe MergeMapping)
          -> IO ()
applyUndo undoStack redoStack indexIORef st mergeMappingIORef = do
  undoStackM <- readIORef undoStack
  redoStackM <- readIORef redoStack
  index <- readIORef indexIORef
  es <- readIORef st
  mergeM <- readIORef mergeMappingIORef

  let apply [] r es = ([], r, es, Nothing)
      apply (((g,gi),m):u) r es = (u, ((eg,egi), mergeM):r, stateSetGI gi . stateSetGraph g $ es, m)
                            where
                              eg = stateGetGraph es
                              egi = stateGetGI es

  let undoS = fromMaybe [] $ M.lookup index undoStackM
      redoS = fromMaybe [] $ M.lookup index redoStackM
      (newUndoS, newRedoS, newES, newMergeMapping) = apply undoS redoS es
  modifyIORef undoStack $ M.insert index newUndoS
  modifyIORef redoStack $ M.insert index newRedoS
  writeIORef st newES
  writeIORef mergeMappingIORef newMergeMapping

-- | restore the last state stored in the redo stack and add the current state to the undo stack
applyRedo :: IORef (M.Map Int32 ChangeStack)
          -> IORef (M.Map Int32 ChangeStack)
          -> IORef Int32
          -> IORef GraphState
          -> IORef (Maybe MergeMapping)
          -> IO ()
applyRedo undoStack redoStack indexIORef st mergeMappingIORef = do
  undoStackM <- readIORef undoStack
  redoStackM <- readIORef redoStack
  index <- readIORef indexIORef
  es <- readIORef st
  mergeM <- readIORef mergeMappingIORef


  let apply u [] es = (u, [], es, Nothing)
      apply u (((g,gi),m):r) es = (((eg,egi), mergeM):u , r, stateSetGI gi . stateSetGraph g $ es, m)
                            where
                              eg = stateGetGraph es
                              egi = stateGetGI es

  let undoS = fromMaybe [] $ M.lookup index undoStackM
      redoS = fromMaybe [] $ M.lookup index redoStackM
      (newUndoS, newRedoS, newES, newMergeMapping) = apply undoS redoS es
  modifyIORef undoStack $ M.insert index newUndoS
  modifyIORef redoStack $ M.insert index newRedoS
  writeIORef st newES
  writeIORef mergeMappingIORef newMergeMapping
