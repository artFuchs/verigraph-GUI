
module Editor.GraphEditor.Helper.UndoRedo(
  ChangeStack
, stackUndo
, applyUndo
, applyRedo
)where

import Data.IORef

import Editor.Data.DiaGraph
import Editor.Data.EditorState
import Editor.Data.Nac

type ChangeStack = [(DiaGraph,Maybe MergeMapping)]

-- | Add a state to the undo stack
stackUndo :: IORef ChangeStack
          -> IORef ChangeStack
          -> EditorState
          -> Maybe MergeMapping
          -> IO ()
stackUndo undo redo es mergeM = do
  let g = editorGetGraph es
      gi = editorGetGI es
  modifyIORef undo (\u -> ((g,gi), mergeM):u )
  modifyIORef redo (\_ -> [])


-- | restore the last state stored in the undo stack and add the current state to the redo stack
applyUndo :: IORef ChangeStack
          -> IORef ChangeStack
          -> IORef EditorState
          -> IORef (Maybe MergeMapping)
          -> IO ()
applyUndo undoStack redoStack st mergeMappingIORef = do
  es <- readIORef st
  undo <- readIORef undoStack
  redo <- readIORef redoStack
  mergeM <- readIORef mergeMappingIORef
  let apply [] r es = ([], r, es, Nothing)
      apply (((g,gi),m):u) r es = (u, ((eg,egi), mergeM):r, editorSetGI gi . editorSetGraph g $ es, m)
                            where
                              eg = editorGetGraph es
                              egi = editorGetGI es
      (nu, nr, nes, nm) = apply undo redo es
  writeIORef undoStack nu
  writeIORef redoStack nr
  writeIORef st nes
  writeIORef mergeMappingIORef nm

-- | restore the last state stored in the redo stack and add the current state to the undo stack
applyRedo :: IORef ChangeStack
          -> IORef ChangeStack
          -> IORef EditorState
          -> IORef (Maybe MergeMapping)
          -> IO ()
applyRedo undoStack redoStack st mergeMappingIORef = do
  undo <- readIORef undoStack
  redo <- readIORef redoStack
  es <- readIORef st
  mergeM <- readIORef mergeMappingIORef
  let apply u [] es = (u, [], es, Nothing)
      apply u (((g,gi),m):r) es = (((eg,egi), mergeM):u , r, editorSetGI gi . editorSetGraph g $ es, m)
                            where
                              eg = editorGetGraph es
                              egi = editorGetGI es
      (nu, nr, nes, nm) = apply undo redo es
  writeIORef undoStack nu
  writeIORef redoStack nr
  writeIORef st nes
  writeIORef mergeMappingIORef nm
