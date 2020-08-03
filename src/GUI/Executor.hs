{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module GUI.Executor (
  buildExecutor
, ExecGraphStore
, storeSetGraphStore
) where

import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk

import           Control.Monad.IO.Class
import           Data.IORef
import           Data.Maybe
import qualified Data.Map as M
import           Data.GI.Base
import           Data.GI.Base.ManagedPtr (unsafeCastTo)
import           Data.Int

import qualified Data.Graphs as G

import GUI.Data.EditorState
import GUI.Data.Info
import GUI.Render.Render
import GUI.Render.GraphDraw

-- this should not be used, but let if be for now
import qualified GUI.Editor as Editor (basicCanvasButtonPressedCallback, basicCanvasMotionCallBack, basicCanvasButtonReleasedCallback)

buildExecutor :: Gtk.TreeStore -> IORef (M.Map Int32 EditorState) -> IORef (G.Graph Info Info) ->  IO (Gtk.Paned)
buildExecutor store statesMap typeGraph = do
    builder <- new Gtk.Builder []
    Gtk.builderAddFromFile builder "./Resources/executor.glade"

    executorPane <- Gtk.builderGetObject builder "executorPane" >>= unsafeCastTo Gtk.Paned . fromJust

    execPane  <- Gtk.builderGetObject builder "execPane" >>= unsafeCastTo Gtk.Paned . fromJust 
    hideRVBtn <- Gtk.builderGetObject builder "hideRVBtn" >>= unsafeCastTo Gtk.Button . fromJust

    mainCanvas <- Gtk.builderGetObject builder "mainCanvas" >>= unsafeCastTo Gtk.DrawingArea . fromJust
    nacCanvas  <- Gtk.builderGetObject builder "nacCanvas" >>= unsafeCastTo Gtk.DrawingArea . fromJust
    lCanvas    <- Gtk.builderGetObject builder "lCanvas" >>= unsafeCastTo Gtk.DrawingArea . fromJust
    rCanvas    <- Gtk.builderGetObject builder "rCanvas" >>= unsafeCastTo Gtk.DrawingArea . fromJust
    Gtk.widgetSetEvents mainCanvas [toEnum $ fromEnum Gdk.EventMaskAllEventsMask - fromEnum Gdk.EventMaskSmoothScrollMask]
    Gtk.widgetSetEvents nacCanvas [toEnum $ fromEnum Gdk.EventMaskAllEventsMask - fromEnum Gdk.EventMaskSmoothScrollMask]
    Gtk.widgetSetEvents lCanvas [toEnum $ fromEnum Gdk.EventMaskAllEventsMask - fromEnum Gdk.EventMaskSmoothScrollMask]
    Gtk.widgetSetEvents rCanvas [toEnum $ fromEnum Gdk.EventMaskAllEventsMask - fromEnum Gdk.EventMaskSmoothScrollMask]

    stopBtn <- Gtk.builderGetObject builder "stopBtn" >>= unsafeCastTo Gtk.Button . fromJust

    treeView <- Gtk.builderGetObject builder "treeView" >>= unsafeCastTo Gtk.TreeView . fromJust
    Gtk.treeViewSetModel treeView (Just store)

    -- IORefs -------------------------------------------------------------------------------------------------------------------
    hostState       <- newIORef emptyES -- state refering to the init graph with rules applied
    oldPoint        <- newIORef (0.0,0.0) -- last point where a mouse button was pressed
    squareSelection <- newIORef Nothing -- selection box : Maybe (x1,y1,x2,y2)
    movingGI        <- newIORef False -- if the user started moving some object - necessary to add a position to the undoStack

    -- callbacks ----------------------------------------------------------------------------------------------------------------
    -- hide rule viewer panel when colse button is pressed
    on hideRVBtn #pressed $ do
        closePos <- get execPane #maxPosition
        Gtk.panedSetPosition execPane closePos

    on mainCanvas #draw $ \context -> do
        es <- readIORef hostState
        tg <- readIORef typeGraph
        sq <- readIORef squareSelection
        renderWithContext context $ drawHostGraph es sq tg
        return False
    on mainCanvas #buttonPressEvent $ Editor.basicCanvasButtonPressedCallback hostState oldPoint squareSelection mainCanvas
    on mainCanvas #motionNotifyEvent $ Editor.basicCanvasMotionCallBack hostState oldPoint squareSelection mainCanvas
    on mainCanvas #buttonReleaseEvent $ Editor.basicCanvasButtonReleasedCallback hostState squareSelection mainCanvas
    
    on stopBtn #pressed $ do
        statesM <- readIORef statesMap
        let initState = fromMaybe emptyES $ M.lookup 1 statesM
        writeIORef hostState initState
    
    return executorPane

{-| ExecGraphStore
    A tuple representing what is showed in each node of the tree in the treeview
    It contains the informations:
    * name,
    * graph id,
    * type (3 - rule, 4 - NAC)
-}
type ExecGraphStore = (String, Int32, Int32)

-- | set the ExecGraphStore in a position given by an iter in the TreeStore
storeSetGraphStore :: Gtk.TreeStore -> Gtk.TreeIter -> ExecGraphStore -> IO ()
storeSetGraphStore store iter (n,i,t) = do
    gv0 <- toGValue (Just n)
    gv1 <- toGValue i
    gv2 <- toGValue t
    #set store iter [0,1,2] [gv0,gv1,gv2]
