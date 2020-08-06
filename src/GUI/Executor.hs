{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module GUI.Executor (
  buildExecutor
, ExecGraphEntry
, updateTreeStore
) where

import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import Graphics.Rendering.Cairo (Render)

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
import GUI.Helper.GrammarMaker

-- this should not be used, but let if be for now
import qualified GUI.Editor as Editor (basicCanvasButtonPressedCallback, basicCanvasMotionCallBack, basicCanvasButtonReleasedCallback)

buildExecutor :: Gtk.TreeStore -> IORef (M.Map Int32 EditorState) -> IORef (G.Graph Info Info) ->  IO (Gtk.Paned, IORef EditorState, IORef Bool)
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
    ruleCanvas    <- Gtk.builderGetObject builder "ruleCanvas" >>= unsafeCastTo Gtk.DrawingArea . fromJust
    Gtk.widgetSetEvents mainCanvas [toEnum $ fromEnum Gdk.EventMaskAllEventsMask - fromEnum Gdk.EventMaskSmoothScrollMask]
    Gtk.widgetSetEvents nacCanvas [toEnum $ fromEnum Gdk.EventMaskAllEventsMask - fromEnum Gdk.EventMaskSmoothScrollMask]
    Gtk.widgetSetEvents lCanvas [toEnum $ fromEnum Gdk.EventMaskAllEventsMask - fromEnum Gdk.EventMaskSmoothScrollMask]
    Gtk.widgetSetEvents rCanvas [toEnum $ fromEnum Gdk.EventMaskAllEventsMask - fromEnum Gdk.EventMaskSmoothScrollMask]
    Gtk.widgetSetEvents ruleCanvas [toEnum $ fromEnum Gdk.EventMaskAllEventsMask - fromEnum Gdk.EventMaskSmoothScrollMask]

    stopBtn <- Gtk.builderGetObject builder "stopBtn" >>= unsafeCastTo Gtk.Button . fromJust

    treeView <- Gtk.builderGetObject builder "treeView" >>= unsafeCastTo Gtk.TreeView . fromJust
    Gtk.treeViewSetModel treeView (Just store)

    -- IORefs -------------------------------------------------------------------------------------------------------------------
    hostState       <- newIORef emptyES -- state refering to the init graph with rules applied
    ruleState       <- newIORef emptyES -- state refering to the graph of the selected rule
    lState          <- newIORef emptyES -- state refering to the graph of the left side of selected rule
    rState          <- newIORef emptyES -- state refering to the graph of the right side of selected rule

    execStarted     <- newIORef False   -- if execution has already started

    -- callbacks ----------------------------------------------------------------------------------------------------------------
    -- hide rule viewer panel when colse button is pressed
    on hideRVBtn #pressed $ do
        closePos <- get execPane #maxPosition
        Gtk.panedSetPosition execPane closePos

    -- canvas
    setCanvasCallBacks mainCanvas hostState typeGraph drawHostGraph
    setCanvasCallBacks ruleCanvas ruleState typeGraph drawRuleGraph
    setCanvasCallBacks lCanvas lState typeGraph drawRuleSideGraph
    setCanvasCallBacks rCanvas rState typeGraph drawRuleSideGraph
    

    -- when select a rule, change their states
    on treeView #cursorChanged $ do
        selection <- Gtk.treeViewGetSelection treeView
        (sel,model,iter) <- Gtk.treeSelectionGetSelected selection
        if sel
            then do
                index <- Gtk.treeModelGetValue model iter 1 >>= fromGValue :: IO Int32
                gType <- Gtk.treeModelGetValue model iter 2 >>= fromGValue :: IO Int32
                case gType of
                    3 -> do
                        statesM <- readIORef statesMap
                        let es = fromMaybe emptyES $ M.lookup index statesM
                            g = editorGetGraph es
                            (l,k,r) = graphToRuleGraphs g
                            (ngiM,egiM) = editorGetGI es
                            lngiM = M.filterWithKey (\k a -> (G.NodeId k) `elem` (G.nodeIds l)) ngiM
                            legiM = M.filterWithKey (\k a -> (G.EdgeId k) `elem` (G.edgeIds l)) egiM
                            rngiM = M.filterWithKey (\k a -> (G.NodeId k) `elem` (G.nodeIds r)) ngiM
                            regiM = M.filterWithKey (\k a -> (G.EdgeId k) `elem` (G.edgeIds r)) egiM
                            les = editorSetGraph l . editorSetGI (lngiM,legiM) $ es
                            res = editorSetGraph r . editorSetGI (rngiM,regiM) $ es
                        writeIORef ruleState es
                        writeIORef lState les
                        writeIORef rState res
                        Gtk.widgetQueueDraw lCanvas
                        Gtk.widgetQueueDraw rCanvas
                        Gtk.widgetQueueDraw ruleCanvas
                    4 -> return () -- TODO: set nac
                    _ -> return () -- error: the selected graph type is not a Rule nor a NAC

            else return ()


    -- execution controls
    on stopBtn #pressed $ do
        statesM <- readIORef statesMap
        let initState = fromMaybe emptyES $ M.lookup 1 statesM
        writeIORef hostState initState
        writeIORef execStarted False
    

    
    
    return (executorPane, hostState, execStarted)

setCanvasCallBacks :: Gtk.DrawingArea 
                   -> IORef EditorState 
                   -> IORef (G.Graph Info Info )
                   -> (EditorState -> Maybe (Double,Double,Double,Double) -> G.Graph Info Info -> Render ()) 
                   -> IO ()
setCanvasCallBacks canvas state typeGraph drawMethod = do
    oldPoint        <- newIORef (0.0,0.0) -- last point where a mouse button was pressed
    squareSelection <- newIORef Nothing   -- selection box : Maybe (x1,y1,x2,y2)
    movingGI        <- newIORef False     -- if the user started moving some object - necessary to add a position to the undoStack

    on canvas #draw $ \context -> do
        es <- readIORef state
        tg <- readIORef typeGraph
        sq <- readIORef squareSelection
        renderWithContext context $ drawMethod es sq tg
        return False
    on canvas #buttonPressEvent $ Editor.basicCanvasButtonPressedCallback state oldPoint squareSelection canvas
    on canvas #motionNotifyEvent $ Editor.basicCanvasMotionCallBack state oldPoint squareSelection canvas
    on canvas #buttonReleaseEvent $ Editor.basicCanvasButtonReleasedCallback state squareSelection canvas 
    return ()  

    



{-| ExecGraphEntry
    A tuple representing what is showed in each node of the tree in the treeview
    It contains the informations:
    * name,
    * id,
    * type (3 - rule, 4 - NAC),
    * parent id (used if type is NAC).
    * active
-}
type ExecGraphEntry = (String, Int32, Int32, Int32)

-- | set the ExecGraphStore in a position given by an iter in the TreeStore
storeSetGraphEntry :: Gtk.TreeStore -> Gtk.TreeIter -> ExecGraphEntry -> IO ()
storeSetGraphEntry store iter (n,i,t,p) = do
    gvn <- toGValue (Just n)
    gvi <- toGValue i
    gvt <- toGValue t
    gvp <- toGValue p
    #set store iter [0,1,2,3] [gvn,gvi,gvt,gvp]

-- search the treeStore for an entry. If the entry is found then update it, else create the entry.
updateTreeStore :: Gtk.TreeStore -> ExecGraphEntry -> IO ()
updateTreeStore store entry = do
    (valid,iter) <- Gtk.treeModelGetIterFirst store
    if valid
        then updateTreeStore' store iter entry
        else do
            iter <- Gtk.treeStoreAppend store Nothing
            storeSetGraphEntry store iter entry

updateTreeStore' :: Gtk.TreeStore -> Gtk.TreeIter -> ExecGraphEntry -> IO ()
updateTreeStore' store iter entry@(n,i,t,p) = do
    cid <- Gtk.treeModelGetValue store iter 1 >>= fromGValue :: IO Int32
    ct  <- Gtk.treeModelGetValue store iter 2 >>= fromGValue :: IO Int32
    if cid == i
        then storeSetGraphEntry store iter entry
        else do
            let updateInList parentIter = do
                    valid <- Gtk.treeModelIterNext store iter
                    if valid
                        then updateTreeStore' store iter entry
                        else do
                            newIter <- Gtk.treeStoreAppend store parentIter
                            storeSetGraphEntry store newIter entry
            case t of
                3 -> updateInList Nothing
                4 -> do
                    case (ct == 3,cid == p) of
                        (True,True) -> do
                            (valid,childIter) <- Gtk.treeModelIterChildren store (Just iter) 
                            if valid
                                then updateTreeStore' store childIter entry
                                else do
                                    childIter <- Gtk.treeStoreAppend store (Just iter)
                                    storeSetGraphEntry store childIter entry
                        (True,False) -> do
                            valid <- Gtk.treeModelIterNext store iter
                            if valid
                                then updateTreeStore' store iter entry
                                else return ()
                        _ -> do 
                            (valid,parentIter) <- Gtk.treeModelIterParent store iter
                            if valid
                                then updateInList (Just parentIter)
                                else return ()



            
    
    

    