{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module GUI.Executor (
  buildExecutor
, ExecGraphEntry
, updateTreeStore
, removeFromTreeStore
, removeTrashFromTreeStore
) where

-- GTK related modules
import qualified GI.Gtk                   as Gtk
import qualified GI.Gdk                   as Gdk
import qualified GI.Pango                 as P
import Graphics.Rendering.Cairo           (Render)

-- modules needed for threads
import           Control.Concurrent
import qualified GI.GLib as GLib

-- basic modules
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.IORef
import           Data.Maybe
import qualified Data.Map                 as M
import           Data.GI.Base
import           Data.GI.Base.ManagedPtr (unsafeCastTo)
import           Data.Int
import qualified Data.Text                as T
import           System.Random

-- Verigraph modules
import qualified Abstract.Category        as Cat
import qualified Abstract.Rewriting.DPO   as DPO
import qualified Data.TypedGraph.Morphism as TGM
import qualified Data.Graphs.Morphism     as GM
import qualified Data.Graphs              as G
import qualified Data.Relation            as R

-- Verigraph-GUI modules
import           GUI.Data.DiaGraph        hiding (empty)
import qualified GUI.Data.DiaGraph        as DG
import           GUI.Data.GraphState
import           GUI.Data.GraphicalInfo
import           GUI.Data.Info
import           GUI.Data.Nac
import           GUI.Render.Render
import           GUI.Render.GraphDraw
import           GUI.Helper.BasicCanvasCallbacks
import           GUI.Helper.FilePath
import qualified GUI.Helper.GrammarMaker  as GMker
import           GUI.Helper.OverlapAvoider
import           GUI.Helper.Geometry
import           GUI.Helper.GraphicalInfo

-- shouldn't use functions from editor module. Must refactore later
import qualified GUI.Editor.Helper.Nac    as Nac

type TGMProduction = DPO.Production (TGM.TypedGraphMorphism Info Info)
type Match = TGM.TypedGraphMorphism Info Info

buildExecutor :: Gtk.TreeStore 
              -> IORef (M.Map Int32 GraphState) 
              -> IORef (G.Graph Info Info) 
              -> IORef (M.Map Int32 NacInfo)
              -> IORef (Maybe Gtk.DrawingArea) -> IORef (Maybe (IORef GraphState))
              -> IO (Gtk.Paned, Gtk.DrawingArea, Gtk.ComboBoxText, IORef GraphState, IORef Bool, IORef (M.Map Int32 [(String, Int32)]))
buildExecutor store statesMap typeGraph nacInfoMap focusedCanvas focusedStateIORef = do
    builder <- new Gtk.Builder []
    resourcesFolder <- getResourcesFolder
    Gtk.builderAddFromFile builder $ T.pack (resourcesFolder ++ "executor.glade")

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

    nacCBox <- Gtk.builderGetObject builder "nacCBox" >>= unsafeCastTo Gtk.ComboBoxText . fromJust

    stopBtn <- Gtk.builderGetObject builder "stopBtn" >>= unsafeCastTo Gtk.Button . fromJust
    startBtn <- Gtk.builderGetObject builder "startBtn" >>= unsafeCastTo Gtk.Button . fromJust
    stepBtn <- Gtk.builderGetObject builder "stepBtn" >>= unsafeCastTo Gtk.Button . fromJust
    pauseBtn <- Gtk.builderGetObject builder "pauseBtn" >>= unsafeCastTo Gtk.Button . fromJust
    execSpeedBtn <- Gtk.builderGetObject builder "execSpeedBtn" >>= unsafeCastTo Gtk.SpinButton . fromJust

    statusLabel <- Gtk.builderGetObject builder "statusLabel" >>= unsafeCastTo Gtk.Label . fromJust
    statusSpinner <- Gtk.builderGetObject builder "statusSpinner" >>= unsafeCastTo Gtk.Spinner . fromJust

    keepRuleCheckBtn <- Gtk.builderGetObject builder "keepRuleCheckBtn" >>= unsafeCastTo Gtk.CheckButton . fromJust
    treeView <- Gtk.builderGetObject builder "treeView" >>= unsafeCastTo Gtk.TreeView . fromJust
    Gtk.treeViewSetModel treeView (Just store)

    rootPath <- Gtk.treePathNewFromIndices [0]
    Gtk.treeViewSetCursor treeView rootPath Gtk.noTreeViewColumn False

    -- IORefs -------------------------------------------------------------------------------------------------------------------
    hostState   <- newIORef emptyState -- state refering to the init graph with rules applied
    ruleState   <- newIORef emptyState -- state refering to the graph of the selected rule
    lState      <- newIORef emptyState -- state refering to the graph of the left side of selected rule
    rState      <- newIORef emptyState -- state refering to the graph of the right side of selected rule
    kGraph      <- newIORef G.empty -- k graph needed for displaying ids in the left and right sides of rules

    lStateOrig  <- newIORef emptyState -- similar to lState, but not moved
    nacState    <- newIORef emptyState -- state refering to the graph of nac
    
    mergeMap    <- newIORef (Nothing :: Maybe MergeMapping)
    nacIDListMap <- newIORef (M.empty :: M.Map Int32 [(String,Int32)])
    
    currentNACIndex    <- newIORef (-1 :: Int32) -- index of current selected NAC
    currentRuleIndex   <- newIORef (-1 :: Int32) -- index of currentRule

    productionMap <- newIORef (M.empty :: M.Map Int32 TGMProduction)
    matchesMap <- newIORef (M.empty :: M.Map Int32 (M.Map Int32 Match))
    currentMatchIndex <- newIORef (-1 :: Int32) -- index of current match

    execStarted  <- newIORef False   -- if execution has already started
    execThread  <- newIORef Nothing  -- thread for execution process
    execDelay <- newIORef (100000 :: Int) -- delay between execution steps
    execKeepInProd <- newIORef True  -- if after a execution step the same match should be selected
    processingMatches <- newIORef False -- flag to check if Verigraph is calculating the next matches - can be a long process if the graph is big and it would be bad to be executed more than once at the same time.

    
    initExecDelay <- Gtk.spinButtonGetValue execSpeedBtn
    writeIORef execDelay $ round (initExecDelay * 1000000)

    -- callbacks ----------------------------------------------------------------------------------------------------------------
    -- hide rule viewer panel when colse button is pressed
    on hideRVBtn #pressed $ do
        closePos <- get execPane #maxPosition
        Gtk.panedSetPosition execPane closePos

    -- canvas
    setCanvasCallBacks ruleCanvas ruleState typeGraph (Just drawRuleGraph) focusedCanvas focusedStateIORef
    setCanvasCallBacks lCanvas lState kGraph (Just drawRuleSideGraph) focusedCanvas focusedStateIORef
    setCanvasCallBacks rCanvas rState kGraph (Just drawRuleSideGraph) focusedCanvas focusedStateIORef
    setCanvasCallBacks rCanvas rState kGraph (Just drawRuleSideGraph) focusedCanvas focusedStateIORef

    (_,mainSqrSel) <- setCanvasCallBacks mainCanvas hostState typeGraph Nothing focusedCanvas focusedStateIORef
    on  mainCanvas #draw $ \context -> do
        es <- readIORef hostState
        tg <- readIORef typeGraph
        sq <- readIORef mainSqrSel
        rIndex <- readIORef currentRuleIndex
        mIndex <- readIORef currentMatchIndex
        matchesM <- readIORef matchesMap
        let matches = fromMaybe M.empty $ M.lookup rIndex matchesM
            match = M.lookup mIndex matches
            matchedElems = case match of
                                Nothing -> ([],[])
                                Just m ->  (matchedNodes,matchedEdges)
                                        where 
                                            mapping = TGM.mapping m
                                            nRel = GM.nodeRelation mapping
                                            eRel = GM.edgeRelation mapping 
                                            nMapping = R.mapping nRel
                                            eMapping = R.mapping eRel
                                            matchedNodes = concat $ M.elems nMapping 
                                            matchedEdges = concat $ M.elems eMapping
        renderWithContext context $ drawHostGraphWithMatches es sq tg matchedElems
        return False

    (_,nacSqrSel)<- setCanvasCallBacks nacCanvas nacState typeGraph Nothing focusedCanvas focusedStateIORef
    on nacCanvas #draw $ \context -> do
        es <- readIORef nacState
        tg <- readIORef typeGraph
        sq <- readIORef nacSqrSel
        index <- readIORef currentNACIndex
        mm <- readIORef mergeMap >>= return . fromMaybe (M.empty,M.empty)
        renderWithContext context $ drawNACGraph es sq tg mm
        return False
    
    on store #rowInserted $ \path iter -> do
        _ <- Gtk.treePathUp path
        _ <- Gtk.treeViewExpandRow treeView path False
        return ()

    -- when select a rule, change their states
    treeViewOccupied <- newIORef False
    on treeView #cursorChanged $ do
        selection <- Gtk.treeViewGetSelection treeView
        (sel,model,iter) <- Gtk.treeSelectionGetSelected selection
        if sel
            then do
                t <- Gtk.treeModelGetValue model iter 2 >>= fromGValue :: IO Int32
                (rIndex,mIndex) <- case t of
                    1 -> do 
                        ri <- Gtk.treeModelGetValue model iter 1 >>= fromGValue :: IO Int32
                        return (ri,-1) 
                        
                    2 -> do 
                        ri <- Gtk.treeModelGetValue model iter 3 >>= fromGValue :: IO Int32
                        rm <- Gtk.treeModelGetValue model iter 1 >>= fromGValue :: IO Int32
                        return (ri,rm)
                        
                    3 -> do 
                        ri <- Gtk.treeModelGetValue model iter 3 >>= fromGValue :: IO Int32
                        return (ri,-1)
                        
                    4 -> do -- show next 100 items or so
                        ri <- Gtk.treeModelGetValue model iter 3 >>= fromGValue :: IO Int32
                        occ <- readIORef treeViewOccupied
                        if occ
                            then return ()
                            else do
                                matchesM <- readIORef matchesMap
                                case M.lookup ri matchesM of 
                                    Nothing -> return ()
                                    Just nM -> do
                                        writeIORef treeViewOccupied True
                                        offset <- Gtk.treeModelGetValue model iter 1 >>= fromGValue :: IO Int32
                                        let numMatches = M.size nM
                                            offset' = 100+(fromIntegral offset)
                                            matchesL = drop offset' $ M.toList nM
                                            newCommentEntry = ( (show numMatches) ++ " matches (showing " ++ (show $ offset'+1) ++ "-" ++ (show $ min numMatches (offset'+100)) ++ ")", 0, 3, ri)
                                            newNextEntry = if numMatches-offset' > 100
                                                            then Just ( "next " ++ (show $ min (numMatches-offset'-100) 100 ) ++ " matches", fromIntegral offset', 4, ri)
                                                            else Nothing
                                            newPreviousEntry = ("previous 100 matches", fromIntegral offset', 5, ri)

                                        (_,iterParent) <- Gtk.treeModelIterParent store iter
                                        (_,firstChildIter) <- Gtk.treeModelIterChildren store (Just iterParent)
                                        treeStoreClearCurrrentLevel store firstChildIter
                                        
                                        updateTreeStore store newCommentEntry
                                        case newNextEntry of
                                            Nothing    -> return ()
                                            Just entry -> updateTreeStore store entry
                                        updateTreeStore store newPreviousEntry
                                        forM_ (take 100 matchesL) $ \(mid,m) -> updateTreeStore store ("match " ++ (show mid), mid, 2, ri)

                                        writeIORef treeViewOccupied False
                        return (ri,-1)                       
                        
                    5 -> do -- show previous 100
                        ri <- Gtk.treeModelGetValue model iter 3 >>= fromGValue :: IO Int32
                        occ <- readIORef treeViewOccupied
                        if occ
                            then return ()
                            else do
                                matchesM <- readIORef matchesMap
                                case M.lookup ri matchesM of
                                    Nothing -> return ()
                                    Just nM -> do
                                        writeIORef treeViewOccupied True
                                        offset <- Gtk.treeModelGetValue model iter 1 >>= fromGValue :: IO Int32
                                        let numMatches = M.size nM
                                            offset' = (fromIntegral offset) - 100
                                            matchesL = drop offset' $ M.toList nM
                                            newCommentEntry = ( (show numMatches) ++ " matches (showing " ++ (show $ offset'+1) ++ "-" ++ (show $ offset'+100) ++ ")", 0, 3, ri)
                                            newNextEntry = ( "next 100 matches", (fromIntegral offset'), 4, ri)
                                            newPreviousEntry = if offset' >= 100
                                                                then Just ("previous 100 matches", (fromIntegral offset'), 5, ri)
                                                                else Nothing

                                        (parentValid,iterParent) <- Gtk.treeModelIterParent store iter
                                        (childValid,firstChildIter) <- Gtk.treeModelIterChildren store (Just iterParent)
                                        treeStoreClearCurrrentLevel store firstChildIter
                                        
                                        updateTreeStore store newCommentEntry
                                        updateTreeStore store newNextEntry
                                        case newPreviousEntry of
                                            Nothing    -> return ()
                                            Just entry -> updateTreeStore store entry
                                        forM_ (take 100 matchesL) $ \(mid,m) -> updateTreeStore store ("match " ++ (show mid), mid, 2, ri)
                                        
                                        writeIORef treeViewOccupied False
                        
                        return (ri,-1)
                    _ -> return (-1,-1)
                writeIORef currentMatchIndex mIndex
                statesM <- readIORef statesMap
                --load rule
                let es = fromMaybe emptyState $ M.lookup rIndex statesM
                    g = stateGetGraph es
                    (l,k,r) = GMker.graphToRuleGraphs g
                    (ngiM,egiM) = stateGetGI es
                currRIndex <- readIORef currentRuleIndex
                if (currRIndex == rIndex)
                    then return ()
                    else do
                        -- split GIs into left, right and middle GIs
                        let lngiM = M.filterWithKey (\k a -> (G.NodeId k) `elem` (G.nodeIds l)) ngiM
                            legiM = M.filterWithKey (\k a -> (G.EdgeId k) `elem` (G.edgeIds l)) egiM
                            rngiM = M.filterWithKey (\k a -> (G.NodeId k) `elem` (G.nodeIds r)) ngiM
                            regiM = M.filterWithKey (\k a -> (G.EdgeId k) `elem` (G.edgeIds r)) egiM
                            (_,lgi) = adjustDiagrPosition (l,(lngiM,legiM))
                            (_,rgi) = adjustDiagrPosition (r,(rngiM,regiM))
                            (_,gi') = adjustDiagrPosition (g,(ngiM,egiM))
                        writeIORef ruleState $ stateSetGI gi' es
                        writeIORef lState $ stateSetGraph l . stateSetGI lgi $ es
                        writeIORef rState $ stateSetGraph r . stateSetGI rgi $ es
                        writeIORef lStateOrig $ stateSetGraph l . stateSetGI (lngiM,legiM) $ es
                        writeIORef kGraph k                    
                        writeIORef currentRuleIndex rIndex

                nacListM <- readIORef nacIDListMap
                let nacList = M.lookup rIndex nacListM
                
                Gtk.comboBoxTextRemoveAll nacCBox
                forM_ (fromMaybe [] nacList) $ \(str,index) -> Gtk.comboBoxTextAppendText nacCBox (T.pack str)
                Gtk.comboBoxSetActive nacCBox 0

                Gtk.widgetQueueDraw lCanvas
                Gtk.widgetQueueDraw rCanvas
                Gtk.widgetQueueDraw ruleCanvas
                Gtk.widgetQueueDraw nacCanvas
            else return ()

    -- use the comboBox on RuleViewer to select NAC graph to display
    on nacCBox #changed $ do
        index <- Gtk.comboBoxGetActive nacCBox
        if index == (-1)
            then writeIORef nacState $ emptyState
            else do
                nacName <- Gtk.comboBoxTextGetActiveText nacCBox >>= return . T.unpack
                ruleIndex <- readIORef currentRuleIndex
                nacListM <- readIORef nacIDListMap
                case M.lookup ruleIndex nacListM of
                    Nothing -> writeIORef nacState $ emptyState
                    Just nacList -> case lookup nacName nacList of
                        Nothing -> writeIORef nacState $ emptyState
                        Just nacIndex -> do
                            les <- readIORef lStateOrig
                            let l = stateGetGraph les
                                lgi = stateGetGI les
                            nacInfoM <- readIORef nacInfoMap
                            (n,ngi) <- case M.lookup nacIndex nacInfoM of
                                Nothing -> return (l,lgi)
                                Just nacInfo -> do
                                    context <- Gtk.widgetGetPangoContext nacCanvas
                                    tg <- readIORef typeGraph
                                    (nacdg',mergeM') <- Nac.applyLhsChangesToNac l nacInfo (Just context)
                                    writeIORef mergeMap $ Just mergeM'
                                    return $ Nac.mountNACGraph (l,lgi) tg (nacdg',mergeM')
                            let (_,ngi') = adjustDiagrPosition (n,ngi)
                                nes = stateSetGI ngi' . stateSetGraph n $ emptyState
                            writeIORef nacState nes
                            writeIORef currentNACIndex nacIndex

    -- execution controls
    -- when stop button is pressed, reset the host graph to initial state
    on stopBtn #pressed $ do
        writeIORef execStarted False
        processing <- readIORef processingMatches
        
        if processing
            then return ()
            else do
                -- kill execution thread if it's running
                mThread <- readIORef execThread
                case mThread of
                    Nothing -> return ()
                    Just t -> do 
                        killThread t
                        writeIORef execThread Nothing
                statesM <- readIORef statesMap
                -- reset the host graph to the initial stage
                writeIORef hostState $ fromMaybe emptyState ( M.lookup 1 statesM )
                removeMatchesFromTreeStore store
                -- load the productions of the treeStore
                loadProductions store typeGraph statesMap nacInfoMap nacIDListMap productionMap
                -- process matches
                execT <- forkFinally (do 
                                        writeIORef processingMatches True
                                        Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
                                            Gtk.spinnerStart statusSpinner
                                            Gtk.labelSetText statusLabel "processing matches"
                                            return False
                                        findMatches store statesMap hostState typeGraph nacInfoMap nacIDListMap matchesMap productionMap)
                                    (\_ -> do 
                                        writeIORef processingMatches False
                                        Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
                                            Gtk.spinnerStop statusSpinner
                                            Gtk.labelSetText statusLabel ""
                                            return False
                                        return ()
                                        )
                writeIORef execThread $ Just execT
                return ()


    -- when pause button is pressed, kills the execution thread
    on pauseBtn #pressed $ do
        writeIORef execStarted False
        writeIORef processingMatches False
        mThread <- readIORef execThread
        case mThread of
            Nothing -> return ()
            Just t -> do 
                killThread t
                writeIORef execThread Nothing
                Gtk.spinnerStop statusSpinner
                Gtk.labelSetText statusLabel ""
                
    
    -- when the step button is pressed, apply the match that is selected
    on stepBtn #pressed $ do 
        started <- readIORef execStarted
        processing <- readIORef processingMatches
        if started || processing
            then return ()
            else do 
                execT <- forkFinally 
                            (do writeIORef execStarted True
                                executeStep treeView store keepRuleCheckBtn mainCanvas statusSpinner statusLabel  typeGraph hostState statesMap nacInfoMap nacIDListMap matchesMap productionMap currentMatchIndex currentRuleIndex processingMatches)
                            (\_ -> writeIORef execStarted False)
                writeIORef execThread $ Just execT
    
    on startBtn #pressed $ do
        started <- readIORef execStarted
        processing <- readIORef processingMatches
        if started || processing
            then return () 
            else do 
                loadProductions store typeGraph statesMap nacInfoMap nacIDListMap productionMap
                execT <- forkFinally 
                            (do writeIORef execStarted True
                                executeMultipleSteps execDelay treeView store keepRuleCheckBtn mainCanvas statusSpinner statusLabel  typeGraph hostState statesMap nacInfoMap nacIDListMap matchesMap productionMap currentMatchIndex currentRuleIndex processingMatches)
                            (\_ -> writeIORef execStarted False)
                writeIORef execThread $ Just execT
                return ()

    on execSpeedBtn #valueChanged $ do
        value <- Gtk.spinButtonGetValue execSpeedBtn
        writeIORef execDelay $ round (value * 1000000)
    
    #show executorPane
    return (executorPane, mainCanvas, nacCBox, hostState, execStarted, nacIDListMap)


executeMultipleSteps execDelay treeView store keepRuleCheckBtn mainCanvas statusSpinner statusLabel typeGraph hostState statesMap nacInfoMap nacIDListMap matchesMap productionMap currentMatchIndex currentRuleIndex processingMatches = do

    executeStep treeView store keepRuleCheckBtn mainCanvas statusSpinner statusLabel  typeGraph hostState statesMap nacInfoMap nacIDListMap matchesMap productionMap currentMatchIndex currentRuleIndex processingMatches

    -- wait before the next step
    delay <- readIORef execDelay
    threadDelay delay

    -- checks if the selected 
    matchesM <- readIORef matchesMap
    let allMatches = concat $ M.elems $ M.map M.elems matchesM
    if length allMatches > 0
        then do 
            executeMultipleSteps execDelay treeView store keepRuleCheckBtn mainCanvas statusSpinner statusLabel typeGraph hostState statesMap nacInfoMap nacIDListMap matchesMap productionMap currentMatchIndex currentRuleIndex processingMatches
        else return ()

executeStep :: Gtk.TreeView -> Gtk.TreeStore -> Gtk.CheckButton -> Gtk.DrawingArea -> Gtk.Spinner -> Gtk.Label
            -> IORef (G.Graph Info Info) -> IORef GraphState -> IORef (M.Map Int32 GraphState) 
            -> IORef (M.Map Int32 NacInfo) -> IORef (M.Map Int32 [(String, Int32)])
            -> IORef (M.Map Int32 (M.Map Int32 Match)) -> IORef (M.Map Int32 TGMProduction) -> IORef Int32 -> IORef Int32 
            -> IORef Bool
            -> IO ()
executeStep treeView store keepRuleCheckBtn mainCanvas statusSpinner statusLabel typeGraph hostState statesMap nacInfoMap nacIDListMap matchesMap productionMap currentMatchIndex currentRuleIndex processingMatches = do
    Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
        Gtk.spinnerStart statusSpinner
        Gtk.labelSetText statusLabel "applying match"
        return False
    
    --apply match
    applyMatchAccordingToLevel hostState statesMap matchesMap productionMap currentMatchIndex currentRuleIndex

    Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
        hostSt <- readIORef hostState
        let g = stateGetGraph hostSt
            (ngi,egi) = stateGetGI hostSt
        context <- Gtk.widgetGetPangoContext mainCanvas
        ngi' <- updateNodesGiDims ngi g context
        modifyIORef hostState $ stateSetGI (ngi',egi)
        
        --update Canvas
        Gtk.widgetQueueDraw mainCanvas
        --update treeView cursor
        matchIndex <- readIORef currentMatchIndex
        keepInProd <- Gtk.toggleButtonGetActive keepRuleCheckBtn
        (maybePath,_) <- Gtk.treeViewGetCursor treeView
        case (matchIndex>=0, keepInProd, maybePath) of
            (True,True,Just path) -> do
                    hasParent <- Gtk.treePathUp path
                    if hasParent
                        then Gtk.treeViewSetCursor treeView path Gtk.noTreeViewColumn False
                        else return ()
            (_,False,_) -> do
                path <- Gtk.treePathNewFirst
                Gtk.treeViewSetCursor treeView path Gtk.noTreeViewColumn False
            _ -> return ()
        -- remove matches from treeStore
        removeMatchesFromTreeStore store
        Gtk.spinnerStart statusSpinner
        Gtk.labelSetText statusLabel "processing matches"
        return False

    -- find next matches
    writeIORef processingMatches True
    findMatches store statesMap hostState typeGraph nacInfoMap nacIDListMap matchesMap productionMap
    writeIORef processingMatches False
    
    Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
        -- if keepInProd is true but the selected production has no matches, then go up one level to full randomness
        Gtk.spinnerStop statusSpinner
        Gtk.labelSetText statusLabel ""
        keepInProd <- Gtk.toggleButtonGetActive keepRuleCheckBtn
        if keepInProd 
            then do
                matchesM <- readIORef matchesMap
                ruleIndex <- readIORef currentRuleIndex
                pathFst <- Gtk.treePathNewFirst
                case M.lookup ruleIndex matchesM of
                    Nothing -> Gtk.treeViewSetCursor treeView pathFst Gtk.noTreeViewColumn False
                    Just m -> if M.size m == 0 
                                then Gtk.treeViewSetCursor treeView pathFst Gtk.noTreeViewColumn False
                                else return ()
            else return ()
        return False
    return ()




type SquareSelection = Maybe (Double,Double,Double,Double)
setCanvasCallBacks :: Gtk.DrawingArea 
                   -> IORef GraphState 
                   -> IORef (G.Graph Info Info )
                   -> Maybe (GraphState -> SquareSelection -> G.Graph Info Info -> Render ())
                   -> IORef (Maybe Gtk.DrawingArea) -> IORef (Maybe (IORef GraphState))
                   -> IO (IORef (Double,Double), IORef SquareSelection)
setCanvasCallBacks canvas state refGraph drawMethod focusedCanvas focusedStateIORef = do
    oldPoint        <- newIORef (0.0,0.0) -- last point where a mouse button was pressed
    squareSelection <- newIORef Nothing   -- selection box : Maybe (x1,y1,x2,y2)
    case drawMethod of
        Just draw -> do 
            on canvas #draw $ \context -> do
                es <- readIORef state
                rg <- readIORef refGraph
                sq <- readIORef squareSelection
                renderWithContext context $ draw es sq rg
                return False
            return ()
        Nothing -> return ()
    on canvas #buttonPressEvent $ basicCanvasButtonPressedCallback state oldPoint squareSelection canvas
    on canvas #motionNotifyEvent $ basicCanvasMotionCallBack state oldPoint squareSelection canvas
    on canvas #buttonReleaseEvent $ basicCanvasButtonReleasedCallback state squareSelection canvas
    on canvas #scrollEvent $ basicCanvasScrollCallback state canvas
    on canvas #focusInEvent $ \event -> do
        writeIORef focusedCanvas $ Just canvas
        writeIORef focusedStateIORef $ Just state
        return False
    return (oldPoint,squareSelection) 
           
loadProductions :: Gtk.TreeStore -> IORef (G.Graph Info Info) -> IORef (M.Map Int32 GraphState) -> IORef (M.Map Int32 NacInfo) -> IORef (M.Map Int32 [(String,Int32)]) -> IORef (M.Map Int32 TGMProduction) -> IO ()
loadProductions store typeGraph statesMap nacInfoMap nacIDListMap productionMap = do
    typeG <- readIORef typeGraph
    nacListM <- readIORef nacIDListMap
    rulesStates <- treeStoreGetRules store statesMap
    nacInfoM <- readIORef nacInfoMap
    let tg = GMker.makeTypeGraph typeG
    productions <- forM rulesStates $ \(id,st) -> do
                        let nacList = fromMaybe [] $ M.lookup id nacListM
                        let getNac index = Just (\(dg,mm) -> (fst dg, mm)) <*> (M.lookup index nacInfoM)
                            mnacs = filter (not . null) $ map (\(_,index) -> getNac index) nacList
                            nacs = map fromJust mnacs
                        let rg = stateGetGraph st
                        return $ (id,GMker.graphToRule rg nacs tg)
    writeIORef productionMap (M.fromList productions)

-- process matches and
-- this function should be executed inside a thread
findMatches :: Gtk.TreeStore
            -> IORef (M.Map Int32 GraphState)
            -> IORef GraphState 
            -> IORef (G.Graph Info Info) 
            -> IORef (M.Map Int32 NacInfo)
            -> IORef (M.Map Int32 [(String,Int32)])
            -> IORef (M.Map Int32 (M.Map Int32 Match))
            -> IORef (M.Map Int32 TGMProduction)
            -> IO ()
findMatches store statesMap hostState typeGraph nacInfoMap nacIDListMap matchesMap productionMap = do
        -- get matches of each rule in the initial graph
        -- prepare initial graph
        hState <- readIORef hostState
        typeG <- readIORef typeGraph
        let g = stateGetGraph hState
            tg = GMker.makeTypeGraph typeG
            obj = GMker.makeTypedGraph g tg
        productions <- readIORef productionMap >>= return . M.toList
        -- get dpo matches
        let morphismClass = Cat.monic :: Cat.MorphismClass (TGM.TypedGraphMorphism Info Info)
            conf = DPO.MorphismsConfig morphismClass
            matches = map (\(id,prod) -> (id,DPO.findApplicableMatches conf prod obj)) productions
        let matchesM = foldr (\(rid,l) m -> M.insert rid (M.fromList $ zip ([1..] :: [Int32]) l) m) M.empty matches
        writeIORef matchesMap matchesM
        -- foreach match, generate a entry
        let matchesEntries = concat $
                                map (\(rid,mM) -> 
                                    map (\(mid,m) -> ("match " ++ (show mid), mid, 2 :: Int32, rid) ) $ M.toList mM) $
                                M.toList matchesM    

        forM_ (M.toList matchesM) $ \(rid,nM) -> do
            let numMatches = M.size nM
            if numMatches > 100
                then do 
                    Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
                        updateTreeStore store ((show numMatches) ++ " matches (showing 1-100)", 0, 3, rid)
                        updateTreeStore store ("next " ++ (show $ min 100 (numMatches - 100) ) ++ " matches",0, 4, rid)
                        return False
                else Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
                        updateTreeStore store ((show numMatches) ++ " matches", 0, 3, rid)
                        return False
            Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
                forM_ (take 100 $ M.toList nM) $ \(mid,_) -> updateTreeStore store ("match " ++ (show mid), mid, 2, rid)        
                return False
        return ()

-- apply matches according to the selected level on the treeView.
-- select top level (Grammar) to full randomness
-- select a prodution to get random matches of that production
-- select a match to no randomness
-- the selected level is given by currentMatchIndex and currentRuleIndex
applyMatchAccordingToLevel :: IORef GraphState 
                            -> IORef (M.Map Int32 GraphState) 
                            -> IORef (M.Map Int32 (M.Map Int32 Match)) 
                            -> IORef (M.Map Int32 TGMProduction)
                            -> IORef Int32
                            -> IORef Int32
                            -> IO ()
applyMatchAccordingToLevel hostState statesMap matchesMap productionMap currentMatchIndex currentRuleIndex= do
    matchesM <- readIORef matchesMap
    prodMap  <- readIORef productionMap
    mIndex <- readIORef currentMatchIndex
    rIndex <- readIORef currentRuleIndex
    
    let prod = M.lookup rIndex prodMap
        matches = fromMaybe M.empty $ M.lookup rIndex matchesM
        match = M.lookup mIndex matches

    -- apply match according to the level selected
    case (prod,match, rIndex, mIndex) of 
        (Just p, Just m, _, _) -> do  -- specified rule, specified match           
            applyMatch hostState statesMap rIndex p m

        (Just p,Nothing, _, -1) -> do -- specified rule, random match
            let matchesL = M.elems matches
            if length matchesL == 0
                then return ()
                else do
                    index <- randomRIO (0, (length matchesL)-1)
                    let m = matchesL!!index
                    applyMatch hostState statesMap rIndex p m

        (Nothing,Nothing,-1,-1) -> do -- full random
            let matchesLL = M.toList $ M.map M.elems matchesM
                mkpairs (x,[]) = []
                mkpairs (x,y:ys) = (x,y): mkpairs (x,ys)
                matchesEntries = concat $ map mkpairs matchesLL
            if length matchesEntries == 0
                then return ()
                else do
                    index <- randomRIO (0,(length matchesEntries)-1)
                    let (rIndex, m) = matchesEntries!!index
                        p = fromJust $ M.lookup rIndex prodMap                        
                    applyMatch hostState statesMap rIndex p m
        _ -> return ()

    

applyMatch :: IORef GraphState -> IORef (M.Map Int32 GraphState) -> Int32 -> TGMProduction -> Match -> IO ()
applyMatch hostState statesMap rIndex p m = do
    let (k,n,f,g) = DPO.calculateDPO m p        
        fMapping = TGM.mapping f
        gMapping = TGM.mapping g
        nMapping = TGM.mapping n
        fNodeRelation' = R.inverseRelation $ GM.nodeRelation fMapping -- G --fn'--> D
        fEdgeRelation' = R.inverseRelation $ GM.edgeRelation fMapping -- G --fe'--> D
        gNodeRelation = GM.nodeRelation gMapping                      -- D --gn--> H
        gEdgeRelation = GM.edgeRelation gMapping                      -- D --ge--> H
        nNodeRelation = GM.nodeRelation nMapping                      -- R --nn--> H
        nNodeRelation' = R.inverseRelation $ GM.nodeRelation nMapping -- H --nn'--> R
        nEdgeRelation' = R.inverseRelation $ GM.edgeRelation nMapping -- H --ne'--> R

    let apply rel k def = case R.apply rel k of [] -> def; id:_ -> id
        replaceInfo (k,i) m = case lookup k m of Nothing -> i; Just i' -> i'

    -- make sure the mapped elements preserve the information between transformations
    let gGraph = GM.domainGraph fMapping
        dGraph = GM.codomainGraph fMapping
        hGraph = GM.codomainGraph gMapping

        gNodesInfo = map (\(k,n) -> (apply fNodeRelation' k (G.NodeId (-1)), G.nodeInfo n)) (G.nodeMap gGraph)
        gEdgesInfo = map (\(k,e) -> (apply fEdgeRelation' k (G.EdgeId (-1)), G.edgeInfo e)) (G.edgeMap gGraph)
        
        dNodesInfo = map (\(k,n) -> (k, G.nodeInfo n)) (G.nodeMap dGraph)
        dEdgesInfo = map (\(k,e) -> (k, G.edgeInfo e)) (G.edgeMap dGraph)
        dNodesInfo' = map (\(k,ni) -> (apply gNodeRelation k (G.NodeId (-1)), replaceInfo (k,ni) gNodesInfo) ) dNodesInfo
        dEdgesInfo' = map (\(k,ei) -> (apply gEdgeRelation k (G.EdgeId (-1)), replaceInfo (k,ei) gEdgesInfo) ) dEdgesInfo

        hNodesInfo = map (\(k,n) -> (k, replaceInfo (k, G.nodeInfo n) dNodesInfo')) (G.nodeMap hGraph)
        hEdgesInfo = map (\(k,e) -> (k, replaceInfo (k, G.edgeInfo e) dEdgesInfo')) (G.edgeMap hGraph)
        
        hGraph' = foldr (\(k,i) g -> G.updateNodePayload k g (\ni -> i)) hGraph hNodesInfo
        hGraph'' = foldr (\(k,i) g -> G.updateEdgePayload k g (\ei -> i)) hGraph' hEdgesInfo
        hNodeMap' = map (\(k,n) -> (k, GMker.nodeFromJust n)) (G.nodeMap hGraph'')
        hEdgeMap' = map (\(k,e) -> (k, GMker.edgeFromJust e)) (G.edgeMap hGraph'')
        finalNodeMap = map (\(k,n) -> (k, n {G.nodeInfo = infoSetOperation (G.nodeInfo n) Preserve})) hNodeMap'
        finalEdgeMap = map (\(k,e) -> (k, e {G.edgeInfo = infoSetOperation (G.edgeInfo e) Preserve})) hEdgeMap'
        finalGraph = G.Graph finalNodeMap finalEdgeMap

    -- modify graphical information to match the modifications
    hostSt <- readIORef hostState
    statesM <- readIORef statesMap
    let (sgiN,sgiE) = stateGetGI hostSt
        -- delete layouts of elements that are not in the f morphism (G --f--> D)
        sgiN' = M.mapKeys G.NodeId $ M.filterWithKey (\k _ -> G.NodeId k `elem` (R.domain fNodeRelation')) sgiN
        sgiE' = M.mapKeys G.EdgeId $ M.filterWithKey (\k _ -> G.EdgeId k `elem` (R.domain fEdgeRelation')) sgiE
        -- modify the ids of the elements that are in the f morphism (G --f--> D)
        dgiN  = M.filterWithKey (\k _ -> fromEnum k > 0) $ M.mapKeys (\k -> apply fNodeRelation' k (G.NodeId (-1))) sgiN'
        dgiE  = M.filterWithKey (\k _ -> fromEnum k > 0) $ M.mapKeys (\k -> apply fEdgeRelation' k (G.EdgeId (-1))) sgiE'
        -- modify the ids of the elements that are in the g morphism (D --g--> H)
        dgiN' = M.filterWithKey (\k _ -> fromEnum k > 0) $ M.mapKeys (\k -> apply gNodeRelation k (G.NodeId (-1))) dgiN
        dgiE' = M.filterWithKey (\k _ -> fromEnum k > 0) $ M.mapKeys (\k -> apply gEdgeRelation k (G.EdgeId (-1))) dgiE

        -- add layouts to the elements of codomain of g morphism that have no relation to elements of it's domain
        rSt = fromJust $ M.lookup rIndex statesM
        (rgiN,rgiE) = stateGetGI rSt
        rGraph = stateGetGraph rSt
        addedNodeIds = filter (\id -> id `notElem` (M.keys dgiN')) (R.domain nNodeRelation')
        addedEdgeIds = filter (\id -> id `notElem` (M.keys dgiE')) (R.domain nEdgeRelation')
        addedNodeIds' = filter (\(_,kr) -> fromEnum kr > 0) $ map (\k -> (k, apply nNodeRelation' k (-1))) addedNodeIds
        addedEdgeIds' = filter (\(_,kr) -> fromEnum kr > 0) $ map (\k -> (k, apply nEdgeRelation' k (-1))) addedEdgeIds
        addedNodeGIs = map (\(k,kr) -> (k,fromJust $ M.lookup (fromEnum kr) rgiN)) addedNodeIds'
        addedEdgeGIs = map (\(k,kr) -> (k,fromJust $ M.lookup (fromEnum kr) rgiE)) addedEdgeIds'

        -- reposition added elements 
        addedEdgeGIs' = calculateEdgesPositions addedEdgeIds addedEdgeGIs dGraph hGraph gNodeRelation
        addedNodeGIs' = calculateNodesPositions addedNodeIds' addedNodeGIs rGraph rgiN dgiN' nNodeRelation

    let hgiN = foldr (\(k,gi) m -> M.insert k gi m) (M.mapKeys fromEnum $ dgiN') (map (\(k,gi) -> (fromEnum k, gi)) addedNodeGIs')
        hgiE = M.mapKeys fromEnum $ foldr (\(k,gi) m -> M.insert k gi m) dgiE' addedEdgeGIs'
        hState = stateSetSelected (addedNodeIds,addedEdgeIds) . stateSetGraph finalGraph . stateSetGI (hgiN,hgiE) $ hostSt

    writeIORef hostState hState

calculateEdgesPositions :: [G.EdgeId] -> [(G.EdgeId, EdgeGI)] -> G.Graph (Maybe Info) (Maybe Info) -> G.Graph (Maybe Info) (Maybe Info) -> R.Relation G.NodeId -> [(G.EdgeId, EdgeGI)]
calculateEdgesPositions addedEdgeIds addedEdgeGIs dGraph hGraph gNodeRelation = 
    addedEdgeGIs'
    where 
        -- 1. get src and tgt nodes from each edge in intermediary graph D;
        addedEdges = M.fromList $ map (\eid -> (eid, fromJust $ G.lookupEdge eid hGraph) ) addedEdgeIds
        gNodeRelation' = R.inverseRelation gNodeRelation
        addedEdgesPeerIds = M.map (\e -> (R.apply gNodeRelation' $ G.sourceId e, R.apply gNodeRelation' $ G.targetId e)) addedEdges
        -- 2. find what would be the added edges positions in D
        (_,edgesPositions) = M.foldrWithKey (\eid (srcl,tgtl) (g,m) ->  case (srcl,tgtl) of 
                                                        (src:_,tgt:_)-> let pos = if src == tgt then newLoopPos src g else newEdgePos src tgt g
                                                                            newId = head $ G.newEdges g
                                                                            g' = G.insertEdge newId src tgt g
                                                                        in (g',M.insert eid pos m)
                                                        _ -> (g,m))
                            (dGraph,M.empty) addedEdgesPeerIds
        -- 3. update positions of the GIs
        addedEdgeGIs' = map (\(k,gi) -> case M.lookup k edgesPositions of
                                            Nothing -> (k,gi)
                                            Just p -> (k,gi {cPosition = p})) addedEdgeGIs


calculateNodesPositions :: [(G.NodeId,G.NodeId)] -> [(G.NodeId,NodeGI)] -> G.Graph Info Info -> M.Map Int NodeGI -> M.Map G.NodeId NodeGI  -> R.Relation G.NodeId -> [(G.NodeId,NodeGI)]
calculateNodesPositions addedNodeIds' addedNodeGIs rGraph rgiN dgiN' nNodeRelation = addedNodeGIs'
    where
         -- 1. find an anchor node foreach added node in R
        addedNodesInContext = map (\(k,kr) -> (k,kr,G.lookupNodeInContext kr rGraph)) addedNodeIds'
        addedNodesInContext' = map (\(k,kr,nc) -> (k,kr,fromJust nc) ) $ filter (\(_,_,nc) -> not $ null nc) addedNodesInContext
        nodeIsPreserved n = (infoOperation $ G.nodeInfo n) == Preserve
        preservedNodes = filter nodeIsPreserved (G.nodes rGraph)
        anchorNodesInR = map (\(k,kr,(n,c)) -> let  nextNodes = filter nodeIsPreserved $ map fst $ [tgt | (_,_,tgt) <- G.outgoingEdges c]
                                                    prevNodes = filter nodeIsPreserved $ map fst $ [src | (src,_,_) <- G.incomingEdges c] 
                                                    anchorNodesIds = map G.nodeId $ case ( nextNodes ++ prevNodes, preservedNodes) of
                                                        (n1:n2:_,_) -> [n1,n2]
                                                        ([n],_) -> [n]
                                                        ([],n1:n2:_) -> [n1,n2]
                                                        ([],[n]) -> [n]
                                                        _ -> []
                                                in (k,kr,anchorNodesIds)
                                                ) addedNodesInContext'
        anchorNodesInR' = filter (\(k,kr,krA) -> not $ null krA) anchorNodesInR
        -- 2. calculate the relative position between each added node and it's anchor
        subPoint (a,b) (c,d) = (a-c,b-d)
        positionLists = map (\(k,kr,krAs) -> 
                            let
                                posN = position $ getNodeGI (fromEnum kr) rgiN
                                posAs = map (\krA -> position $ getNodeGI (fromEnum krA) rgiN) krAs 
                                posList = case posAs of 
                                    [posA] -> [posN, posA]
                                    [posA1,posA2] -> [posN, posA1, posA2]
                                    _ -> []
                            in  (k,krAs,posList)
                            ) anchorNodesInR'
        -- 3. calculate position that the node should have in H
        anchorNodesInH = map (\(k,krAs,posF) -> (k,map (R.apply nNodeRelation) krAs,posF)) positionLists
        addedNodePositions = M.fromList 
                                $ map (\(k,kAs,posL) -> let posAs = case kAs of
                                                                [(kA1:_),(kA2:_)] -> map (\kA -> position <$> M.lookup kA dgiN')  (map head kAs)
                                                                [kA:_] -> [position <$> M.lookup kA dgiN']
                                                                _ -> []
                                                            posN = case (posL,posAs) of
                                                                ([pN,pA],[Just pA']) -> Just $ addPoint pA' (subPoint pN pA)
                                                                ([pN,pA1,pA2],[Just pA1', Just pA2']) -> 
                                                                                let angle1 = angle pA1 pA2
                                                                                    angle2 = angle pA1' pA2'
                                                                                    (a,d) = toPolarFrom pA1 pN
                                                                                    dist = d * ( vectorLength ( subPoint pA2' pA1' ) / vectorLength ( subPoint pA2 pA1 ) )
                                                                                in Just $ pointAt (a + (angle2 - angle1)) dist pA1'
                                                                _ -> Nothing
                                                        in (k,posN)
                                                        ) anchorNodesInH
        -- 4. add to GI
        addedNodeGIs' = map (\(k,gi) -> let newPos = M.lookup k addedNodePositions
                                            gi' = case newPos of 
                                                    Just (Just pos) -> gi {position = pos}
                                                    _ -> gi
                                            gi'' = repositionNode gi' (M.mapKeys fromEnum dgiN',M.empty)
                                        in (k,gi'')
                                        ) addedNodeGIs


{-| ExecGraphEntry
    A tuple representing what is showed in each node of the tree in the treeview
    It contains the informations:
    * name,
    * id (case type is 2) or offset (case type is 4 or 5)
    * type (0 - topic, 1 - rule, 2 - rule match, 3 - comment about the numer of matches, 4 - next, 5 - previous),
    * parent id (used if type is >= 2).
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
            storeSetGraphEntry store rootIter ("Grammar", (-1), 0, (-1))
            return Nothing

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
updateTreeStore' store iter entry@(n,i,t,p) = do
    cid <- Gtk.treeModelGetValue store iter 1 >>= fromGValue :: IO Int32
    ct  <- Gtk.treeModelGetValue store iter 2 >>= fromGValue :: IO Int32
    if ct == t && cid == i
        then storeSetGraphEntry store iter entry
        else case t of
            1 -> do 
                (valid,rootIter) <- Gtk.treeModelIterParent store iter
                if valid
                    then updateInList rootIter
                    else return ()
            2 -> case (ct==1,cid==p) of
                (True,True) -> do
                    (valid,childIter) <- Gtk.treeModelIterChildren store (Just iter)
                    if valid
                        then updateTreeStore' store childIter entry
                        else do
                            newIter <- Gtk.treeStoreAppend store (Just iter)
                            storeSetGraphEntry store newIter entry
                (True,False) -> do
                    valid <- Gtk.treeModelIterNext store iter
                    if valid
                        then updateTreeStore' store iter entry
                        else return ()
                _ -> do
                    (valid,parentIter) <- Gtk.treeModelIterParent store iter
                    if valid
                        then updateInList parentIter
                        else return ()
            3 -> insertMiscInRule (cid==p) 0
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
         
