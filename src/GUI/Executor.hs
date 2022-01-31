{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module GUI.Executor (
  buildExecutor
, updateTreeStore
, removeFromTreeStore
, removeTrashFromTreeStore
, applyMatch
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
import qualified Data.Word                as Word
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
import           GUI.Executor.TreeStore
import           GUI.Executor.MatchApplier
import           GUI.Render.Render
import           GUI.Render.GraphDraw
import           GUI.Helper.BasicCanvasCallbacks
import           GUI.Helper.FilePath
import qualified GUI.Helper.GrammarMaker  as GMker
import           GUI.Helper.Geometry
import           GUI.Helper.GraphicalInfo
import           GUI.Helper.SplitPredicates

-- shouldn't use functions from editor module. Must refactore later
import qualified GUI.Editor.Helper.Nac    as Nac


type TGMPredicate = TGMProduction


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
    currentMatchedElements <- newIORef (([],[]) :: ([G.NodeId],[G.EdgeId])) -- elements of hostState that should be highlighted
    ruleState   <- newIORef emptyState -- state refering to the graph of the selected rule
    lState      <- newIORef emptyState -- state refering to the graph of the left side of selected rule
    lStateOrig  <- newIORef emptyState -- similar to lState, but not moved. used to help displaying nacs
    rState      <- newIORef emptyState -- state refering to the graph of the right side of selected rule
    nacState    <- newIORef emptyState -- state refering to the graph of nac
    kGraph      <- newIORef G.empty -- k graph needed for displaying ids in the left and right sides of rules

    mergeMap    <- newIORef (Nothing :: Maybe MergeMapping)
    nacIDListMap <- newIORef (M.empty :: M.Map Int32 [(String,Int32)])
    currentNACIndex    <- newIORef (-1 :: Int32) -- index of current selected NAC

    productionMap <- newIORef (M.empty :: M.Map Int32 TGMProduction) -- map containing the productions of the grammar, must be set before executing transformations
    predicateMap <- newIORef (M.empty :: M.Map Int32 TGMPredicate) -- matches for predicates are computed, but not executed
    matchesMap <- newIORef (M.empty :: M.Map Int32 (M.Map Int32 Match)) -- map containing applicable matche of productions and predicates
    prodMatchesMap <- newIORef (M.empty :: M.Map Int32 (M.Map Int32 Match)) -- map containing applicable matches of productions
    currentMatchIndex <- newIORef (-1 :: Int32) -- index of current match
    currentRuleIndex   <- newIORef (-1 :: Int32) -- index of current selecte Rule

    -- control variables
    isInInitialState <- newIORef True -- if hostState refers to the initial state of grammar
    execThread  <- newIORef Nothing -- thread for execution process
    execKeepInProd <- newIORef True  -- if after a execution step the same match should be selected
    execDelay <- newIORef (100000 :: Int) -- delay between execution steps

    -- set initial exec delay according to the displayed in the GUI
    initExecDelay <- Gtk.spinButtonGetValue execSpeedBtn
    writeIORef execDelay $ round (initExecDelay * 1000000)

    -- callbacks ----------------------------------------------------------------------------------------------------------------

    -- variable bundles
    let controlVars = (isInInitialState, execThread) -- variables used to decide what action to ake when pressing 'step' or 'start'
        widgets = (treeView, keepRuleCheckBtn, mainCanvas, statusSpinner, statusLabel)
        statusWidgets = (statusSpinner, statusLabel)
        baseVars = (store, statesMap, typeGraph, nacInfoMap) -- 'global' variables
        execVars = (nacIDListMap, matchesMap, prodMatchesMap, productionMap, predicateMap) -- variables to store match information
        execStVars = (hostState, currentMatchIndex, currentRuleIndex, execDelay) -- variables needed to execute transformation steps
        selectedNacVars = (currentRuleIndex, currentNACIndex, nacIDListMap) -- variables to select wich nac is used
        nacStVars = (nacState, lStateOrig) -- variables used to show the current nac
        nacInfoVars = (nacInfoMap, mergeMap, typeGraph) -- variables used to build the nac




    -- some callbacks called by multiple events ---------------------------------------------------------------------------------

    -- Events and their callbacks -----------------------------------------------------------------------------------------------

    -- hide rule viewer panel when colse button is pressed
    on hideRVBtn #pressed $ do
        closePos <- get execPane #maxPosition
        Gtk.panedSetPosition execPane closePos

    -- canvas
    setBasicCanvasCallbacks ruleCanvas ruleState typeGraph (Just drawRuleGraph) focusedCanvas focusedStateIORef
    setBasicCanvasCallbacks lCanvas lState typeGraph (Just drawHostGraph) focusedCanvas focusedStateIORef
    setBasicCanvasCallbacks rCanvas rState typeGraph (Just drawHostGraph) focusedCanvas focusedStateIORef
    setMainCanvasCallbacks mainCanvas hostState typeGraph focusedCanvas focusedStateIORef currentMatchedElements
    setNacCanvasCallbacks nacCanvas nacState typeGraph focusedCanvas focusedStateIORef mergeMap

    on store #rowInserted $ \path iter -> do
        _ <- Gtk.treePathUp path
        _ <- Gtk.treeViewExpandRow treeView path False
        return ()

    -- when select a rule, change their states
    on treeView #cursorChanged $
      selectRuleAndMatch
          treeView nacCBox [mainCanvas, lCanvas, rCanvas, ruleCanvas, nacCanvas]
          statesMap currentRuleIndex nacIDListMap
          matchesMap currentMatchIndex currentMatchedElements
          hostState lStateOrig rState lState ruleState kGraph

    -- use the comboBox on RuleViewer to select NAC graph to display
    on nacCBox #changed $ nacCBoxChangedCallback nacCBox nacCanvas selectedNacVars nacInfoVars nacStVars

    on treeView #rowActivated $ treeViewRowActivatedCallback controlVars widgets baseVars execVars execStVars

    -- execution controls
    -- when stop button is pressed, reset the host graph to initial state
    on stopBtn #pressed $ stopPressedCallBack controlVars baseVars execVars execStVars statusWidgets

    -- when pause button is pressed, kills the execution thread
    on pauseBtn #pressed $ killThreadIfRunning execThread

    -- when the step button is pressed, apply the match that is selected
    on stepBtn #pressed $ startStepExecution controlVars widgets baseVars execVars execStVars

    -- when the start button is pressed, start applying mutiple steps
    on startBtn #pressed $ startPressedCallback controlVars widgets baseVars execVars execStVars

    on execSpeedBtn #valueChanged $ execSpeedBtnChangeCallback execSpeedBtn execDelay

    #show executorPane
    return (executorPane, mainCanvas, nacCBox, hostState, isInInitialState, nacIDListMap)


--------------------------------------------------------------------------------------------------------------------------------
-- Callback functions in appearance order order --------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------

setMainCanvasCallbacks mainCanvas hostState typeGraph focusedCanvas focusedStateIORef currentMatchedElements =
  do
    (_,mainSqrSel) <- setBasicCanvasCallbacks mainCanvas hostState typeGraph Nothing focusedCanvas focusedStateIORef
    on mainCanvas #draw $ \context -> do
        st <- readIORef hostState
        sq <- readIORef mainSqrSel
        aloc <- Gtk.widgetGetAllocation mainCanvas
        w <- Gdk.getRectangleWidth aloc >>= return . fromIntegral :: IO Double
        h <- Gdk.getRectangleHeight aloc >>= return . fromIntegral :: IO Double
        matchedElems <- readIORef currentMatchedElements
        renderWithContext context $ drawGraphHighlighting st sq matchedElems (Just (w,h))
        return False


setNacCanvasCallbacks nacCanvas nacState typeGraph focusedCanvas focusedStateIORef mergeMap = do
    (_,nacSqrSel) <- setBasicCanvasCallbacks nacCanvas nacState typeGraph Nothing focusedCanvas focusedStateIORef
    on nacCanvas #draw $ \context -> do
        es <- readIORef nacState
        tg <- readIORef typeGraph
        sq <- readIORef nacSqrSel
        mm <- readIORef mergeMap >>= return . fromMaybe (M.empty,M.empty)
        aloc <- Gtk.widgetGetAllocation nacCanvas
        w <- Gdk.getRectangleWidth aloc >>= return . fromIntegral :: IO Double
        h <- Gdk.getRectangleHeight aloc >>= return . fromIntegral :: IO Double
        renderWithContext context $ drawNACGraph es sq tg mm (Just (w,h))
        return False


-- Callback for when the treeview cursor is changed.
-- Show the selected rule, as well as the selected match if any.
-- This also select the first Nac of the rule, showing it.
selectRuleAndMatch
  treeView nacCBox
  allCanvas@[mainCanvas, lCanvas, rCanvas, ruleCanvas, nacCanvas]
  statesMap currentRuleIndex nacIDListMap
  matchesMap currentMatchIndex currentMatchedElements
  hostState lStateOrig rState lState ruleState kGraph =
  do
    selection <- Gtk.treeViewGetSelection treeView
    (sel,model,iter) <- Gtk.treeSelectionGetSelected selection
    when sel $
      do
        (rIndex, mIndex) <- getRuleAndMatchIndex model iter
        currRIndex <- readIORef currentRuleIndex
        --load rule
        when (currRIndex /= rIndex) $ do
          loadSelectedRule statesMap rIndex ruleState lState rState lStateOrig kGraph lCanvas rCanvas ruleCanvas
          writeIORef currentRuleIndex rIndex

        -- set the NAC list to the NAC combo box
        nacListM <- readIORef nacIDListMap
        let nacList = M.lookup rIndex nacListM
        Gtk.comboBoxTextRemoveAll nacCBox
        forM_ (fromMaybe [] nacList) $ \(str,index) -> Gtk.comboBoxTextAppendText nacCBox (T.pack str)
        Gtk.comboBoxSetActive nacCBox 0

        -- load match
        setMatchingElementsText matchesMap rIndex mIndex hostState mainCanvas currentMatchedElements
        writeIORef currentMatchIndex mIndex

        mapM_ Gtk.widgetQueueDraw allCanvas




nacCBoxChangedCallback
  nacCBox nacCanvas
  (currentRuleIndex, currentNACIndex, nacIDListMap)
  (nacInfoMap, mergeMap, typeGraph)
  (nacState, lStateOrig)
  =
  do
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
                        (nacdg,mergeM) <- case M.lookup nacIndex nacInfoM of
                            Nothing -> return ((l,lgi),(M.empty,M.empty))
                            Just nacInfo -> do
                                context <- Gtk.widgetGetPangoContext nacCanvas
                                tg <- readIORef typeGraph
                                (nacdg,mergeM) <- Nac.applyLhsChangesToNac l nacInfo (Just context)
                                let nacdg' = Nac.mountNACGraph (l,lgi) tg (nacdg,mergeM)
                                return (nacdg',mergeM)
                        context <- Gtk.widgetGetPangoContext nacCanvas
                        let (nacG,ngi') = adjustDiagrPosition nacdg
                            rNMapping = M.fromList $ map (\(a,b) -> (b,"[" ++ (show . fromEnum $ a) ++ "]") ) $ M.toList $ M.union (fst mergeM) $ M.fromList (map (\a -> (a,a)) $ G.nodeIds l)
                            rEMapping = M.fromList $ map (\(a,b) -> (b,"[" ++ (show . fromEnum $ a) ++ "]") ) $ M.toList $ M.union (snd mergeM) $ M.fromList (map (\a -> (a,a)) $ G.edgeIds l)
                            nst = stateSetGI ngi' . stateSetGraph nacG $ emptyState
                        nst' <- setInfoExtra nst (rNMapping, rEMapping) context
                        writeIORef mergeMap (Just mergeM)
                        writeIORef nacState nst'
                        writeIORef currentNACIndex nacIndex
                        Gtk.widgetQueueDraw nacCanvas



treeViewRowActivatedCallback
  controlVars
  widgets
  baseVars@(store,_,_,_)
  execVars@(_, matchesMap, _, _, _)
  execStVars
  path col
  =
  do
    (v,iter) <- Gtk.treeModelGetIter store path
    when v $ do
      t <- Gtk.treeModelGetValue store iter 2 >>= fromGValue :: IO Int32
      case t of
        1 -> startStepExecution controlVars widgets baseVars execVars execStVars
        2 -> startStepExecution controlVars widgets baseVars execVars execStVars
        3 -> startStepExecution controlVars widgets baseVars execVars execStVars
        4 -> showNextMatches store iter matchesMap
        5 -> showPreviousMatches store iter matchesMap
        _ -> return ()





stopPressedCallBack
  (isInInitialState, execThread)
  (store, statesMap, typeGraph, nacInfoMap)
  (nacIDListMap, matchesMap, prodMatchesMap, productionMap, predicateMap)
  (hostState, currentMatchIndex, currentRuleIndex, execDelay)
  statusWidgets@(statusSpinner, statusLabel) =
  do
    -- stop execution if running and set control variables to initial state
    writeIORef isInInitialState True
    killThreadIfRunning execThread
    -- reset the host graph to the initial stage
    statesM <- readIORef statesMap
    writeIORef hostState $ fromMaybe emptyState ( M.lookup 1 statesM )
    removeMatchesFromTreeStore store
    -- load the productions of the treeStore
    loadProductions store typeGraph statesMap nacInfoMap nacIDListMap productionMap predicateMap
    -- process matches
    startExecThread execThread statusWidgets $
      do
        Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
            Gtk.spinnerStart statusSpinner
            Gtk.labelSetText statusLabel "processing matches"
            return False
        findMatches store hostState typeGraph nacInfoMap nacIDListMap matchesMap prodMatchesMap productionMap predicateMap




-- Callback
startPressedCallback
  controlVars@(isInInitialState, execThread)
  (treeView, keepRuleCheckBtn, mainCanvas, statusSpinner, statusLabel)
  (store, statesMap, typeGraph, nacInfoMap)
  (nacIDListMap, matchesMap, prodMatchesMap, productionMap, predicateMap)
  (hostState, currentMatchIndex, currentRuleIndex, execDelay) =
    startExecThread
      execThread (statusSpinner, statusLabel)
      $ do
          writeIORef isInInitialState False
          executeMultipleSteps
            execDelay
            treeView keepRuleCheckBtn mainCanvas statusSpinner statusLabel
            store statesMap typeGraph nacInfoMap
            nacIDListMap matchesMap prodMatchesMap productionMap predicateMap
            hostState currentMatchIndex currentRuleIndex




-- set the execThread to apply a transformation step
startStepExecution
  controlVars@(isInInitialState, execThread)
  (treeView, keepRuleCheckBtn, mainCanvas, statusSpinner, statusLabel)
  (store, statesMap, typeGraph, nacInfoMap)
  (nacIDListMap, matchesMap, prodMatchesMap, productionMap, predicateMap)
  (hostState, currentMatchIndex, currentRuleIndex, _)
  =
  startExecThread
    execThread (statusSpinner, statusLabel)
    $ do
        writeIORef isInInitialState False
        executeStep
          treeView keepRuleCheckBtn mainCanvas statusSpinner statusLabel
          store statesMap typeGraph nacInfoMap
          nacIDListMap matchesMap prodMatchesMap productionMap predicateMap
          hostState currentMatchIndex currentRuleIndex




--change the execution speed of the transformation
execSpeedBtnChangeCallback :: Gtk.SpinButton -> IORef Int -> IO ()
execSpeedBtnChangeCallback execSpeedBtn execDelay = do
  value <- Gtk.spinButtonGetValue execSpeedBtn
  writeIORef execDelay $ round (value * 1000000)


------------------------------------------------------------------------------------------------------------------------------------------------
-- auxiliar functions related with setting and executing the analysis---------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------

-- execute multiple transformation steps, delayed by a time the user specifies until there are no more matches to apply
executeMultipleSteps :: IORef Int -- speed control
            -> Gtk.TreeView -> Gtk.CheckButton -> Gtk.DrawingArea -> Gtk.Spinner -> Gtk.Label -- widgets
            -> Gtk.TreeStore -> IORef (M.Map Int32 GraphState) -> IORef (G.Graph Info Info) -> IORef (M.Map Int32 NacInfo) -- general graph information
            -> IORef (M.Map Int32 [(String, Int32)]) -> IORef (M.Map Int32 (M.Map Int32 Match)) -> IORef (M.Map Int32 (M.Map Int32 Match))
            -> IORef (M.Map Int32 TGMProduction) -> IORef (M.Map Int32 TGMPredicate) -- matches  and productions
            -> IORef GraphState -> IORef Int32 -> IORef Int32 -- current state
            -> IO ()
executeMultipleSteps execDelay
                     treeView keepRuleCheckBtn mainCanvas statusSpinner statusLabel
                     store statesMap typeGraph nacInfoMap
                     nacIDListMap matchesMap prodMatchesMap productionMap predicateMap
                     hostState currentMatchIndex currentRuleIndex =
  do
    executeStep treeView keepRuleCheckBtn mainCanvas statusSpinner statusLabel
                store statesMap typeGraph nacInfoMap
                nacIDListMap matchesMap prodMatchesMap productionMap predicateMap
                hostState currentMatchIndex currentRuleIndex

    Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
        Gtk.spinnerStart statusSpinner
        Gtk.labelSetText statusLabel "executing"
        return False

    -- wait before the next step
    delay <- readIORef execDelay
    threadDelay delay

    -- if there are matches to apply then apply them
    matchesM <- readIORef prodMatchesMap
    let allMatches = concat $ M.elems $ M.map M.elems matchesM
    if length allMatches > 0
        then do
            executeMultipleSteps execDelay
                                 treeView keepRuleCheckBtn mainCanvas statusSpinner statusLabel
                                 store statesMap typeGraph nacInfoMap
                                 nacIDListMap matchesMap prodMatchesMap productionMap predicateMap
                                 hostState currentMatchIndex currentRuleIndex
        else return ()




-- execute a transformation step
executeStep :: Gtk.TreeView -> Gtk.CheckButton -> Gtk.DrawingArea -> Gtk.Spinner -> Gtk.Label -- widgets
            -> Gtk.TreeStore -> IORef (M.Map Int32 GraphState) -> IORef (G.Graph Info Info) -> IORef (M.Map Int32 NacInfo) -- general graph information
            -> IORef (M.Map Int32 [(String, Int32)]) -> IORef (M.Map Int32 (M.Map Int32 Match)) -> IORef (M.Map Int32 (M.Map Int32 Match))
            -> IORef (M.Map Int32 TGMProduction) -> IORef (M.Map Int32 TGMPredicate)
            -> IORef GraphState -> IORef Int32 -> IORef Int32 -- current state
            -> IO ()
executeStep treeView keepRuleCheckBtn mainCanvas statusSpinner statusLabel
            store statesMap typeGraph nacInfoMap
            nacIDListMap matchesMap prodMatchesMap productionMap predicateMap
            hostState currentMatchIndex currentRuleIndex = do
    Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
        Gtk.spinnerStart statusSpinner
        Gtk.labelSetText statusLabel "applying match"
        return False

    --apply match
    applyMatchAccordingToLevel hostState statesMap prodMatchesMap productionMap currentMatchIndex currentRuleIndex

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
    findMatches store hostState typeGraph nacInfoMap nacIDListMap matchesMap prodMatchesMap productionMap predicateMap

    clearStatusIndicators statusSpinner statusLabel

    -- if keepInProd is true but the selected production has no matches, then go up one level to full randomness
    Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
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
            applyMatchIO hostState statesMap rIndex p m

        (Just p,Nothing, _, -1) -> do -- specified rule, random match
            let matchesL = M.elems matches
            if length matchesL == 0
                then return ()
                else do
                    index <- randomRIO (0, (length matchesL)-1)
                    let m = matchesL!!index
                    applyMatchIO hostState statesMap rIndex p m

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
                        mp = M.lookup rIndex prodMap
                    case mp of
                      Nothing -> return ()
                      Just p -> applyMatchIO hostState statesMap rIndex p m

        _ -> return ()


-- apply a match to the hostGraph, modifying it's IORef.
applyMatchIO :: IORef GraphState -> IORef (M.Map Int32 GraphState) -> Int32 -> TGMProduction -> Match -> IO ()
applyMatchIO hostState statesMap rIndex p m = do
    hostSt <- readIORef hostState
    statesM <- readIORef statesMap
    hState <- return $ applyMatch hostSt statesM rIndex p m
    writeIORef hostState hState

-- find applicable matches for all rules
-- this function should be executed inside a thread
findMatches :: Gtk.TreeStore
            -> IORef GraphState
            -> IORef (G.Graph Info Info)
            -> IORef (M.Map Int32 NacInfo)
            -> IORef (M.Map Int32 [(String,Int32)])
            -> IORef (M.Map Int32 (M.Map Int32 Match))
            -> IORef (M.Map Int32 (M.Map Int32 Match))
            -> IORef (M.Map Int32 TGMProduction)
            -> IORef (M.Map Int32 TGMPredicate)
            -> IO ()
findMatches store hostState typeGraph nacInfoMap nacIDListMap matchesMap prodMatchesMap productionMap predicateMap = do
    -- prepare initial graph
    g <- readIORef hostState >>= return . stateGetGraph
    typeG <- readIORef typeGraph
    let tg = GMker.makeTypeGraph typeG
        obj = GMker.makeTypedGraph g tg
    productions <- readIORef productionMap >>= return . M.toList
    predicates <- readIORef predicateMap >>= return . M.toList
    -- get dpo matches
    let morphismClass = Cat.monic :: Cat.MorphismClass (TGM.TypedGraphMorphism Info Info)
        conf = DPO.MorphismsConfig morphismClass
        findMs (id,prod) = (id,DPO.findApplicableMatches conf prod obj)
        prodMatches = map findMs productions
        predMatches = map findMs predicates
    let buildMatchM = foldr (\(rid,l) m -> M.insert rid (M.fromList $ zip ([1..] :: [Int32]) l) m) M.empty
        prodMatchesM = buildMatchM prodMatches
        matchesM = buildMatchM (prodMatches ++ predMatches)
    writeIORef matchesMap matchesM
    writeIORef prodMatchesMap prodMatchesM
    -- add matches entries to the treeStore
    populateTreeStoreWithMatches store matchesM



-- load the productions and predicates of the grammar
loadProductions :: Gtk.TreeStore
                -> IORef (G.Graph Info Info) -> IORef (M.Map Int32 GraphState) -> IORef (M.Map Int32 NacInfo)
                -> IORef (M.Map Int32 [(String,Int32)])
                -> IORef (M.Map Int32 TGMProduction) -> IORef (M.Map Int32 TGMProduction)
                -> IO ()
loadProductions store typeGraph statesMap nacInfoMap nacIDListMap productionMap predicateMap = do
    typeG <- readIORef typeGraph
    nacListM <- readIORef nacIDListMap
    rulesStates <- treeStoreGetRules store statesMap
    nacInfoM <- readIORef nacInfoMap
    let tg = GMker.makeTypeGraph typeG
    rules <- forM rulesStates $ \(id,st) -> do
                        let nacList = fromMaybe [] $ M.lookup id nacListM
                        let getNac index = do
                                            (dg,mm) <- M.lookup index nacInfoM
                                            return (fst dg, mm)
                            nacs = catMaybes $ map (\(_,index) -> getNac index) nacList
                        let rg = stateGetGraph st
                        return $ (show id, GMker.graphToRule rg nacs tg)
    let (productions', predicates') = splitPredicates rules
        productions = map (\(k,r) -> (read k :: Int32, r)) productions'
        predicates = map (\(k,r) -> (read k :: Int32, r)) predicates'
    writeIORef productionMap (M.fromList productions)
    writeIORef predicateMap (M.fromList predicates)


------------------------------------------------------------------------------------------------------------------------------------------------
-- auxiliar functions related with the status indicator widgets --------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------

-- small auxiliar function to stop the statusSpinner and clear te statusLabel
clearStatusIndicators :: Gtk.Spinner -> Gtk.Label -> IO Word.Word32
clearStatusIndicators statusSpinner statusLabel =
  Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
    Gtk.spinnerStop statusSpinner
    Gtk.labelSetText statusLabel ""
    return False

-- small auxiliar function to stop the statusSpinner and clear te statusLabel
indicateStatus :: Gtk.Spinner -> Gtk.Label -> T.Text -> IO Word.Word32
indicateStatus statusSpinner statusLabel statusText = do
  Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
    Gtk.spinnerStop statusSpinner
    Gtk.labelSetText statusLabel statusText
    return False


------------------------------------------------------------------------------------------------------------------------------------------------
-- auxiliar functions related with setting threads ---------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------

-- start the execTread with a function of type IO ()
-- if the execution and it's not processing matches
startExecThread :: IORef (Maybe ThreadId)
                -> (Gtk.Spinner, Gtk.Label)
                -> IO () -> IO ()
startExecThread
  execThread
  (statusSpinner, statusLabel)
  function
  =
  do
    mT <- readIORef execThread
    case mT of
      Just t -> return ()
      Nothing -> do
        execT <- forkFinally
                    function
                    (\_ -> do
                      clearStatusIndicators statusSpinner statusLabel
                      writeIORef execThread $ Nothing)
        writeIORef execThread $ Just execT

-- if there's a threadId in @threadIORef@, kill it and clean the reference
killThreadIfRunning :: IORef (Maybe ThreadId) -> IO ()
killThreadIfRunning threadIORef = do
  mThread <- readIORef threadIORef
  case mThread of
      Nothing -> return ()
      Just t -> do
          killThread t
          writeIORef threadIORef Nothing


------------------------------------------------------------------------------------------------------------------------------------------------
-- auxiliar functions related to displaying a rule after selecting it in the treestore----------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------

loadSelectedRule :: IORef (M.Map Int32 GraphState) -> Int32
                  -> IORef GraphState -> IORef GraphState -> IORef GraphState -> IORef GraphState -> IORef (G.Graph Info Info)
                  -> Gtk.DrawingArea -> Gtk.DrawingArea -> Gtk.DrawingArea
                  -> IO ()
loadSelectedRule
  statesMap rIndex
  ruleState lState rState lStateOrig kGraph
  lCanvas rCanvas ruleCanvas
  =
  do
    statesM <- readIORef statesMap
    let es = fromMaybe emptyState $ M.lookup rIndex statesM
        g = stateGetGraph es
        (l,k,r) = GMker.graphToRuleGraphs g
        (ngiM,egiM) = stateGetGI es
    lcontext <- Gtk.widgetGetPangoContext lCanvas
    rcontext <- Gtk.widgetGetPangoContext rCanvas
    -- split layouts into left and right layouts and change the labels to include the id on the mapping
    let lngiM = M.filterWithKey (\k a -> (G.NodeId k) `elem` (G.nodeIds l)) ngiM
        legiM = M.filterWithKey (\k a -> (G.EdgeId k) `elem` (G.edgeIds l)) egiM
        rngiM = M.filterWithKey (\k a -> (G.NodeId k) `elem` (G.nodeIds r)) ngiM
        regiM = M.filterWithKey (\k a -> (G.EdgeId k) `elem` (G.edgeIds r)) egiM
        -- show ids of elements that belongs to LHS
        nmap = M.fromList $ map (\k -> (k, "[" ++ (show . fromEnum $ k) ++ "]")) (G.nodeIds l)
        emap = M.fromList $ map (\k -> (k, "[" ++ (show . fromEnum $ k) ++ "]")) (G.edgeIds l)
    lst <- setInfoExtra (stateSetGraph l . stateSetGI (lngiM,legiM) $ es) (nmap,emap) lcontext
    rst <- setInfoExtra (stateSetGraph r . stateSetGI (rngiM,regiM) $ es) (nmap,emap) rcontext
        -- adjust positions of the elements of the diagraphs
    let (_,lgi) = adjustDiagrPosition (stateGetGraph lst, stateGetGI lst)
        (_,rgi) = adjustDiagrPosition (stateGetGraph rst, stateGetGI rst)
        (_,gi') = adjustDiagrPosition (g,(ngiM,egiM))
    writeIORef ruleState $ stateSetGI gi' es
    writeIORef lState $ stateSetGI lgi $ lst
    writeIORef rState $ stateSetGI rgi $ rst
    writeIORef lStateOrig $ stateSetGraph l . stateSetGI (lngiM,legiM) $ es
    writeIORef kGraph k

setMatchingElementsText :: IORef (M.Map Int32 (M.Map Int32 Match)) -> Int32 -> Int32
                        -> IORef GraphState -> Gtk.DrawingArea -> IORef ([G.NodeId], [G.EdgeId])
                        -> IO ()
setMatchingElementsText
  matchesMap rIndex mIndex
  hostState
  mainCanvas
  currentMatchedElements
  =
  do
    hostSt <- readIORef hostState
    matchesM <- readIORef matchesMap
    context <- Gtk.widgetGetPangoContext mainCanvas
    let matches = fromMaybe M.empty $ M.lookup rIndex matchesM
        match = M.lookup mIndex matches
        mappings = case match of
            Nothing -> (M.empty,M.empty)
            Just m ->  (nMapping',eMapping')
                    where
                        mapping = TGM.mapping m
                        nMapping = R.mapping . R.inverseRelation . GM.nodeRelation $ mapping
                        eMapping = R.mapping . R.inverseRelation . GM.edgeRelation $ mapping
                        nMapping' = M.map (\id -> "[" ++ (concat $ map (show . fromEnum) id) ++ "]" ) nMapping
                        eMapping' = M.map (\id -> "[" ++ (concat $ map (show . fromEnum) id) ++ "]" ) eMapping
    hostSt' <- setInfoExtra hostSt mappings context
    writeIORef hostState hostSt'
    writeIORef currentMatchedElements (M.keys (fst mappings), M.keys (snd mappings))



------------------------------------------------------------------------------------------------------------------------------------------------
-- auxiliar functions related with changing the treestore --------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------

-- auxiliar function to findmatches that populates the TreeStore with the matches found
-- this function should be executed inside a thread
populateTreeStoreWithMatches :: Gtk.TreeStore -> M.Map Int32 (M.Map Int32 Match) -> IO ()
populateTreeStoreWithMatches store matchesM = do
    let matches = M.toList matchesM
    forM_ matches $ \(rid,mM) -> do
        mk <- return $ M.keys mM
        numMatches <- return $ length mk
        matchesEntries <- forM (take 100 mk) $ \mid -> return ("match " ++ (show mid), mid, 2, rid, "")
        entries <- return $!  if numMatches > 100 then
                                let nextPageEntry = ("next " ++ (show $ min 100 (numMatches - 100) ) ++ " matches",0, 4, rid, "")
                                in  nextPageEntry:matchesEntries
                              else
                                matchesEntries
        comment <- return $ " -> " ++ (show numMatches) ++ case numMatches of
                                                            1 -> " match"
                                                            _ -> " matches" ++ (if numMatches > 100 then " (showing 1-100)" else "")
        Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
            ruleIter <- getRuleIter store rid
            case ruleIter of
                Nothing -> return ()
                Just iter -> do
                    realName <- Gtk.treeModelGetValue store iter 4 >>= fromGValue >>= return . fromMaybe "" :: IO String
                    newName <- return $ realName ++ comment
                    nngv <- toGValue (Just newName)
                    #set store iter [0] [nngv]
                    forM_ entries $ \entry -> do
                        entryIter <- Gtk.treeStoreAppend store (Just iter)
                        storeSetGraphEntry store entryIter entry
                    return ()
            return False
        return ()


showPreviousMatches :: Gtk.TreeStore -> Gtk.TreeIter -> IORef (M.Map Int32 (M.Map Int32 Match)) -> IO ()
showPreviousMatches store iter matchesMap =
  do
    ri <- Gtk.treeModelGetValue store iter 3 >>= fromGValue :: IO Int32
    matchesM <- readIORef matchesMap
    case M.lookup ri matchesM of
        Nothing -> return ()
        Just nM -> do
            offset <- Gtk.treeModelGetValue store iter 1 >>= fromGValue :: IO Int32
            let numMatches = M.size nM
                offset' = (fromIntegral offset) - 100
                matchesL = drop offset' $ M.toList nM
                comment = " -> " ++ (show numMatches) ++ " matches (showing " ++ (show $ offset'+1) ++ "-" ++ (show $ min numMatches (offset'+100)) ++ ")"
                newNextEntry = ( "next 100 matches", (fromIntegral offset'), 4, ri, "")
                newPreviousEntry = if offset' >= 100
                                    then Just ("previous 100 matches", (fromIntegral offset'), 5, ri, "")
                                    else Nothing

            (parentValid,ruleIter) <- Gtk.treeModelIterParent store iter
            (childValid,firstChildIter) <- Gtk.treeModelIterChildren store (Just ruleIter)
            treeStoreClearCurrrentLevel store firstChildIter

            -- set the comment of number of matches to the rule name
            realRuleName <- Gtk.treeModelGetValue store ruleIter 4 >>= fromGValue >>= return . fromMaybe "" :: IO String
            newNameGV <- toGValue $ Just (realRuleName ++ comment)
            #set store ruleIter [0] [newNameGV]

            -- add the "next matches" and "previous matches" entries
            updateTreeStore store newNextEntry
            case newPreviousEntry of
                Nothing    -> return ()
                Just entry -> updateTreeStore store entry
            forM_ (take 100 matchesL) $ \(mid,m) -> updateTreeStore store ("match " ++ (show mid), mid, 2, ri, "")

showNextMatches :: Gtk.TreeStore -> Gtk.TreeIter -> IORef (M.Map Int32 (M.Map Int32 Match)) -> IO ()
showNextMatches store iter matchesMap = do
    ri <- Gtk.treeModelGetValue store iter 3 >>= fromGValue :: IO Int32
    matchesM <- readIORef matchesMap
    case M.lookup ri matchesM of
        Nothing -> return ()
        Just nM -> do
            offset <- Gtk.treeModelGetValue store iter 1 >>= fromGValue :: IO Int32
            let numMatches = M.size nM
                offset' = 100+(fromIntegral offset)
                matchesL = drop offset' $ M.toList nM
                comment = " -> " ++ (show numMatches) ++ " matches (showing " ++ (show $ offset'+1) ++ "-" ++ (show $ min numMatches (offset'+100)) ++ ")"
                newNextEntry = if numMatches-offset' > 100
                                then Just ( "next " ++ (show $ min (numMatches-offset'-100) 100 ) ++ " matches", fromIntegral offset', 4, ri, "")
                                else Nothing
                newPreviousEntry = ("previous 100 matches", fromIntegral offset', 5, ri, "")

            (_,ruleIter) <- Gtk.treeModelIterParent store iter
            (_,firstChildIter) <- Gtk.treeModelIterChildren store (Just ruleIter)
            treeStoreClearCurrrentLevel store firstChildIter

            -- set the comment of number of matches to the rule name
            realRuleName <- Gtk.treeModelGetValue store ruleIter 4 >>= fromGValue >>= return . fromMaybe "" :: IO String
            newNameGV <- toGValue $ Just (realRuleName ++ comment)
            #set store ruleIter [0] [newNameGV]

            -- add the "next matches" and "previous matches" entries
            case newNextEntry of
                Nothing    -> return ()
                Just entry -> updateTreeStore store entry
            updateTreeStore store newPreviousEntry
            forM_ (take 100 matchesL) $ \(mid,m) -> updateTreeStore store ("match " ++ (show mid), mid, 2, ri, "")
