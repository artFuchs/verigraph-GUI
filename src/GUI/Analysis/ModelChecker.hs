{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module GUI.Analysis.ModelChecker (
  buildModelCheckerGUI
, breadthFirstSearchIO
) where

-- GTK modules
import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import qualified GI.Pango as P
import           Data.GI.Base
import           Graphics.Rendering.Cairo (Render)

-- modules needed for threads
import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Exception.Base
import qualified GI.GLib as GLib

-- Haskell structures
import           Control.Monad
import           Data.IORef
import           Data.Int
import           Data.Maybe
import qualified Data.IntMap as IntMap
import qualified Data.Text   as T
import qualified Data.Map    as M
import qualified Data.Set    as Set
import qualified Data.List   as List
import           Data.Char
import qualified Data.Time.Clock   as Time
import           Data.IntMap                       (IntMap)
import qualified Data.IntMap                       as IntMap


-- verigraph structures
import qualified  Abstract.Category                 as Cat
import qualified  Abstract.Rewriting.DPO            as DPO
import qualified  Abstract.Rewriting.DPO.StateSpace as SS
import qualified  Data.Graphs                       as G
import qualified  Data.TypedGraph.Morphism          as TGM
import qualified  Data.TypedGraph                   as TG
import            Rewriting.DPO.TypedGraph
import qualified  Logic.Ctl                         as Logic
import qualified  Logic.Model                       as Logic

-- GUI modules
import            GUI.Analysis.ModelChecker.StateSpace
import            GUI.Data.Info hiding (empty)
import qualified  GUI.Data.Info as Info
import            GUI.Data.Nac
import            GUI.Data.DiaGraph
import            GUI.Data.GraphState
import            GUI.Data.GraphicalInfo
import            GUI.Dialogs
import            GUI.Executor
import            GUI.Helper.BasicCanvasCallbacks
import            GUI.Helper.GraphicalInfo
import            GUI.Helper.GrammarMaker
import            GUI.Helper.FilePath
import            GUI.Helper.Util
import            GUI.Render.Render
import            GUI.Render.GraphDraw


-- User Interface -----------------------------------------------------------------------------------------

{- | Builds the GUI for the Model Checker module, setting the callbacks for the widgets
     This method returns
     * The Gtk.Box that is the superior level of the GUI
     * The Canvas where the state space is drawn
     * The reference to the GraphState that is drawn on the canvas
-}
buildModelCheckerGUI :: Gtk.Window
                   -> Gtk.TreeStore
                   -> Gtk.MenuItem
                   -> IORef (Maybe Gtk.DrawingArea)
                   -> IORef (Maybe (IORef GraphState))
                   -> IORef (M.Map Int32 GraphState)
                   -> IORef (M.Map Int32 (DiaGraph, MergeMapping))
                   -> IORef (G.Graph Info Info)
                   -> IO (Gtk.Box, Gtk.DrawingArea, IORef GraphState)
buildModelCheckerGUI window store genStateSpaceItem focusedCanvas focusedStateIORef graphStatesIORef nacsInfoIORef typeGraph = do

  -- build GUI
  builder <- new Gtk.Builder []
  resourcesFolder <- getResourcesFolder
  Gtk.builderAddFromFile builder $ T.pack (resourcesFolder ++ "stateSpace.glade")

  mainBox <- Gtk.builderGetObject builder "main_box" >>= unsafeCastTo Gtk.Box . fromJust
  generateBtn <- Gtk.builderGetObject builder "generate_button" >>= unsafeCastTo Gtk.Button . fromJust
  stopBtn <- Gtk.builderGetObject builder "stop_button" >>= unsafeCastTo Gtk.Button . fromJust
  depthSpinBtn <- Gtk.builderGetObject builder "depth_spin_button" >>= unsafeCastTo Gtk.SpinButton . fromJust

  formulaEntry <- Gtk.builderGetObject builder "formula_entry" >>= unsafeCastTo Gtk.Entry . fromJust
  formulaCheckBtn <- Gtk.builderGetObject builder "check_formula_btn" >>= unsafeCastTo Gtk.Button . fromJust

  canvas <- Gtk.builderGetObject builder "canvas" >>= unsafeCastTo Gtk.DrawingArea . fromJust
  Gtk.widgetSetEvents canvas [toEnum $ fromEnum Gdk.EventMaskAllEventsMask - fromEnum Gdk.EventMaskSmoothScrollMask]

  auxCanvas <- Gtk.builderGetObject builder "canvas_aux" >>= unsafeCastTo Gtk.DrawingArea . fromJust
  Gtk.widgetSetEvents auxCanvas [toEnum $ fromEnum Gdk.EventMaskAllEventsMask - fromEnum Gdk.EventMaskSmoothScrollMask]

  paned <-  Gtk.builderGetObject builder "paned" >>= unsafeCastTo Gtk.Paned . fromJust
  closePos <- get paned #maxPosition
  Gtk.panedSetPosition paned closePos

  statusSpinner <- Gtk.builderGetObject builder "status_spinner" >>= unsafeCastTo Gtk.Spinner . fromJust
  statusLabel <- Gtk.builderGetObject builder "status_label" >>= unsafeCastTo Gtk.Label . fromJust
  Gtk.spinnerStop statusSpinner
  Gtk.labelSetText statusLabel ""

  -- set IORefs for generating the stateSpace and execute the model check
  ssGraphState <- newIORef emptyState
  modelIORef <- newIORef Nothing
  execThread <- newIORef Nothing -- thread to generate state space
  constructThread <- newIORef Nothing -- thread to show state space
  goodStatesIORef <- newIORef Nothing
  -- IORefs for displaying the graphState of each state
  statesGSs <- newIORef IntMap.empty
  currentState <- newIORef emptyState
  completedGen <- newIORef False
  -- set the MVars
  constructEndedMVar <- newEmptyMVar
  timeMVar <- newEmptyMVar

  -- callback bindings ------------------------------------------------------------------------
  let widgets = (window, canvas, store, depthSpinBtn, statusLabel, statusSpinner)
      resIORefs = (ssGraphState, modelIORef, goodStatesIORef, statesGSs)
      gsIORefs = (graphStatesIORef, nacsInfoIORef)
      threadIORefs = (execThread, constructThread)
      mvars = (constructEndedMVar, timeMVar)

  on generateBtn #pressed $ Gtk.menuItemActivate genStateSpaceItem
  on genStateSpaceItem #activate $ genStateSpaceItemActivate widgets resIORefs gsIORefs threadIORefs mvars completedGen

  -- stop generation of state space
  on stopBtn #pressed $ stopBtnCallback execThread constructThread constructEndedMVar


  on formulaCheckBtn #pressed $ checkFormula window formulaEntry statusLabel modelIORef goodStatesIORef
  on formulaEntry #keyPressEvent $ formulaEntryKeyPressedCallback window formulaEntry statusLabel modelIORef goodStatesIORef


  -- canvas - to draw the state space graph
  setMainCanvasCallbacks canvas ssGraphState focusedCanvas focusedStateIORef statesGSs auxCanvas currentState goodStatesIORef
  -- auxCanvas - to draw the selected state
  setBasicCanvasCallbacks auxCanvas currentState typeGraph (Just drawHostGraph) focusedCanvas focusedStateIORef

  return (mainBox, canvas, ssGraphState)


-- callbacks ------------------------------------------------------------------------
genStateSpaceItemActivate :: (Gtk.Window, Gtk.DrawingArea, Gtk.TreeStore, Gtk.SpinButton, Gtk.Label, Gtk.Spinner)
                          -> (IORef GraphState, IORef (Maybe (Logic.KripkeStructure String)), IORef (Maybe [G.NodeId]), IORef (IntMap GraphState))
                          -> (IORef (M.Map Int32 GraphState), IORef (M.Map Int32 NacInfo))
                          -> (IORef (Maybe ThreadId), IORef (Maybe ThreadId))
                          -> (MVar Bool, MVar Time.UTCTime)
                          -> IORef Bool
                          -> IO ()
genStateSpaceItemActivate (window, canvas, store, depthSpinBtn, statusLabel, statusSpinner)
                          (ssGraphState, modelIORef, goodStatesIORef, statesGSs)
                          (graphStatesIORef, nacsInfoIORef)
                          (execThread, constructThread)
                          (constructEndedMVar, timeMVar)
                          completedGen =
  do
    maybeT <- readIORef execThread
    case maybeT of
      Just t -> return ()
      Nothing -> do
        eGG <- convertGrammar store graphStatesIORef nacsInfoIORef
        case eGG of
          Left msg -> showError window (T.pack msg)
          Right grammar -> do
            ssIORef <- newIORef Nothing
            maxStates <- Gtk.spinButtonGetValueAsInt depthSpinBtn >>= return . fromIntegral
            context <- Gtk.widgetGetPangoContext canvas
            ruleIndexesMap <- getRuleIndexesMap store
            gStatesMap <- readIORef graphStatesIORef
            writeIORef goodStatesIORef Nothing
            writeIORef ssGraphState emptyState
            writeIORef completedGen False
            execT <- forkFinally
                        (generateSSThread statusSpinner statusLabel canvas context timeMVar grammar maxStates ssIORef ssGraphState modelIORef constructThread constructEndedMVar ruleIndexesMap statesGSs gStatesMap completedGen)
                        (generateSSThreadEnd statusSpinner statusLabel execThread timeMVar constructThread constructEndedMVar completedGen)
            writeIORef execThread (Just execT)


stopBtnCallback :: IORef (Maybe ThreadId) -> IORef (Maybe ThreadId) -> MVar Bool -> IO ()
stopBtnCallback execThread constructThread constructEndedMVar = do
  killThreadIfRunning constructThread
  killThreadIfRunning execThread
  _ <- tryTakeMVar constructEndedMVar
  putMVar constructEndedMVar False
  writeIORef constructThread Nothing
  writeIORef execThread Nothing


formulaEntryKeyPressedCallback :: Gtk.Window -> Gtk.Entry -> Gtk.Label
             -> IORef (Maybe (Logic.KripkeStructure String)) -> IORef (Maybe [G.NodeId])
             -> Gdk.EventKey
             -> IO Bool
formulaEntryKeyPressedCallback window formulaEntry statusLabel modelIORef goodStatesIORef eventKey = do
  k <- get eventKey #keyval >>= return . chr . fromIntegral
  case k of
    -- Return or Enter (Numpad) -> then check formula
    '\65293' -> checkFormula window formulaEntry statusLabel modelIORef goodStatesIORef
    '\65421' -> checkFormula window formulaEntry statusLabel modelIORef goodStatesIORef
    -- Backspace -> clear goodStatesIORef
    '\65288' -> do
        t <- Gtk.entryGetTextLength formulaEntry >>= return . fromIntegral
        if t == 0 then do
          writeIORef goodStatesIORef Nothing
          Gtk.labelSetText statusLabel ""
        else return ()
    _ -> return ()
  return False


checkFormula :: Gtk.Window -> Gtk.Entry -> Gtk.Label
             -> IORef (Maybe (Logic.KripkeStructure String)) -> IORef (Maybe [G.NodeId])
             -> IO ()
checkFormula window formulaEntry statusLabel modelIORef goodStatesIORef =
  do
    exprTxt <- Gtk.entryGetText formulaEntry
    exprStr <- return $ T.unpack exprTxt
    case Logic.parseExpr "" exprStr of
      Left err -> showError window $ T.pack ("Invalid CTL formula:\n" ++ (show err))
      Right expr -> do
        maybeModel <- readIORef modelIORef
        case maybeModel of
          Nothing -> showError window $ "Must Generate State Space before checking a formula"
          Just model -> do
            startTime <- Time.getCurrentTime
            modelCheck model expr goodStatesIORef
            endTime <- Time.getCurrentTime
            gstates <- readIORef goodStatesIORef
            let diff = Time.diffUTCTime endTime startTime
            let text = if (G.NodeId 0) `elem` fromMaybe [] gstates then
                        T.pack ("The formula \"" ++ exprStr ++ "\" holds for the initial state. Formula checked in " ++ (show diff) ++ "seconds" )
                       else
                        T.pack ("The formula \"" ++ exprStr ++ "\" doesn't hold for the inital state. Formula checked in " ++ (show diff) ++ "seconds" )
            Gtk.labelSetText statusLabel text



setMainCanvasCallbacks :: Gtk.DrawingArea -> IORef GraphState
                       -> IORef (Maybe Gtk.DrawingArea) -> IORef (Maybe (IORef GraphState))
                       -> IORef (IntMap GraphState) -> Gtk.DrawingArea -> IORef GraphState
                       -> IORef (Maybe [G.NodeId])
                       -> IO ()
setMainCanvasCallbacks canvas ssGraphState focusedCanvas focusedStateIORef statesGSs auxCanvas currentState goodStatesIORef = do
  emptyG <- newIORef G.empty
  (_,squareSelection) <- setBasicCanvasCallbacks canvas ssGraphState emptyG Nothing focusedCanvas focusedStateIORef
  on canvas #buttonPressEvent $ \eventButton -> do
    ssSt <- readIORef ssGraphState
    let (ns,_) = stateGetSelected ssSt
    case ns of
      (n:_) -> do
        stMap <- readIORef statesGSs
        let index = (fromEnum n)
        let mst = IntMap.lookup index stMap
        case mst of
          Just st -> do
            writeIORef currentState st
            Gtk.widgetQueueDraw auxCanvas
          _ -> return ()
      _ -> return ()
    return False

  on canvas #draw $ \context -> do
    aloc <- Gtk.widgetGetAllocation canvas
    w <- Gdk.getRectangleWidth aloc >>= return . fromIntegral :: IO Double
    h <- Gdk.getRectangleHeight aloc >>= return . fromIntegral :: IO Double
    ss <- readIORef ssGraphState
    sq <- readIORef squareSelection
    gsts <- readIORef goodStatesIORef
    renderWithContext context   $ drawStateSpace ss sq gsts (w,h)
    return False
  return ()









-- | Function that the state space generation thread executes
-- this thread generates a subthread in which the state space visualization is generated while the exploration progresses
generateSSThread :: Gtk.Spinner -> Gtk.Label -> Gtk.DrawingArea -> P.Context
                 -> MVar Time.UTCTime
                 -> DPO.Grammar (TGM.TypedGraphMorphism Info Info) -> Int
                 -> IORef (Maybe (Space Info Info))
                 -> IORef GraphState -> IORef (Maybe (Logic.KripkeStructure String))
                 -> IORef (Maybe ThreadId) -> MVar Bool
                 -> M.Map String Int32 -> IORef (IntMap GraphState) -> M.Map Int32 GraphState
                 -> IORef Bool
                 -> IO ()
generateSSThread statusSpinner statusLabel canvas context timeMVar grammar statesNum ssIORef ssGraphState modelIORef constructThread constructEndedMVar ruleIndexesMap statesGSs graphStatesMap completedGen = do
  -- get current time to compare and indicate the duration of the generation
  startTime <- Time.getCurrentTime
  putMVar timeMVar startTime

  -- indicate that the generation started
  Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
      Gtk.spinnerStart statusSpinner
      Gtk.labelSetText statusLabel "generating state space"
      return False

  --
  let initialGraph = DPO.start grammar
      mconf = (DPO.MorphismsConfig Cat.monic) :: DPO.MorphismsConfig (TGM.TypedGraphMorphism Info Info)

  -- start thread to indicate the current status of the generation
  ssMVar <- newEmptyMVar
  indicateThread <- forkIO $ showStateSpace statusLabel ssMVar initialGraph canvas context ssGraphState modelIORef constructEndedMVar
  writeIORef constructThread (Just indicateThread)

  -- generate state
  (_,_,matchesMap, completed) <- exploreStateSpace mconf statesNum grammar initialGraph (Just ssMVar)
  -- generate states visualization
  writeIORef statesGSs $ generateStatesGraphState graphStatesMap matchesMap ruleIndexesMap
  writeIORef completedGen completed

  return ()


-- | function that executes after the the state space generation ends
generateSSThreadEnd :: Gtk.Spinner -> Gtk.Label
                    -> IORef (Maybe ThreadId)
                    -> MVar Time.UTCTime
                    -> IORef (Maybe ThreadId)
                    -> MVar Bool
                    -> IORef Bool
                    -> Either SomeException ()  -- this is just ignored
                    -> IO ()
generateSSThreadEnd statusSpinner statusLabel execThread timeMVar constructThread constructEndedMVar completedGen e = do
  ended <- takeMVar constructEndedMVar
  completed <- readIORef completedGen

  killThreadIfRunning constructThread

  -- get current time to compare and indicate the duration of the generation
  endTime <- Time.getCurrentTime
  startTime <- tryTakeMVar timeMVar
  diff <- case startTime of
            Nothing -> return (-1)
            Just time -> return $ Time.diffUTCTime endTime time

  -- indicate that the generation ended
  Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
    Gtk.spinnerStop statusSpinner
    let text =  case (ended, completed) of
                  (True, True) -> T.pack $ "Generation completed in " ++ (show diff)
                  (True, False) -> T.pack $ "Incomplete generation. Time elapsed: " ++ (show diff)
                  _ -> T.pack $ "Generation interrupted. Time elapsed: " ++ (show diff)
    Gtk.labelSetText statusLabel text
    return False

  writeIORef execThread Nothing



-- | generates the visualization of the state space
showStateSpace :: Gtk.Label -> MVar (Space Info Info, Bool) -> TG.TypedGraph Info Info
                     -> Gtk.DrawingArea -> P.Context
                     -> IORef GraphState -> IORef (Maybe (Logic.KripkeStructure String))
                     -> MVar Bool
                     -> IO ()
showStateSpace statusLabel ssMVar initialGraph canvas context ssGraphState modelIORef constructEndedMVar = do
  -- wait till there is something
  (stateSpace, lastIteration) <- takeMVar ssMVar

  -- indicate the number of states in the current space
  let s = IntMap.size $ SS.states stateSpace
  Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
    Gtk.labelSetText statusLabel (T.pack ("generating state space ( " ++ (show s) ++ " states)"))
    return False

  -- build the model to make logical verifications and
  let model = SS.toKripkeStructure stateSpace
  writeIORef modelIORef (Just model)

  -- generate the graphState to show the space state
  oldST <- readIORef ssGraphState
  let st = generateStateSpaceVisualization stateSpace oldST
      (ngi,egi) = stateGetGI st
      g = stateGetGraph st
  ngi' <- updateNodesGiDims ngi g context
  st' <- return $ stateSetGI (ngi',egi) st

  -- update canvas
  Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
    writeIORef ssGraphState st'
    Gtk.widgetQueueDraw canvas
    return False

  if lastIteration then
    putMVar constructEndedMVar True
  else
    showStateSpace statusLabel ssMVar initialGraph canvas context ssGraphState modelIORef constructEndedMVar




-- | Given a logic model and a expression to parse, verify wich states are good and wich are bad
-- adapted from Verigraph CLI/ModelChecker.hs
modelCheck :: Logic.KripkeStructure String -> Logic.Expr -> IORef (Maybe [G.NodeId]) -> IO ()
modelCheck model expr goodStatesIORef =
  let
    allGoodStates = Logic.satisfyExpr' model expr
  in do
    writeIORef goodStatesIORef $ Just (map G.NodeId allGoodStates)


-- | draw state space graph -------------------------------------------------------------
drawStateSpace :: GraphState -> Maybe (Double,Double,Double,Double) -> Maybe [G.NodeId] -> (Double,Double) -> Render ()
drawStateSpace state sq maybeGoodStates alloc = do
  drawGraph state sq nodeColors M.empty M.empty M.empty (Just alloc)
  where
    g = stateGetGraph state
    allGoodStates = fromMaybe [] maybeGoodStates
    nodeColors = case maybeGoodStates of
                    Nothing            -> M.empty
                    Just allGoodStates ->
                        let (goodStates, badStates) = List.partition (`List.elem` allGoodStates) (G.nodeIds g)
                        in  (M.fromList $ map (\n -> (n,(0,1,0))) goodStates)
                            `M.union`
                            (M.fromList $ map (\n -> (n,(1,0,0))) badStates)

-- | for each of the generated states, generate a GraphState to display it
generateStatesGraphState :: M.Map Int32 GraphState -> IntMap (Int, Match Info Info, String, TypedGraphRule Info Info) -> M.Map String Int32 -> IntMap GraphState
generateStatesGraphState graphStatesMap matchesMap ruleIndexesMap =
  generateStatesGraphState' graphStatesMap matchesMap ruleIndexesMap genStates indexesToGenerate
  where
    initialSt = fromJust $ M.lookup 1 graphStatesMap
    genStates = IntMap.singleton 0 initialSt
    indexesToGenerate = IntMap.keys $ IntMap.filter (\(i,_,_,_) -> i == 0) matchesMap

-- auxiliar function to generateStatesGraphState
generateStatesGraphState' :: M.Map Int32 GraphState -> IntMap (Int, Match Info Info, String, TypedGraphRule Info Info) -> M.Map String Int32 -> IntMap GraphState -> [Int] -> IntMap GraphState
generateStatesGraphState' graphStatesMap matchesMap ruleIndexesMap genStates [] = genStates
generateStatesGraphState' graphStatesMap matchesMap ruleIndexesMap genStates (index:indexesToGenerate) =
  generateStatesGraphState' graphStatesMap matchesMap ruleIndexesMap genStates' indexesToGenerate'
  where
    indexesToGenerate' = indexesToGenerate ++ (IntMap.keys $ IntMap.filter (\(i,_,_,_) -> i == index) matchesMap)
    genStates' =
      fromMaybe genStates $ do
        matchInfo <- IntMap.lookup index matchesMap
        gst <- generateStateGS graphStatesMap matchInfo ruleIndexesMap genStates index
        return (IntMap.insert index gst genStates)

-- auxiliar function to generateStatesGraphState
-- generates the state space of one state
generateStateGS :: M.Map Int32 GraphState -> (Int, Match Info Info, String, TypedGraphRule Info Info) -> M.Map String Int32 -> IntMap GraphState -> Int -> Maybe GraphState
generateStateGS graphStatesMap (i,m,n,p) ruleIndexesMap genStates index =
  do
    ost <- IntMap.lookup i genStates
    ri <- M.lookup n ruleIndexesMap
    return (applyMatch ost graphStatesMap ri p m)


-- Function to get a Map that translates rule names to rule indexes
getRuleIndexesMap :: Gtk.TreeStore -> IO (M.Map String Int32)
getRuleIndexesMap store = do
  (valid, iter) <- Gtk.treeModelGetIterFromString store "2:0"
  if not valid
    then return M.empty
    else do
      l <- getRuleNamesAndIndexes store iter
      return (M.fromList l)

-- Auxiliar function to get releIndexesMap
getRuleNamesAndIndexes :: Gtk.TreeStore -> Gtk.TreeIter -> IO [(String, Int32)]
getRuleNamesAndIndexes store iter = do
  name <- Gtk.treeModelGetValue store iter 0 >>= fromGValue >>= return . fromJust :: IO String
  index <- Gtk.treeModelGetValue store iter 2 >>= fromGValue :: IO Int32
  continue <- Gtk.treeModelIterNext store iter
  if continue then do
    ls <- getRuleNamesAndIndexes store iter
    return $ (name,index):ls
  else
    return [(name,index)]
