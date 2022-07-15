{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

module GUI.Editor.Helper.Callbacks where

import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import qualified GI.Cairo.Structs as Cairo
import qualified GI.Pango as P
import           Data.GI.Base

import Control.Monad

import           Data.IORef
import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import           Data.Int
import           Data.Graphs hiding (empty)
import qualified Data.Graphs as G
import qualified Data.Text as T
import           Data.Char
import           Data.List
import           Data.Ord

import           GUI.Data.Diagram
import           GUI.Data.GraphState
import           GUI.Data.GraphicalInfo
import           GUI.Data.Info hiding (empty)
import qualified GUI.Data.Info as I
import           GUI.Data.Nac
import           GUI.Helper.BasicCanvasCallbacks
import           GUI.Helper.GraphicalInfo
import           GUI.Helper.GraphValidation
import           GUI.Render.Render
import           GUI.Render.GraphDraw

import           GUI.Editor.Helper.CreateElements
import           GUI.Editor.Helper.UndoRedo
import           GUI.Editor.Helper.TreeStore
import           GUI.Editor.Helper.TypeInfer
import           GUI.Editor.UI.UpdateInspector


type StoreIORefs      = ( IORef (M.Map Int32 GraphState), IORef [Int32], IORef Int32, IORef Int32 )
type ChangesIORefs    = ( IORef Bool, IORef [Bool], IORef (M.Map Int32 Diagram))
type NacIORefs        = ( IORef (M.Map Int32 (Diagram, MergeMapping)), IORef (Maybe MergeMapping))
type SelectableTypesIORefs = (IORef (M.Map String (NodeGI, Int32)), IORef (M.Map String (M.Map (String, String) EdgeGI, Int32)), IORef (Maybe String), IORef (Maybe String))


--------------------------------------------------------------------------------
-- Canvas ----------------------------------------------------------------------
--------------------------------------------------------------------------------

drawGraphByType :: Gtk.DrawingArea
                -> IORef GraphState
                -> IORef (Graph Info Info)
                -> IORef (Maybe (Double,Double,Double,Double) )
                -> IORef Int32
                -> IORef (Maybe MergeMapping)
                -> Cairo.Context
                -> IO Bool
drawGraphByType canvas currentState typeGraph squareSelection currentGraphType mergeMapping context =
  do
    es <- readIORef currentState
    sq <- readIORef squareSelection
    t <- readIORef currentGraphType
    aloc <- Gtk.widgetGetAllocation canvas
    w <- Gdk.getRectangleWidth aloc >>= return . fromIntegral :: IO Double
    h <- Gdk.getRectangleHeight aloc >>= return . fromIntegral :: IO Double
    case t of
      1 -> renderWithContext context $ drawTypeGraph es sq (Just (w,h))
      2 -> do
        tg <- readIORef typeGraph
        renderWithContext context $ drawHostGraph es sq tg (Just (w,h))
      3 -> do
        tg <- readIORef typeGraph
        renderWithContext context $ drawRuleGraph es sq tg (Just (w,h))
      4 -> do
        tg <- readIORef typeGraph
        mm <- readIORef mergeMapping >>= return . Maybe.fromMaybe (M.empty, M.empty)
        renderWithContext context $ drawNACGraph es sq tg mm (Just (w,h))
      _ -> return ()
    return False


canvasButtonPressedCallback
  canvas window nameEntry autoLabelNCheckBtn autoLabelECheckBtn
  nodeTypeCBox edgeTypeCBox
  store storeIORefs@(graphStates,currentPath,currentGraph,currentGraphType)
  currentState typeGraph nacInfoMap mergeMapping
  changesIORefs undoStack redoStack
  currentNodeType possibleNodeTypes currentEdgeType possibleEdgeTypes possibleSelectableEdgeTypes
  currentShape currentStyle currentC currentLC
  oldPoint squareSelection
  eventButton =
  do
    basicCanvasButtonPressedCallback currentState oldPoint squareSelection canvas eventButton
    b <- get eventButton #button
    click <- get eventButton #type
    ms <- get eventButton #state

    st <- readIORef currentState
    mergeM <- readIORef mergeMapping

    let doubleLeftClick = (b == 1) && (click == Gdk.EventType2buttonPress)
    let rightButton = (b == 3) && not (Gdk.ModifierTypeControlMask `elem` ms)

    created <- case (doubleLeftClick, rightButton) of
        -- left button with double click -> rename element
        (True,False) -> do
          case stateGetSelected st of
            ([],[]) -> return ()
            _ -> Gtk.widgetGrabFocus nameEntry
          return False
        -- right button -> create a node or edge
        (False,True) -> createNodesOrEdgesCallback canvas nameEntry autoLabelNCheckBtn autoLabelECheckBtn
                               currentState currentGraph currentGraphType typeGraph mergeMapping
                               currentNodeType possibleNodeTypes currentEdgeType possibleEdgeTypes
                               currentShape currentStyle currentC currentLC
                               eventButton
        _ -> return False
    -- if a node or edges were created then update the UI to indicate changes and possibly invalid flags
    if created then
      do
        updateByType
            currentGraphType
            store graphStates nacInfoMap mergeMapping currentState typeGraph currentGraph currentPath
            possibleNodeTypes possibleEdgeTypes possibleSelectableEdgeTypes
            nodeTypeCBox edgeTypeCBox
        indicateChanges window store storeIORefs changesIORefs undoStack redoStack mergeM st
    else
      return ()

    return True

indicateChangesWhenMovingElements :: Gtk.Window -> Gtk.TreeStore
                                  -> IORef Bool ->  IORef GraphState -> IORef (Maybe MergeMapping)
                                  -> IORef (M.Map Int32 ChangeStack) -> IORef (M.Map Int32 ChangeStack)
                                  -> StoreIORefs -> ChangesIORefs -> Gdk.EventMotion
                                  -> IO Bool
indicateChangesWhenMovingElements window store movingGI currentState mergeMapping undoStack redoStack
                                  storeIORefs changesIORefs eventMotion =
  do
    ms <- get eventMotion #state
    es <- readIORef currentState
    mv <- readIORef movingGI
    let leftButton = Gdk.ModifierTypeButton1Mask `elem` ms
        (sNodes, sEdges) = stateGetSelected es
    if leftButton && ((length sNodes) + (length sEdges) > 0) && (not mv) then
      do
        writeIORef movingGI True
        mergeM <- readIORef mergeMapping
        indicateChanges window store storeIORefs changesIORefs undoStack redoStack mergeM es
    else
      return ()

    return True

canvasButtonReleasedCallback
  canvas
  currentGraphType currentState typeGraph mergeMapping
  selectableTypesIORefs@(possibleNodeTypes, possibleSelectableEdgeTypes, currentNodeType, currentEdgeType) possibleEdgeTypes edgeTypeCBox
  squareSelection movingGI
  currentC currentLC
  typeInspWidgets hostInspWidgets ruleInspWidgets nacInspWidgets
  (nodeTypeBox, edgeTypeBox)
  eventButton =
    do
      -- select elements that are inside squareSelection
      basicCanvasButtonReleasedCallback currentState squareSelection canvas eventButton
      -- if there are elements selected, then update the inspector and the edge type selection comboBox
      b <- get eventButton #button
      gType <- readIORef currentGraphType
      tg <- readIORef typeGraph
      es <- readIORef currentState
      case b of
        1 -> writeIORef movingGI False
        _ -> return ()

      if gType > 1 then
        changeEdgeTypeCBoxByContext possibleEdgeTypes possibleSelectableEdgeTypes edgeTypeCBox currentState tg
      else
        return ()

      pSET <- readIORef possibleSelectableEdgeTypes

      updateInspector currentGraphType currentState  mergeMapping selectableTypesIORefs
                      currentC currentLC
                      typeInspWidgets hostInspWidgets ruleInspWidgets nacInspWidgets
                      (nodeTypeBox, edgeTypeBox)
      return True

canvasKeyPressCallback :: Gtk.Entry -> Gtk.MenuItem -> Gdk.EventKey -> IO Bool
canvasKeyPressCallback nameEntry del eventKey = do
    k <- get eventKey #keyval >>= return . chr . fromIntegral
    ms <- get eventKey #state
    case (Gdk.ModifierTypeControlMask `elem` ms, Gdk.ModifierTypeShiftMask `elem` ms, toLower k) of
      -- F2 - rename selection
      (False,False,'\65471') -> Gtk.widgetGrabFocus nameEntry
      -- 'delete' while the focus is on canvas - delete elements
      (False,False,'\65535') -> Gtk.menuItemActivate del
      _ -> return ()
    return True

--------------------------------------------------------------------------------
-- Inspector Pane --------------------------------------------------------------
--------------------------------------------------------------------------------

nameEntryKeyPressedCallback ::
  Gtk.DrawingArea -> Gtk.Window -> Gtk.Entry ->
  Gtk.ComboBoxText -> Gtk.ComboBoxText ->
  Gtk.TreeStore -> StoreIORefs ->
  IORef GraphState -> IORef (Graph Info Info) -> IORef (M.Map Int32 NacInfo) -> IORef (Maybe MergeMapping) ->
  ChangesIORefs -> IORef (M.Map Int32 ChangeStack) -> IORef (M.Map Int32 ChangeStack) ->
  SelectableTypesIORefs -> IORef (M.Map String (M.Map (String, String) EdgeGI, Int32)) ->
  IORef (Double,Double,Double) -> IORef (Double,Double,Double) ->
  (Gtk.Entry, Gtk.Box, Gtk.ColorButton, Gtk.ColorButton, Gtk.Frame, [Gtk.RadioButton], Gtk.Frame, [Gtk.RadioButton]) ->
  (Gtk.Entry, Gtk.ComboBoxText, Gtk.ComboBoxText) ->
  (Gtk.Entry, Gtk.ComboBoxText, Gtk.ComboBoxText, Gtk.ComboBoxText) ->
  (Gtk.Entry, Gtk.ComboBoxText, Gtk.ComboBoxText, Gtk.Button, Gtk.Button) ->
  Gtk.Box -> Gtk.Box ->
  Gdk.EventKey ->
  IO Bool

nameEntryKeyPressedCallback
  canvas window nameEntry
  nodeTypeCBox edgeTypeCBox
  store storeIORefs@(graphStates,currentPath,currentGraph,currentGraphType)
  currentState typeGraph nacInfoMap mergeMapping
  changesIORefs undoStack redoStack
  selectableTypesIORefs@(possibleNodeTypes, possibleSelectableEdgeTypes, _, _) possibleEdgeTypes
  currentC currentLC
  typeInspWidgets hostInspWidgets ruleInspWidgets nacInspWidgets
  nodeTypeBox edgeTypeBox
  eventKey =
    do
      k <- get eventKey #keyval >>= return . chr . fromIntegral
      --if it's Return or Enter (Numpad), change focus to the canvas
      case k of
         '\65293' -> rename
         '\65421' -> rename
         '\65289' -> rename
         _       -> return ()
      return False
    where
      rename =
        do
          es <- readIORef currentState
          mergeM <- readIORef mergeMapping
          renameSelectedCallback nameEntry canvas currentState possibleEdgeTypes typeGraph
          indicateChanges window store storeIORefs changesIORefs undoStack redoStack mergeM es
          updateInspector currentGraphType currentState  mergeMapping selectableTypesIORefs
                          currentC currentLC
                          typeInspWidgets hostInspWidgets ruleInspWidgets nacInspWidgets
                          (nodeTypeBox, edgeTypeBox)
          updateByType
              currentGraphType
              store graphStates nacInfoMap mergeMapping currentState typeGraph currentGraph currentPath
              possibleNodeTypes possibleEdgeTypes possibleSelectableEdgeTypes
              nodeTypeCBox edgeTypeCBox


renameSelectedCallback :: Gtk.Entry -> Gtk.DrawingArea
               -> IORef GraphState -> IORef (M.Map String (M.Map (String, String) EdgeGI, Int32)) -> IORef (Graph Info Info)
               -> IO ()
renameSelectedCallback nameEntry canvas currentState possibleEdgeTypes typeGraph =
  do
    es <- readIORef currentState
    name <- Gtk.entryGetText nameEntry >>= return . T.unpack
    context <- Gtk.widgetGetPangoContext canvas
    es' <- renameSelected es name context

    -- infere the types of edge(s) connected to the renamed node(s)
    typesE <- readIORef possibleEdgeTypes
    tg <- readIORef typeGraph
    let
      typesE' = M.map fst typesE
      es'' = infereEdgesTypesAfterNodeChange es' tg typesE'

    -- show changes
    writeIORef currentState es''
    Gtk.widgetQueueDraw canvas

-- update the typeGraph and selectable element types according to the current graph type
updateByType
  currentGraphType
  store graphStates nacInfoMap mergeMapping currentState typeGraph currentGraph currentPath
  possibleNodeTypes possibleEdgeTypes possibleSelectableEdgeTypes
  nodeTypeCBox edgeTypeCBox =
  do
    gt <- readIORef currentGraphType
    case gt of
      0 -> return ()
      1 -> updateTG currentState typeGraph possibleNodeTypes possibleEdgeTypes possibleSelectableEdgeTypes graphStates nodeTypeCBox edgeTypeCBox store
      _ -> do
        if gt == 4
          then updateNacInfo nacInfoMap currentGraph mergeMapping currentState
          else return ()
        setCurrentValidFlag store currentState typeGraph currentPath


-- indicate if the was changes in the project and add 'oldSt' to the undoStack
indicateChanges :: Gtk.Window -> Gtk.TreeStore -> StoreIORefs -> ChangesIORefs
                -> IORef (M.Map Int32 ChangeStack) -> IORef (M.Map Int32 ChangeStack) -> Maybe MergeMapping -> GraphState -> IO ()
indicateChanges window store (_,currentPath,currentGraph,currentGraphType) (changedProject, changedGraph, _)
                undoStack redoStack oldMergeM oldSt =
  do
    setChangeFlags window store changedProject changedGraph currentPath currentGraph True
    stackUndo undoStack redoStack currentGraph oldSt oldMergeM




-- update IORefs that store NACs structs
updateNacInfo :: IORef (M.Map Int32 NacInfo) -> IORef Int32 -> IORef (Maybe MergeMapping) -> IORef GraphState -> IO ()
updateNacInfo nacInfoMap currentGraph mergeMapping currentState = do
      maybeMergeM <- readIORef mergeMapping
      case maybeMergeM of
        Nothing -> return ()
        Just mergeM -> do
            st <- readIORef currentState
            let g = stateGetGraph st
                gi = stateGetGI st
                nacInfo = extractNac g gi mergeM
            index <- readIORef currentGraph
            modifyIORef nacInfoMap $ M.insert index nacInfo



-- update the active typegraph if the corresponding diagraph is valid, set it as empty if not
updateTG :: IORef GraphState
          -> IORef (Graph Info Info)
          -> IORef (M.Map String (NodeGI, Int32))
          -> IORef (M.Map String (M.Map (String, String) EdgeGI, Int32))
          -> IORef (M.Map String (M.Map (String, String) EdgeGI, Int32))
          -> IORef (M.Map Int32 GraphState)
          -> Gtk.ComboBoxText
          -> Gtk.ComboBoxText
          -> Gtk.TreeStore
          -> IO ()
updateTG currentState typeGraph possibleNodeTypes possibleEdgeTypes possibleSelectableEdgeTypes graphStates nodeTypeCBox edgeTypeCBox store =
  do
    es <- readIORef currentState
    -- check if all edges and nodes have different names
    let g = stateGetGraph es
        (ngiM,egiM) = stateGetGI es
        nameConflictG = nameConflictGraph g
        diffNames = and (map nodeInfo (nodes nameConflictG)) &&
                    and (map edgeInfo (edges nameConflictG))

    -- if so, get the typegraph and possible names for nodes and edges (labels)
    let (tg, pNT, pET) = if diffNames
              then (g, pNT, pET)
              else (G.empty, M.empty, M.empty)
                where
                  pNT = M.fromList $ zipWith (\i (k,gi) -> (k, (gi, i)) ) [0..] nodeNames2GI
                  pET = M.fromList $ zipWith (\i (k,sm) -> (k, (sm, i)) ) [0..] edgeNames2SubMap
                  -- generate a mapping of node labels to node GIs
                  nodeNames2GI = sortBy (comparing fst) $ map (\(Node nid info) -> (infoLabelStr info, getNodeGI (fromEnum nid) ngiM)) (nodes g)
                  -- generate a mapping of edge labels to a submapping of (source node label, target node label) to edge GIs
                  edgeNames2SubMap = M.toList $
                                      foldr (\(Edge eid s t info) m ->
                                            let srcType = infoLabelStr . nodeInfo . Maybe.fromJust $ lookupNode s g
                                                tgtType = infoLabelStr . nodeInfo . Maybe.fromJust $ lookupNode t g
                                                edgeType = infoLabelStr info
                                                egi = getEdgeGI (fromEnum eid) egiM
                                                subM = case M.lookup edgeType m of
                                                  Nothing -> M.singleton (srcType,tgtType) egi
                                                  Just sm -> M.insert (srcType,tgtType) egi sm
                                                in M.insert edgeType subM m)
                                      M.empty (edges g)

    writeIORef typeGraph tg
    writeIORef possibleNodeTypes pNT
    writeIORef possibleEdgeTypes pET
    writeIORef possibleSelectableEdgeTypes pET

    -- update the comboBoxes
    updateComboBoxText nodeTypeCBox (map T.pack $ M.keys pNT)
    updateComboBoxText edgeTypeCBox (map T.pack $ M.keys pET)
    selNT <- Gtk.comboBoxGetActive nodeTypeCBox
    if selNT < 0 && M.size pNT > 0
      then Gtk.comboBoxSetActive nodeTypeCBox 0
      else return ()
    -- update the valid flags
    states <- readIORef graphStates
    setValidFlags store tg states




updateComboBoxText :: Gtk.ComboBoxText -> [T.Text] -> IO ()
updateComboBoxText cbox texts = do
    -- clean comboBox values
    Gtk.comboBoxTextRemoveAll cbox
    -- populate cbox
    forM_ texts $ \t -> Gtk.comboBoxTextAppendText cbox t



-- updates the edgeTypeCBox considering the current context (if a edge is selected, and which edges are selected)
changeEdgeTypeCBoxByContext :: IORef (M.Map String (M.Map (String,String)EdgeGI,Int32))
                            -> IORef (M.Map String (M.Map (String,String)EdgeGI,Int32))
                            -> Gtk.ComboBoxText
                            -> IORef GraphState
                            -> Graph Info Info
                            -> IO ()
changeEdgeTypeCBoxByContext possibleEdgeTypes possibleSelectableEdgeTypes edgeTypeCBox currentState tg = do
  es <- readIORef currentState
  sEdges <- return . snd . stateGetSelected $ es
  pET <- readIORef possibleEdgeTypes
  let defaultAction = do -- set all possible edge types as selectable and populate the combo box
        writeIORef possibleSelectableEdgeTypes pET
        updateComboBoxText edgeTypeCBox (map T.pack $ M.keys pET)
  case (G.null tg, sEdges) of
    -- typegraph is null -> impossible to do anything
    (True,_) -> return ()
    -- no edge selected -> all edge types are selectable
    (False, []) -> defaultAction
    -- one edge selected -> modify comboboxes to show what kind of types this edge can have
    (False,[eid]) -> changeEdgeTypeCBoxOne es eid pET possibleSelectableEdgeTypes edgeTypeCBox tg defaultAction
    -- many edges selected -> modify comboboxes to show what kind of types these edges can have
    (False,eids) -> changeEdgeTypeCBoxMultiple es eids pET possibleSelectableEdgeTypes edgeTypeCBox tg defaultAction

-- auxiliar function to changeEdgeTypeCBoxByContext
changeEdgeTypeCBoxOne es eid pET possibleSelectableEdgeTypes edgeTypeCBox tg defaultAction =
  do
    let g = stateGetGraph es
        edgeEndings = do
          e <- lookupEdge eid g
          src <- lookupNode (sourceId e) g
          tgt <- lookupNode (targetId e) g
          return (src, tgt)
    case edgeEndings of
      Just (src,tgt) ->
        updatePSETandCBox src tgt tg pET possibleSelectableEdgeTypes edgeTypeCBox
      Nothing ->
        do
          mapM_ putStrLn ["Error: changeEdgeTypeCBoxOne: edge or edge endings not found on lookup.", show eid, show g]
          defaultAction

-- auxiliar function to changeEdgeTypeCBoxByContext
changeEdgeTypeCBoxMultiple es eids pET possibleSelectableEdgeTypes edgeTypeCBox tg defaultAction =
  do
    let g = stateGetGraph es
        edgs = Maybe.catMaybes $ map (\eid -> lookupEdge eid g) eids
        endings = foldr (checkEndings g) (True,Nothing) edgs
    case endings of
      -- all selected edges have source and target nodes of same type ->
      --      modify comboboxes to show what kind of types these edges can have
      (True,Just (src,tgt)) -> updatePSETandCBox src tgt tg pET possibleSelectableEdgeTypes edgeTypeCBox
      -- the selected edges have source and target nodes of different types ->
      --      all edge types are selectable
      _ -> defaultAction

-- auxiliar function to changeEdgeTypeCBoxOne ane changeEdgeTypeCBoxMultiple
-- updates possibleSelectableEdgeTypes and edgeTypeCBox with the edge types that can connect src and tgt
updatePSETandCBox src tgt tg pET possibleSelectableEdgeTypes edgeTypeCBox =
  do
    let
      possibleTypes = listPossibleEdgeTypes tg src tgt
      pSET = M.filterWithKey (\k _ -> k `elem` possibleTypes) pET
      posF acc v = (acc+1,(fst v,acc))
      pSET' = snd $ M.mapAccum posF 0 pSET
    writeIORef possibleSelectableEdgeTypes pSET'
    updateComboBoxText edgeTypeCBox (map T.pack $ M.keys pSET')


-- two edges are compatible if their connecting nodes have the same types
-- this function gets a graph, an edge and a tuple containing a boolean value that informs
-- if so far the edges are compatible and the nodes of the previous comparation
-- made to be used in a fold
checkEndings g e ends =
  let mSrc = lookupNode (sourceId e) g
      mTgt = lookupNode (targetId e) g
  in case ends of
      (True,Nothing) ->
          case (mSrc,mTgt) of
              (Just s, Just t) -> (True,Just (s,t))
              _ -> (False,Nothing)
      (True,Just (s,t)) ->
          (srcMatch && tgtMatch, Just (s,t))
          where
            srcMatch = nmatch mSrc s
            tgtMatch = nmatch mTgt t
            nmatch mn n = case mn of
              Nothing -> False
              Just n' -> (infoType $ nodeInfo n) == (infoType $ nodeInfo n')
      _ -> (False,Nothing)
