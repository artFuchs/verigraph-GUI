{-|
  This module provides the functions used as callbacks for the Graph Grammar editor
-}

{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module GUI.Editor(
  startEditor
, prepToExport
, confirmOperation
, storeCurrentES
)where

import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import qualified GI.Pango as P
import qualified GI.Cairo.Structs as Cairo
import Data.GI.Base
import Data.GI.Base.GValue
import Data.GI.Base.GType
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Zip
import Graphics.Rendering.Cairo
import Graphics.Rendering.Pango.Layout
import Graphics.Rendering.Pango

-- haskell data modules
import Data.IORef
import Data.List
import Data.Int
import Data.Char
import Data.Maybe
import Data.Either
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Tree as Tree
import Data.Monoid
import Data.Ord

-- verigraph modules
import Abstract.Rewriting.DPO
import Data.Graphs hiding (null, empty)
import qualified Data.Graphs as G
import qualified Data.TypedGraph.Morphism as TGM

-- Verigraph-GUI modules
import           GUI.Data.DiaGraph hiding (empty)
import qualified GUI.Data.DiaGraph as DG
import           GUI.Data.GraphState
import           GUI.Data.GraphicalInfo
import           GUI.Data.Info hiding (empty)
import qualified GUI.Data.Info as I
import           GUI.Data.Nac
import           GUI.Dialogs
import           GUI.Helper.BasicCanvasCallbacks
import           GUI.Helper.Geometry
import           GUI.Helper.GrammarMaker
import           GUI.Helper.GraphicalInfo
import           GUI.Helper.GraphValidation
import           GUI.Helper.List
import           GUI.Render.Render
import           GUI.Render.GraphDraw

-- Editor modules
import GUI.Editor.Helper.Clipboard
import GUI.Editor.Helper.Nac
import GUI.Editor.Helper.SaveLoad
import GUI.Editor.Helper.TypeInfer
import GUI.Editor.Helper.TreeStore
import GUI.Editor.Helper.UndoRedo
import GUI.Editor.UI.UIBuilders
import GUI.Editor.UI.UpdateInspector

---------------------------------------------------------------------------------------------------------------------------------
--  IORefs Used in Editor  ------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------

type StoreIORefs      = ( IORef (M.Map Int32 GraphState), IORef [Int32], IORef Int32, IORef Int32 )
type ChangesIORefs    = ( IORef Bool, IORef [Bool], IORef (M.Map Int32 DiaGraph))
type NacIORefs        = ( IORef (M.Map Int32 (DiaGraph, MergeMapping)), IORef (Maybe MergeMapping))

---------------------------------------------------------------------------------------------------------------------------------
--  Editor Construction  --------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------

{- | Create the editor widgets (the treeView at left, the canvas at center and the inpector panel at right)
    and set the callbacks for them.
    Returns the gtkPaned parent to those widgets.
-}
startEditor :: Gtk.Window -> Gtk.TreeStore
               -> IORef (Maybe String) -> IORef (Graph Info Info)
               -> StoreIORefs -> ChangesIORefs -> NacIORefs
               -> [Gtk.MenuItem] -> [Gtk.MenuItem] -> [Gtk.MenuItem]
               -> IORef (Maybe Gtk.DrawingArea) -> IORef (Maybe (IORef GraphState))
               -> IO (Gtk.Paned, Gtk.DrawingArea, IORef GraphState)
startEditor window store
            fileName typeGraph
            storeIORefs changesIORefs nacIORefs
            fileItems editItems viewItems
            focusedCanvas focusedStateIORef = do

  -- create the ui of the editor
  (mainPane, treeFrame, canvas, nameEntry, entryLabel, layoutWidgets, typeSelectionWidgets) <- buildEditor

  let (layoutBox, fillColorBox, fillColorBtn, lineColorBox, lineColorBtn, nodeShapeFrame, radioShapes, edgeStyleFrame, radioStyles) = layoutWidgets
      (typeSelectionBox, autoLabelNCheckBtn, autoLabelECheckBtn, nodeTypeBox, nodeTypeCBox, edgeTypeBox, edgeTypeCBox, operationBox, operationCBox, mergeBtn, splitBtn) = typeSelectionWidgets
      [circleRadioBtn, rectRadioBtn, squareRadioBtn] = radioShapes
      [normalRadioBtn, slashedRadioBtn, pointedRadioBtn] = radioStyles
  let
    typeInspWidgets = (nameEntry, fillColorBox, fillColorBtn, lineColorBtn, nodeShapeFrame, radioShapes, edgeStyleFrame, radioStyles)
    hostInspWidgets = (nameEntry, nodeTypeCBox, edgeTypeCBox)
    ruleInspWidgets = (nameEntry, nodeTypeCBox, edgeTypeCBox, operationCBox)
    nacInspWidgets  = (nameEntry, nodeTypeCBox, edgeTypeCBox, mergeBtn, splitBtn)

  -- create the treePanel and append it to the treeFrame
  (treeBox, treeview, nameRenderer, activeRenderer, createRBtn, removeBtn, createNBtn) <- buildTreePanel
  Gtk.containerAdd treeFrame treeBox
  Gtk.treeViewSetModel treeview (Just store)
  initTreeView treeview



  -- "unpack" menuItems
  let [newm,opn,svn,sva] = fileItems
      [del,undo,redo,cpy,pst,cut,sla,sln,sle,mrg,spt] = editItems
      [zin,zut,z50,zdf,z150,z200,vdf] = viewItems

  mapM_ (\m -> Gtk.widgetSetSensitive m False) [mrg,spt]

  #showAll mainPane
  #hide typeSelectionBox
  #hide createNBtn
  #hide removeBtn

  ----------------------------------------------------------------------------------------------------------------------------
  -- IORefs  -----------------------------------------------------------------------------------------------------------------
  ----------------------------------------------------------------------------------------------------------------------------

  -- variables used to edit
  currentState    <- newIORef emptyState -- current state: the necessary info to draw the selected graph
  oldPoint        <- newIORef (0.0,0.0) -- last point where a mouse button was pressed
  squareSelection <- newIORef Nothing -- selection box : Maybe (x1,y1,x2,y2)
  movingGI          <- newIORef False -- if the user started moving some object - necessary to add a position to the undoStack

  -- copy/paste
  clipboard       <- newIORef DG.empty -- clipboard - DiaGraph

  -- variables used to edit visual elements of type graphs
  currentShape    <- newIORef NCircle -- the shape that all new nodes must have
  currentStyle    <- newIORef ENormal -- the style that all new edges must have
  currentC        <- newIORef (1,1,1) -- the color to init new nodes
  currentLC       <- newIORef (0,0,0) -- the color to init new edges and the line and text of new nodes

  -- variables for undo/redo
  undoStack       <- newIORef (M.empty :: M.Map Int32 ChangeStack)
  redoStack       <- newIORef (M.empty :: M.Map Int32 ChangeStack)

  -- variables for typeInference
  {-
      Possible types that a node can have in a typed graph.
      Each node type is identified by a string
       and specifies a pair with a NodeGI and the position of the entry in the comboBox
  -}
  possibleNodeTypes   <- newIORef ( M.empty :: M.Map String (NodeGI, Int32))
  {-
      Possible types that an edge can have in a typed graph.
      Each edge type is identified by a string and specifies a pair with a map of EdgeGI,
       which keys are a pair of Strings (Source, Target), and the position in the comboBox
  -}
  possibleEdgeTypes   <- newIORef ( M.empty :: M.Map String (M.Map (String, String) EdgeGI, Int32))
  -- Subset of possible Edge Types for selected edges with changed positions in the combobox
  possibleSelectableEdgeTypes <- newIORef (M.empty :: M.Map String (M.Map (String, String) EdgeGI, Int32))
  currentNodeType     <- newIORef ( Nothing :: Maybe String)
  currentEdgeType     <- newIORef ( Nothing :: Maybe String)

  let selectableTypesIORefs = (possibleNodeTypes, possibleEdgeTypes, currentNodeType, currentEdgeType)


  -- "unpack" IORefs
  let (graphStates,currentPath,currentGraph,currentGraphType) = storeIORefs
      (changedProject, changedGraph, lastSavedState) = changesIORefs
      (nacInfoMap, mergeMapping) = nacIORefs

  -- set IORefs to make the zoom controls work on the canvas
  writeIORef focusedCanvas $ Just canvas
  writeIORef focusedStateIORef $ Just currentState



  ----------------------------------------------------------------------------------------------------------------------------
  -- Auxiliar Functions for Bindings  ----------------------------------------------------------------------------------------
  ----------------------------------------------------------------------------------------------------------------------------

  let updateByType = do
        gt <- readIORef currentGraphType
        case gt of
          0 -> return ()
          1 -> updateTG currentState typeGraph possibleNodeTypes possibleEdgeTypes possibleSelectableEdgeTypes graphStates nodeTypeCBox edgeTypeCBox store
          _ -> do
            if gt == 4
              then updateNacInfo nacInfoMap currentGraph mergeMapping currentState
              else return ()
            setCurrentValidFlag store currentState typeGraph currentPath





  ----------------------------------------------------------------------------------------------------------------------------
  -- Event Bindings for the Canvas -------------------------------------------------------------------------------------------
  ----------------------------------------------------------------------------------------------------------------------------

  -- drawing event
  on canvas #draw $ drawGraphByType currentState typeGraph squareSelection currentGraphType mergeMapping

  -- mouse button pressed on canvas
  -- set callback to select elements on canvas
  on canvas #buttonPressEvent $ basicCanvasButtonPressedCallback currentState oldPoint squareSelection canvas
  -- set callback to create nodes or edges
  on canvas #buttonPressEvent $ \eventButton -> do
    b <- get eventButton #button
    click <- get eventButton #type
    ms <- get eventButton #state

    es <- readIORef currentState
    mergeM <- readIORef mergeMapping

    let doubleLeftClick = (b == 1) && (click == Gdk.EventType2buttonPress)
    let rightButton = (b == 3) && not (Gdk.ModifierTypeControlMask `elem` ms)

    created <- case (doubleLeftClick, rightButton) of
        -- left button with double click -> rename element
        (True,False) -> do
          es <- readIORef currentState
          case stateGetSelected es of
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
        indicateChanges window store storeIORefs changesIORefs undoStack redoStack mergeM es
    else
      return ()

    -- changed the edge type box displayed options based on wich edges are selected
    gType <- readIORef currentGraphType
    tg <- readIORef typeGraph
    if gType > 1
      then changeEdgeTypeCBoxByContext possibleEdgeTypes possibleSelectableEdgeTypes edgeTypeCBox currentState tg
      else return ()

    -- update inspector UI
    updateInspector currentGraphType currentState  mergeMapping selectableTypesIORefs
                    currentC currentLC
                    typeInspWidgets hostInspWidgets ruleInspWidgets nacInspWidgets
                    (nodeTypeBox, edgeTypeBox)

    return True

  -- mouse motion on canvas
  -- set callback to move elements or expand the square selection area
  on canvas #motionNotifyEvent $ basicCanvasMotionCallBack currentState oldPoint squareSelection canvas
  -- if the left button is hold while moving, indicate changes and add the previous diagram to the undo stack
  on canvas #motionNotifyEvent $ indicateChangesWhenMovingElements window store movingGI currentState mergeMapping undoStack redoStack storeIORefs changesIORefs

  -- mouse button release on canvas
  -- set callback to select elements that are inside squareSelection
  on canvas #buttonReleaseEvent $ basicCanvasButtonReleasedCallback currentState squareSelection canvas
  -- if there are elements selected by the squareSelection, then update the inspector and the edge type selection comboBox
  on canvas #buttonReleaseEvent $ \eventButton -> do
    b <- get eventButton #button
    gType <- readIORef currentGraphType
    tg <- readIORef typeGraph
    es <- readIORef currentState
    case b of
      1 -> do
        writeIORef movingGI False
        let (sNodes, sEdges) = stateGetSelected es
        case (length sNodes > 0 || length sEdges > 0) of
          True -> do
            if gType > 1
              then changeEdgeTypeCBoxByContext possibleEdgeTypes possibleSelectableEdgeTypes edgeTypeCBox currentState tg
              else return ()
            updateInspector currentGraphType currentState  mergeMapping selectableTypesIORefs
                            currentC currentLC
                            typeInspWidgets hostInspWidgets ruleInspWidgets nacInspWidgets
                            (nodeTypeBox, edgeTypeBox)
          False -> return ()
      _ -> return ()
    return True

  -- mouse wheel scroll on canvas
  on canvas #scrollEvent $ basicCanvasScrollCallback currentState canvas

  -- keyboard
  on canvas #keyPressEvent $ \eventKey -> do
    k <- get eventKey #keyval >>= return . chr . fromIntegral
    ms <- get eventKey #state
    case (Gdk.ModifierTypeControlMask `elem` ms, Gdk.ModifierTypeShiftMask `elem` ms, toLower k) of
      -- F2 - rename selection
      (False,False,'\65471') -> Gtk.widgetGrabFocus nameEntry
      -- 'delete' while the focus is on canvas - delete elements
      (False,False,'\65535') -> Gtk.menuItemActivate del
      _ -> return ()
    return True

  -- if the canvas is focused, then set this canvas to respond to the events of the view menu, like zoom in and zoom out
  on canvas #focusInEvent $ \event -> do
    writeIORef focusedCanvas $ Just canvas
    writeIORef focusedStateIORef $ Just currentState
    return False

  ----------------------------------------------------------------------------------------------------------------------------
  -- Event Bindings for the Inspector Panel  ---------------------------------------------------------------------------------
  ----------------------------------------------------------------------------------------------------------------------------

  -- if the user press enter, then change focus to the canvas so that the selected elements are renamed
  on nameEntry #keyPressEvent $ \eventKey -> do
    k <- get eventKey #keyval >>= return . chr . fromIntegral
    --if it's Return or Enter (Numpad), change focus to the canvas
    case k of
       '\65293' -> Gtk.widgetGrabFocus canvas
       '\65421' -> Gtk.widgetGrabFocus canvas
       _       -> return ()
    return False

  -- when the nameEntry lose focus, rename selected element(s)
  on nameEntry #focusOutEvent $ \event -> do
    es <- readIORef currentState
    mergeM <- readIORef mergeMapping
    renameSelectedCallback nameEntry canvas currentState possibleEdgeTypes typeGraph
    indicateChanges window store storeIORefs changesIORefs undoStack redoStack mergeM es
    updateInspector currentGraphType currentState  mergeMapping selectableTypesIORefs
                    currentC currentLC
                    typeInspWidgets hostInspWidgets ruleInspWidgets nacInspWidgets
                    (nodeTypeBox, edgeTypeBox)
    updateByType
    return False


  -- select a fill color
  -- change the selection fill color and
  -- set the current fill color as the selected color
  on fillColorBtn #colorSet $ do
    st <- readIORef currentState
    setNewColor canvas fillColorBtn currentC False currentState
    updateTG currentState typeGraph possibleNodeTypes possibleEdgeTypes possibleSelectableEdgeTypes graphStates nodeTypeCBox edgeTypeCBox store
    indicateChanges window store storeIORefs changesIORefs undoStack redoStack Nothing st

  -- select a line color
  -- same as above, except it's for the line color
  on lineColorBtn #colorSet $ do
    st <- readIORef currentState
    setNewColor canvas lineColorBtn currentLC True currentState
    updateTG currentState typeGraph possibleNodeTypes possibleEdgeTypes possibleSelectableEdgeTypes graphStates nodeTypeCBox edgeTypeCBox store
    indicateChanges window store storeIORefs changesIORefs undoStack redoStack Nothing st


  -- toogle the radio buttons for node shapes
  -- change the shape of the selected nodes and set the current shape for new nodes
  circleRadioBtn `on` #toggled $ do
    st <- readIORef currentState
    changed <- setNewShape circleRadioBtn canvas NCircle currentShape currentState
    if changed then
      do indicateChanges window store storeIORefs changesIORefs undoStack redoStack Nothing st
         updateTG currentState typeGraph possibleNodeTypes possibleEdgeTypes possibleSelectableEdgeTypes graphStates nodeTypeCBox edgeTypeCBox store
    else return ()

  rectRadioBtn `on` #toggled $ do
    st <- readIORef currentState
    changed <- setNewShape rectRadioBtn canvas NRect currentShape currentState
    if changed then
      do  indicateChanges window store storeIORefs changesIORefs undoStack redoStack Nothing st
          updateTG currentState typeGraph possibleNodeTypes possibleEdgeTypes possibleSelectableEdgeTypes graphStates nodeTypeCBox edgeTypeCBox store
    else return ()

  squareRadioBtn `on` #toggled $ do
    st <- readIORef currentState
    changed <- setNewShape squareRadioBtn canvas NSquare currentShape currentState
    if changed then
      do  indicateChanges window store storeIORefs changesIORefs undoStack redoStack Nothing st
          updateTG currentState typeGraph possibleNodeTypes possibleEdgeTypes possibleSelectableEdgeTypes graphStates nodeTypeCBox edgeTypeCBox store
    else return ()





  -- toogle the radio buttons for edge styles
  -- change the style of the selected edges and set the current style for new edges
  normalRadioBtn `on` #toggled $ do
    st <- readIORef currentState
    changed <- setNewStyle normalRadioBtn canvas ENormal currentStyle currentState
    if changed then
      do  indicateChanges window store storeIORefs changesIORefs undoStack redoStack Nothing st
          updateTG currentState typeGraph possibleNodeTypes possibleEdgeTypes possibleSelectableEdgeTypes graphStates nodeTypeCBox edgeTypeCBox store
    else return ()

  pointedRadioBtn `on` #toggled $ do
    st <- readIORef currentState
    changed <- setNewStyle pointedRadioBtn canvas EPointed currentStyle currentState
    if changed then
      do  indicateChanges window store storeIORefs changesIORefs undoStack redoStack Nothing st
          updateTG currentState typeGraph possibleNodeTypes possibleEdgeTypes possibleSelectableEdgeTypes graphStates nodeTypeCBox edgeTypeCBox store
    else return ()

  slashedRadioBtn `on` #toggled $ do
    st <- readIORef currentState
    changed <- setNewStyle slashedRadioBtn canvas ESlashed currentStyle currentState
    if changed then
      do  indicateChanges window store storeIORefs changesIORefs undoStack redoStack Nothing st
          updateTG currentState typeGraph possibleNodeTypes possibleEdgeTypes possibleSelectableEdgeTypes graphStates nodeTypeCBox edgeTypeCBox store
    else return ()


  -- choose a type in the type nodeTypeCBox for nodes
  on nodeTypeCBox #changed $ do
    gt <- readIORef currentGraphType
    index <- Gtk.comboBoxGetActive nodeTypeCBox
    case (index<0,gt<2) of
      (True,_) -> return ()
      (False,True) -> do
        typeInfo <- Gtk.comboBoxTextGetActiveText nodeTypeCBox >>= return . T.unpack
        writeIORef currentNodeType $ Just typeInfo
      (False,False) -> do
        typeInfo <- Gtk.comboBoxTextGetActiveText nodeTypeCBox >>= return . T.unpack
        typeEntry <- readIORef possibleNodeTypes >>= return . M.lookup typeInfo
        case typeEntry of
          Nothing -> return ()
          Just (typeNGI,_) -> do
            es <- readIORef currentState

            let (sNids,sEids) = stateGetSelected es
                g = stateGetGraph es
                -- foreach selected node, change their types
                acceptableSNids = filter (\nid -> case lookupNode nid g of
                                                    Nothing -> False
                                                    Just n -> not $ infoLocked (nodeInfo n)) sNids
                giM = stateGetGI es
                g' = foldr (\nid g -> updateNodePayload nid g (\info -> infoSetType info typeInfo)) g acceptableSNids
                newNGI = foldr  (\nid giM -> let ngi = getNodeGI (fromEnum nid) giM
                                             in M.insert (fromEnum nid) (typeNGI {position = position ngi, dims = dims ngi}) giM)
                                (fst giM)
                                acceptableSNids
                es' = stateSetGraph g' . stateSetGI (newNGI, snd giM) . stateSetSelected (acceptableSNids,sEids) $ es

                -- foreach changed node, change the type of the edges connected to it
            typesE <- readIORef possibleEdgeTypes >>= return . M.map fst
            tg <- readIORef typeGraph
            let es'' = infereEdgesTypesAfterNodeChange es' tg typesE

            if gt == 4
              then do
                nacInfoM <- readIORef nacInfoMap
                index <- readIORef currentGraph
                let ((ng,ngiM), nacM) = fromMaybe ((G.empty,(M.empty,M.empty)),(M.empty,M.empty)) $ M.lookup index nacInfoM
                    newNG = extractNacGraph (stateGetGraph es'') nacM
                    newNGIM = extractNacGI (stateGetGraph es'') (stateGetGI es'') nacM
                modifyIORef nacInfoMap $ M.insert index ((newNG, newNGIM), nacM)
              else return ()

            writeIORef currentState es''
            writeIORef currentNodeType $ Just typeInfo
            Gtk.widgetQueueDraw canvas
            setCurrentValidFlag store currentState typeGraph currentPath

  -- choose a type in the type comboBox for edges
  on edgeTypeCBox #changed $ do
    gt <- readIORef currentGraphType
    if gt < 2
      then return ()
      else do
        index <- Gtk.comboBoxGetActive edgeTypeCBox
        if index == (-1)
          then return ()
          else do
            es <- readIORef currentState
            typeInfo <- Gtk.comboBoxTextGetActiveText edgeTypeCBox >>= return . T.unpack
            typeEntry <- readIORef possibleEdgeTypes >>= return . M.lookup typeInfo
            case typeEntry of
              Nothing -> return()
              Just (pET,_) -> do
                let (sNids,sEids) = stateGetSelected es
                    g = stateGetGraph es
                    giM = stateGetGI es
                    acceptableSEids = filter (\eid -> case lookupEdge eid g of
                                                          Nothing -> False
                                                          Just e -> not $ infoLocked (edgeInfo e)) sEids
                    edgesInContext = catMaybes $ map (\eid -> lookupEdgeInContext eid g) acceptableSEids

                    changeEdgeGI ((src,_),e,(tgt,_)) giM =
                          let eid = fromEnum $ edgeId e
                              egi = getEdgeGI eid giM
                          in case M.lookup (infoType $ nodeInfo src, infoType $ nodeInfo tgt) pET of
                                  Nothing -> giM
                                  Just typeGI -> M.insert eid (typeGI {cPosition = cPosition egi}) giM
                    newEGI = foldr changeEdgeGI (snd giM) edgesInContext
                    newGI = (fst giM, newEGI)
                    newGraph = foldr (\eid g -> updateEdgePayload eid g (\info -> infoSetType info typeInfo)) g acceptableSEids
                case gt of
                  4 -> do
                    nacInfoM <- readIORef nacInfoMap
                    index <- readIORef currentGraph
                    let ((ng,ngiM), nacM) = fromMaybe ( (G.empty,(M.empty,M.empty)), (M.empty,M.empty) ) $ M.lookup index nacInfoM
                        newNG = extractNacGraph newGraph nacM
                        newNacGI = extractNacGI newGraph newGI nacM
                        newNacdg = (newNG, newNacGI)
                    modifyIORef nacInfoMap $ M.insert index (newNacdg, nacM)
                  _ -> return ()
                writeIORef currentState (stateSetGI newGI . stateSetGraph newGraph $ es)
                writeIORef currentEdgeType $ Just typeInfo
                Gtk.widgetQueueDraw canvas
                setCurrentValidFlag store currentState typeGraph currentPath

  -- choose a operaion in the operation comboBox
  on operationCBox #changed $ do
    t <- readIORef currentGraphType
    if t < 3
      then return ()
      else do
        index <- Gtk.comboBoxGetActive operationCBox
        if index < 0 || index > 2
          then return ()
          else do
            es <- readIORef currentState
            operationInfo <- return $ case index of
              0 -> Preserve
              1 -> Create
              2 -> Delete
            let (sNids,sEids) = stateGetSelected es
                g = stateGetGraph es
                gi = stateGetGI es
                newGraph  = foldl (\g nid -> updateNodePayload nid g (\info -> infoSetOperation info operationInfo)) g sNids
                newGraph' = foldl (\g eid -> updateEdgePayload eid g (\info -> infoSetOperation info operationInfo)) newGraph sEids
            context <- Gtk.widgetGetPangoContext canvas
            font <- case operationInfo == Preserve of
              True -> return Nothing
              False -> return $ Just "Sans Bold 10"
            maybeNodedims <- forM sNids $ \nid -> do
              let maybeNode = G.lookupNode nid newGraph'
              case maybeNode of
                Nothing -> return Nothing
                Just n -> do
                  dim <- getStringDims (infoVisible $ nodeInfo n) context font
                  return $ Just (nid, dim)
            let ndims = catMaybes maybeNodedims
                newNgiM = foldl (\giM (nid, dim) -> let gi = (getNodeGI (fromEnum nid) giM) {dims = dim}
                                                    in M.insert (fromEnum nid) gi giM) (fst gi) ndims
            writeIORef currentState (stateSetGI (newNgiM, snd gi) . stateSetGraph newGraph' $ es)
            Gtk.widgetQueueDraw canvas


  -- merge or split buttons pressed: merge or split elements in nac
  on mergeBtn #clicked $ Gtk.menuItemActivate mrg
  on splitBtn #clicked $ Gtk.menuItemActivate spt

  ----------------------------------------------------------------------------------------------------------------------------
  -- Event Bindings for the TreeView  ----------------------------------------------------------------------------------------
  ----------------------------------------------------------------------------------------------------------------------------
  -- event: changed the selected graph
  on treeview #cursorChanged $ do
    selection <- Gtk.treeViewGetSelection treeview
    (sel,model,iter) <- Gtk.treeSelectionGetSelected selection
    case sel of
      False -> return ()
      True -> do
        -- get the current path for update
        path <- Gtk.treeModelGetPath model iter >>= Gtk.treePathGetIndices >>= return . fromMaybe [0]
        -- compare the selected graph with the current one
        cIndex <- readIORef currentGraph
        cGType <- readIORef currentGraphType
        index <- Gtk.treeModelGetValue model iter 2 >>= fromGValue  :: IO Int32
        gType <- Gtk.treeModelGetValue model iter 3 >>= fromGValue  :: IO Int32

        -- change the graph according to selection
        case (cIndex == index, gType) of
          -- case the selection did not change, just update the path
          (True, _)  -> writeIORef currentPath path

          -- case the selection in the treeview is just a Topic, do nothing
          (False, 0) -> return ()

          -- case the selection is a NAC, mount the graph with the LHS part, the additional elements and the merge information
          (False, 4) -> do
            -- update the current path
            writeIORef currentPath path
            -- update the current graph in the tree
            storeCurrentES window currentState storeIORefs nacInfoMap

            -- build the graph for the nac
            writeIORef currentGraphType gType
            states <- readIORef graphStates
            let maybeState = M.lookup index states
            case maybeState of
              Just es -> do
                tg <- readIORef typeGraph
                -- load lhs diagraph
                (lhs,lgi) <- getParentLHSDiaGraph store path graphStates
                let lhsIsValid = isGraphValid lhs tg

                case lhsIsValid of
                  False -> do
                    showError window "Parent rule have type errors. Please, correct them before loading nacGraph."
                    if (length path > 1)
                      then do
                        parentPath <- Gtk.treePathNewFromIndices (init path)
                        Gtk.treeViewSetCursor treeview parentPath (Nothing :: Maybe Gtk.TreeViewColumn) False
                      else return ()
                  True -> do
                    -- load nac' diagraph
                    context <- Gtk.widgetGetPangoContext canvas
                    lhsNgi <- updateNodesGiDims (fst lgi) lhs context
                    let lhsgi = (lhsNgi, snd lgi)
                    nacInfoM <- readIORef nacInfoMap
                    (nG,nGI) <- case M.lookup index nacInfoM of
                      Nothing -> return (lhs,lhsgi)
                      Just (nacdg,mergeM) -> do
                        (nacdg',mergeM') <- applyLhsChangesToNac lhs (nacdg,mergeM) (Just context)
                        writeIORef mergeMapping $ Just mergeM'
                        modifyIORef nacInfoMap $ M.insert index (nacdg', mergeM')
                        return $ mountNACGraph (lhs,lhsgi) tg (nacdg', mergeM')
                    writeIORef currentState $ stateSetGI nGI . stateSetGraph nG $ es
                    writeIORef currentGraph index
              Nothing -> return ()

          -- case the selection is another type of graph, get the graph from the map
          (False, _) -> do
            -- update the current path
            writeIORef currentPath path
            -- update the current graph in the tree
            storeCurrentES window currentState storeIORefs nacInfoMap
            -- load the selected graph from the tree
            writeIORef currentGraphType gType
            states <- readIORef graphStates
            let maybeState = M.lookup index states
            case maybeState of
              Just es -> do
                writeIORef currentState es
                writeIORef currentGraph index
                writeIORef mergeMapping Nothing
              Nothing -> return ()

        -- auxiliar function to update nodes and edges elements according to the active typeGraph
        let updateElements = do
              pnt <- readIORef possibleNodeTypes
              pet <- readIORef possibleEdgeTypes
              es <- readIORef currentState
              let g = stateGetGraph es
                  (ngi, egi) = stateGetGI es
                  fn node = (nid, infoType (nodeInfo node), getNodeGI nid ngi)
                            where nid = fromEnum (nodeId node)
                  gn (i,t,gi) = case M.lookup t pnt of
                                  Nothing -> (i,gi)
                                  Just (gi',_) -> (i, gi {fillColor = (fillColor gi'), shape = (shape gi')})
                  fe ((src,_), edge, (tgt,_)) = (eid, eType, srcType, tgtType, getEdgeGI eid egi)
                            where eid = fromEnum (edgeId edge)
                                  eType = infoType (edgeInfo edge)
                                  srcType = infoType $ nodeInfo src
                                  tgtType = infoType $ nodeInfo tgt
                  ge (i,et,currentState,tt,gi) = case M.lookup et pet of
                                  Nothing -> (i,gi)
                                  Just (sm,_) -> case M.lookup (currentState,tt) sm of
                                    Nothing -> (i,gi)
                                    Just gi' -> (i, gi' {cPosition = cPosition gi})
                  newNodeGI = M.fromList . map gn . map fn $ nodes g
                  newEdgeGI = M.fromList . map ge . map fe $ edgesInContext g
              writeIORef currentState (stateSetGI (newNodeGI, newEdgeGI) es)

        -- auxiliar function to set the GI of new elements to a default
        let resetCurrentGI = do
                writeIORef currentShape NCircle
                writeIORef currentStyle ENormal
                writeIORef currentC (1,1,1)
                writeIORef currentLC (0,0,0)

        -- change the UI elements according to the selected graph
        case gType of
          1 -> do
            #show layoutBox
            #hide typeSelectionBox
            mapM_ (\m -> Gtk.widgetSetSensitive m False) [mrg,spt]
          2 -> do
            #hide layoutBox
            #show typeSelectionBox
            #hide operationBox
            #hide mergeBtn
            #hide splitBtn
            resetCurrentGI
            updateElements
            mapM_ (\m -> Gtk.widgetSetSensitive m False) [mrg,spt]
          3 -> do
            #hide layoutBox
            #show typeSelectionBox
            #show operationBox
            #hide mergeBtn
            #hide splitBtn
            resetCurrentGI
            updateElements
            mapM_ (\m -> Gtk.widgetSetSensitive m False) [mrg,spt]
          4 -> do
            #hide layoutBox
            #show typeSelectionBox
            #hide operationBox
            #show mergeBtn
            #show splitBtn
            resetCurrentGI
            updateElements
            mapM_ (\m -> Gtk.widgetSetSensitive m True) [mrg,spt]
          _ -> return ()

        if gType == 3 || gType == 4
          then do
            #show createNBtn
            #show removeBtn
            case gType of
              3 -> set removeBtn [#label := T.pack "Remove Rule"]
              4 -> set removeBtn [#label := T.pack "Remove NAC"]
          else do
            #hide createNBtn
            #hide removeBtn
        Gtk.widgetQueueDraw canvas

  -- pressed the 'new rule' button on the treeview area
  -- create a new Rule
  on createRBtn #clicked $ do
    states <- readIORef graphStates
    let newKey = if M.size states > 0 then maximum (M.keys states) + 1 else 0
    (valid,parent) <- Gtk.treeModelIterNthChild store Nothing 2
    if not valid
      then return ()
      else do
        n <- Gtk.treeModelIterNChildren store (Just parent)
        iter <- Gtk.treeStoreAppend store (Just parent)
        storeSetGraphStore store iter ("Rule" ++ (show n), False, newKey, 3, True, True)
        path <- Gtk.treeModelGetPath store iter
        Gtk.treeViewExpandToPath treeview path
        modifyIORef graphStates (M.insert newKey emptyState)
        modifyIORef undoStack (M.insert newKey [])
        modifyIORef redoStack (M.insert newKey [])

  -- pressed the 'remove rule' button on the treeview area
  -- remove a Rule
  on removeBtn #clicked $ do
    selection <- Gtk.treeViewGetSelection treeview
    (sel,model,iter) <- Gtk.treeSelectionGetSelected selection
    if not sel
      then return ()
      else do
        let removeNac iter = do
                index <- Gtk.treeModelGetValue store iter 2 >>= fromGValue
                hasMore <- Gtk.treeStoreRemove store iter
                modifyIORef graphStates $ M.delete index
                modifyIORef nacInfoMap $ M.delete index
                return hasMore
        let removeNacs iter = do
                continue <- removeNac iter
                if continue
                  then removeNacs iter
                  else return ()
        gtype <- Gtk.treeModelGetValue store iter 3 >>= fromGValue :: IO Int32
        case gtype of
          3 -> do
            (hasNac, nacIter) <- Gtk.treeModelIterChildren store (Just iter)
            if hasNac
              then removeNacs nacIter
              else return ()
            index <- Gtk.treeModelGetValue store iter 2 >>= fromGValue
            modifyIORef graphStates $ M.delete index
            Gtk.treeStoreRemove store iter
            return ()
          4 -> do
            removeNac iter
            return ()
          _ -> showError window "Selected Graph is not a NAC or a Rule."

  -- pressed the 'create NAC' vutton on the treeview area
  -- create NAC for the current rule
  on createNBtn #clicked $ do
    states <- readIORef graphStates
    selection <- Gtk.treeViewGetSelection treeview
    (sel, model, iter) <- Gtk.treeSelectionGetSelected selection
    gtype <- Gtk.treeModelGetValue store iter 3 >>= fromGValue :: IO Int32
    iterR <- case gtype of
      3 -> return iter
      4 -> do
        (valid, iterR) <- Gtk.treeModelIterParent store iter
        if valid
          then return iterR
          else return iter
    if not sel
      then return ()
      else do
        index <- Gtk.treeModelGetValue store iterR 2 >>= fromGValue :: IO Int32
        if gtype == 3 || gtype == 4
          then do
            storeCurrentES window currentState storeIORefs nacInfoMap
            states <- readIORef graphStates
            let es = fromMaybe emptyState $ M.lookup index states
                (lhs,_,_) = graphToRuleGraphs (stateGetGraph es)
                gi = stateGetGI es
                ngi' = M.filterWithKey (\k a -> (NodeId k) `elem` (nodeIds lhs)) (fst gi)
                egi' = M.filterWithKey (\k a -> (EdgeId k) `elem` (edgeIds lhs)) (snd gi)
                nodeMap = M.empty
                edgeMap = M.empty
                newKey = if M.size states > 0 then maximum (M.keys states) + 1 else 0
            n <- Gtk.treeModelIterNChildren store (Just iterR)
            iterN <- Gtk.treeStoreAppend store (Just iterR)
            storeSetGraphStore store iterN ("NAC" ++ (show n), False, newKey, 4, True, True)
            path <- Gtk.treeModelGetPath store iterN
            Gtk.treeViewExpandToPath treeview path
            modifyIORef nacInfoMap (M.insert newKey (DG.empty,(nodeMap,edgeMap)))
            modifyIORef graphStates (M.insert newKey (stateSetGraph lhs . stateSetGI (ngi',egi') $ emptyState))
            modifyIORef undoStack (M.insert newKey [])
            modifyIORef redoStack (M.insert newKey [])
          else showError window "Selected Graph is not a rule, it's not possible to create NACs for it."

  -- edited a graph name
  on nameRenderer #edited $ \pathStr newName -> do
    path <- Gtk.treePathNewFromString pathStr
    (v,iter) <- Gtk.treeModelGetIter store path
    if v
      then do
        gval <- toGValue (Just newName)
        Gtk.treeStoreSet store iter [0] [gval]
        writeIORef changedProject True
        indicateProjChanged window True
      else return ()

  -- toggle the active property of a rule
  on activeRenderer #toggled $ \pathRepr -> do
    path <- Gtk.treePathNewFromString pathRepr
    (valid, iter) <- Gtk.treeModelGetIter store path
    if valid
      then do
        gType <- Gtk.treeModelGetValue store iter 3 >>= \gv -> (fromGValue gv :: IO Int32)
        active <- Gtk.treeModelGetValue store iter 4 >>= \gv -> (fromGValue gv :: IO Bool)
        notActive <- toGValue (not active)
        case gType of
          3 -> do Gtk.treeStoreSetValue store iter 4 notActive
                  #showAll window
          4 -> do Gtk.treeStoreSetValue store iter 4 notActive
                  #showAll window
          _ -> return ()
      else return ()

  ----------------------------------------------------------------------------------------------------------------------------
  -- Event Bindings for the MenuItems  ---------------------------------------------------------------------------------------
  ----------------------------------------------------------------------------------------------------------------------------

  -- File Menu Items ---------------------------------------------------------------------------------------------------------

  -- new project
  on newm #activate $ do
    continue <- confirmOperation window store changedProject currentState nacInfoMap fileName storeIORefs
    if continue
      then do
        Gtk.treeStoreClear store
        initStore store
        initTreeView treeview
        writeIORef currentState emptyState
        writeIORef undoStack $ M.fromList [(a, []) | a <- [0..2]]
        writeIORef redoStack $ M.fromList [(a, []) | a <- [0..2]]
        writeIORef fileName Nothing
        writeIORef currentPath [0]
        writeIORef currentGraphType 1
        writeIORef currentGraph 0
        writeIORef graphStates $ M.fromList [(a, emptyState) | a <- [0..2]]
        writeIORef lastSavedState M.empty
        writeIORef changedProject False
        writeIORef changedGraph [False]
        writeIORef nacInfoMap M.empty
        writeIORef mergeMapping Nothing
        set window [#title := "Verigraph-GUI"]
        Gtk.widgetQueueDraw canvas
      else return ()

  -- open project
  on opn #activate $ do
    continue <- confirmOperation window store changedProject currentState nacInfoMap fileName storeIORefs
    if continue
      then do
        mg <- loadFile window
        case mg of
          Nothing -> return ()
          Just (forest,fn) -> do
                writeIORef graphStates M.empty
                Gtk.treeStoreClear store
                let toGSandStates n = case n of
                              Topic name -> ((name,False,0,0,False), (0,(-1,emptyState)))
                              TypeGraph id name es -> ((name,False,id,1,True), (1, (id,es)))
                              HostGraph id name es -> ((name,False,id,2,True), (2, (id,es)))
                              RuleGraph id name es a -> ((name,False,id,3,a), (3, (id,es)))
                              NacGraph id name _ -> ((name,False,id,4,True), (4,(id,emptyState)))
                let toNACInfos n = case n of
                              NacGraph id name nacInfo -> (id,nacInfo)
                              _ -> (0,(DG.empty,(M.empty,M.empty)))
                    infoForest = map (fmap toGSandStates) forest
                    nameForest = map (fmap fst) infoForest
                    statesForest = map (fmap snd) infoForest
                    statesList = map snd . filter (\st-> fst st /= 0) . concat . map Tree.flatten $ statesForest
                    nacInfos = filter (\ni -> fst ni /= 0). concat . map Tree.flatten $ map (fmap toNACInfos) forest
                let putInStore (Tree.Node (name,c,i,t,a) fs) mparent = do
                        iter <- Gtk.treeStoreAppend store mparent
                        storeSetGraphStore store iter (name,c,i,t,a,True)
                        case t of
                          0 -> mapM_ (\n -> putInStore n (Just iter)) fs
                          3 -> mapM_ (\n -> putInStore n (Just iter)) fs
                          _ -> return ()
                mapM (\n -> putInStore n Nothing) nameForest
                let (i,es) = if length statesList > 0 then statesList!!0 else (0,emptyState)
                writeIORef currentState es
                writeIORef undoStack $ M.fromList [(i,[]) | i <- [0 .. (maximum $ map fst statesList)]]
                writeIORef redoStack $ M.fromList [(i,[]) | i <- [0 .. (maximum $ map fst statesList)]]
                let statesMap = M.fromList statesList
                writeIORef graphStates statesMap
                writeIORef lastSavedState $ M.map (\es -> (stateGetGraph es, stateGetGI es)) statesMap
                writeIORef fileName $ Just fn
                writeIORef currentGraph i
                writeIORef currentPath [0]
                writeIORef currentGraphType 1
                writeIORef mergeMapping Nothing
                writeIORef nacInfoMap $ M.fromList nacInfos
                p <- Gtk.treePathNewFromIndices [0]
                Gtk.treeViewExpandToPath treeview p
                namesCol <- Gtk.treeViewGetColumn treeview 1
                Gtk.treeViewSetCursor treeview p namesCol False
                afterSave store window graphStates changesIORefs fileName
                updateTG currentState typeGraph possibleNodeTypes possibleEdgeTypes possibleSelectableEdgeTypes graphStates nodeTypeCBox edgeTypeCBox store
                Gtk.widgetQueueDraw canvas
      else return ()

  -- save project
  on svn #activate $ do
    storeCurrentES window currentState storeIORefs nacInfoMap
    context <- Gtk.widgetGetPangoContext canvas
    updateAllNacs store graphStates nacInfoMap context
    structs <- getStructsToSave store graphStates nacInfoMap
    saved <- saveFile structs fileName window
    if saved
      then do afterSave store window graphStates changesIORefs fileName
      else return ()

  -- save project as
  sva `on` #activate $ do
    storeCurrentES window currentState storeIORefs nacInfoMap
    context <- Gtk.widgetGetPangoContext canvas
    updateAllNacs store graphStates nacInfoMap context
    structs <- getStructsToSave store graphStates nacInfoMap
    saved <- saveFileAs structs fileName window
    if saved
      then afterSave store window graphStates changesIORefs fileName
      else return ()

  -- Edit Menu ---------------------------------------------------------------------------------------------------------------
  -- delete item
  on del #activate $ do
    es <- readIORef currentState
    gtype <- readIORef currentGraphType
    case gtype of
      4 -> do
        -- remove elements from nacg
        index <- readIORef currentGraph
        nacInfoM <- readIORef nacInfoMap
        let ((nacg,nacgi), mapping) = fromJust $ M.lookup index nacInfoM
            selected = stateGetSelected es
            nacES = stateSetSelected selected . stateSetGraph nacg . stateSetGI nacgi $ emptyState
            nacES' = deleteSelected nacES
            nacg' = stateGetGraph nacES'
            nacgi' = stateGetGI nacES'
        modifyIORef nacInfoMap $ M.insert index ((nacg',nacgi'), mapping)
        -- add mergeMapping information to undo stack
        mergeM <- readIORef mergeMapping
        stackUndo undoStack redoStack currentGraph es mergeM
      _ -> stackUndo undoStack redoStack currentGraph es Nothing
    modifyIORef currentState (\es -> deleteSelected es)
    setChangeFlags window store changedProject changedGraph currentPath currentGraph True
    updateByType
    Gtk.widgetQueueDraw canvas

  -- undo
  on undo #activate $ do
    currGraph <- readIORef currentGraph
    uStack <- readIORef undoStack >>= return . fromMaybe [] . M.lookup currGraph
    case uStack of
      [] -> return ()
      _  -> do
        -- apply undo
        applyUndo undoStack redoStack currentGraph currentState mergeMapping
        -- reset nac diagraph
        gtype <- readIORef currentGraphType
        if gtype == 4
          then updateNacInfo nacInfoMap currentGraph mergeMapping currentState
          else return ()

        -- indicate changes
        index <- readIORef currentGraph
        es <- readIORef currentState
        sst <- readIORef lastSavedState
        let (g,gi) = (stateGetGraph es, stateGetGI es)
            x = fromMaybe DG.empty $ M.lookup index sst
        setChangeFlags window store changedProject changedGraph currentPath currentGraph $ not (isDiaGraphEqual (g,gi) x)
        Gtk.widgetQueueDraw canvas
        updateByType

  -- redo
  on redo #activate $ do
    currGraph <- readIORef currentGraph
    rStack <- readIORef redoStack >>= return . fromMaybe [] . M.lookup currGraph
    case rStack of
      [] -> return ()
      _ -> do
        -- apply redo
        applyRedo undoStack redoStack currentGraph currentState mergeMapping
        -- change nac diagraph
        gtype <- readIORef currentGraphType
        if gtype == 4
          then updateNacInfo nacInfoMap currentGraph mergeMapping currentState
          else return ()
        -- indicate changes
        index <- readIORef currentGraph
        sst <- readIORef lastSavedState
        es <- readIORef currentState
        let (g,gi) = (stateGetGraph es, stateGetGI es)
        let x = fromMaybe DG.empty $ M.lookup index sst
        setChangeFlags window store changedProject changedGraph currentPath currentGraph $ not (isDiaGraphEqual (g,gi) x)
        Gtk.widgetQueueDraw canvas
        updateByType

  -- copy
  on cpy #activate $ do
    es <- readIORef currentState
    let copy = copySelected es
    writeIORef clipboard $ copy

  -- paste
  on pst #activate $ do
    es <- readIORef currentState
    clip <- readIORef clipboard
    gtype <- readIORef currentGraphType
    case gtype of
      4 -> do
        mergeM <- readIORef mergeMapping
        stackUndo undoStack redoStack currentGraph es mergeM
      _ -> stackUndo undoStack redoStack currentGraph es Nothing
    setChangeFlags window store changedProject changedGraph currentPath currentGraph True
    modifyIORef currentState (pasteClipBoard clip)
    Gtk.widgetQueueDraw canvas
    updateByType

  -- cut
  on cut #activate $ do
    es <- readIORef currentState
    gtype <- readIORef currentGraphType
    writeIORef clipboard $ copySelected es
    modifyIORef currentState (\es -> deleteSelected es)
    gtype <- readIORef currentGraphType
    case gtype of
      4 -> do
        mergeM <- readIORef mergeMapping
        stackUndo undoStack redoStack currentGraph es mergeM
      _ -> stackUndo undoStack redoStack currentGraph es Nothing
    setChangeFlags window store changedProject changedGraph currentPath currentGraph  True
    Gtk.widgetQueueDraw canvas
    updateByType

  -- merge elements in NACs
  on mrg #activate $ do
    gtype <- readIORef currentGraphType
    if (gtype /= 4)
      then return ()
      else do
        es <- readIORef currentState
        context <- Gtk.widgetGetPangoContext canvas
        path <- readIORef currentPath
        tg <- readIORef typeGraph

        -- load NAC
        index <- readIORef currentGraph
        nacInfoM <- readIORef nacInfoMap
        let nacInfo = fromMaybe (DG.empty, (M.empty,M.empty)) $ M.lookup index nacInfoM

        -- merge elements
        merging <- mergeNACElements es nacInfo tg context

        case merging of
          Nothing -> return ()
          Just (((ng,ngi),mergeM),es') -> do
            -- modify IORefs and update the UI
            modifyIORef nacInfoMap $ M.insert index ((ng,ngi),mergeM)
            writeIORef mergeMapping $ Just (mergeM)
            writeIORef currentState $ es'
            stackUndo undoStack redoStack currentGraph es (Just $ snd nacInfo)
            Gtk.widgetQueueDraw canvas
            updateInspector currentGraphType currentState  mergeMapping selectableTypesIORefs
                            currentC currentLC
                            typeInspWidgets hostInspWidgets ruleInspWidgets nacInspWidgets
                            (nodeTypeBox, edgeTypeBox)

  -- split elements in NACs
  on spt #activate $ do
    gtype <- readIORef currentGraphType
    if (gtype /= 4)
      then return ()
      else do
        es <- readIORef currentState
        tg <- readIORef typeGraph
        context <- Gtk.widgetGetPangoContext canvas

        -- load lhs
        path <- readIORef currentPath
        (lhsg, lhsgi) <- getParentLHSDiaGraph store path graphStates

        -- load nac information
        index <- readIORef currentGraph
        nacInfoM <- readIORef nacInfoMap
        let ((nacg, nacgi), (nM,eM)) = fromMaybe (DG.empty, (M.empty,M.empty)) $ M.lookup index nacInfoM

        -- split elements
        ((nacdg',mergeM'),es') <- splitNACElements es ((nacg,nacgi),(nM,eM)) (lhsg, lhsgi) tg context

        -- update IORefs
        modifyIORef nacInfoMap (M.insert index (nacdg', mergeM'))
        writeIORef mergeMapping $ Just mergeM'
        writeIORef currentState es'
        stackUndo undoStack redoStack currentGraph es (Just (nM,eM))
        Gtk.widgetQueueDraw canvas

  return (mainPane, canvas, currentState)






---------------------------------------------------------------------------------------------------------------------------------
-- Auxiliar functions for Callbacks  --------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------


drawGraphByType :: IORef GraphState
                -> IORef (Graph Info Info)
                -> IORef (Maybe (Double,Double,Double,Double) )
                -> IORef Int32
                -> IORef (Maybe MergeMapping)
                -> Cairo.Context
                -> IO Bool
drawGraphByType currentState typeGraph squareSelection currentGraphType mergeMapping context =
  do
    es <- readIORef currentState
    sq <- readIORef squareSelection
    t <- readIORef currentGraphType
    case t of
      1 -> renderWithContext context $ drawTypeGraph es sq
      2 -> do
        tg <- readIORef typeGraph
        renderWithContext context $ drawHostGraph es sq tg
      3 -> do
        tg <- readIORef typeGraph
        renderWithContext context $ drawRuleGraph es sq tg
      4 -> do
        tg <- readIORef typeGraph
        mm <- readIORef mergeMapping >>= return . fromMaybe (M.empty, M.empty)
        renderWithContext context $ drawNACGraph es sq tg mm
      _ -> return ()
    return False


-- create a node or n edges and return if the
createNodesOrEdgesCallback :: Gtk.DrawingArea -> Gtk.Entry -> Gtk.CheckButton -> Gtk.CheckButton
                           -> IORef GraphState -> IORef Int32 -> IORef Int32 -> IORef (Graph Info Info) -> IORef (Maybe MergeMapping)
                           -> IORef (Maybe String) -> IORef (M.Map String (NodeGI,Int32)) -> IORef (Maybe String) -> IORef (M.Map String (M.Map (String, String) EdgeGI, Int32))
                           -> IORef NodeShape -> IORef EdgeStyle -> IORef GIColor -> IORef GIColor
                           -> Gdk.EventButton
                           -> IO Bool
createNodesOrEdgesCallback canvas nameEntry autoLabelNCheckBtn autoLabelECheckBtn
                           currentState currentGraph currentGraphType typeGraph mergeMapping
                           currentNodeType possibleNodeTypes currentEdgeType possibleEdgeTypes
                           currentShape currentStyle currentC currentLC
                           eventButton  =
  do
    x <- get eventButton #x
    y <- get eventButton #y
    es <- readIORef currentState
    let z = stateGetZoom es
        (px,py) = stateGetPan es
        (x',y') = (x/z - px, y/z - py)
    -- add the current state to the undo stack
    mergeM <- readIORef mergeMapping
    let g = stateGetGraph es
        gi = stateGetGI es
        dstNode = selectNodeInPosition gi (x',y')
    context <- Gtk.widgetGetPangoContext canvas
    case (dstNode) of
      -- no selected node: create node
      Nothing -> do
        auto <- Gtk.toggleButtonGetActive autoLabelNCheckBtn
        createNodeCallback currentGraphType currentState currentNodeType possibleNodeTypes currentShape currentC currentLC (x',y') auto context
      -- one node selected: create edges targeting this node
      Just nid -> do
        auto <- Gtk.toggleButtonGetActive autoLabelECheckBtn
        createEdgeCallback typeGraph currentGraphType currentState currentEdgeType possibleEdgeTypes currentStyle currentLC auto nid


createNodeCallback :: IORef Int32
                   -> IORef GraphState
                   -> IORef (Maybe String)
                   -> IORef (M.Map String (NodeGI, Int32))
                   -> IORef NodeShape
                   -> IORef GIColor
                   -> IORef GIColor
                   -> GIPos
                   -> Bool
                   -> P.Context
                   -> IO Bool
createNodeCallback currentGraphType currentState currentNodeType possibleNodeTypes currentShape currentC currentLC (x,y) auto context =
  do
    gType <- readIORef currentGraphType
    case gType of
      0 -> return False
      1 -> do
        cShape <- readIORef currentShape
        cColor <- readIORef currentC
        cLColor <- readIORef currentLC
        createNode' currentState I.empty True (x,y) cShape cColor cLColor context
        return True
      _ -> do
        (t, shape, c, lc) <- getNodeLayoutPrimitives currentNodeType possibleNodeTypes currentShape currentC currentLC
        createNode' currentState (infoSetType I.empty t) auto (x,y) shape c lc context
        return True

createEdgeCallback :: IORef (Graph Info Info) -> IORef Int32 -> IORef GraphState
                   -> IORef (Maybe String) -> IORef (M.Map String (M.Map (String, String) EdgeGI, Int32))
                   -> IORef EdgeStyle -> IORef GIColor
                   -> Bool
                   -> NodeId
                   -> IO Bool
createEdgeCallback typeGraph currentGraphType currentState currentEdgeType possibleEdgeTypes currentStyle currentLC auto nid =
  do
    gType <- readIORef currentGraphType
    case gType of
      0 -> return False
      1 -> do
        es <- readIORef currentState
        estyle <- readIORef currentStyle
        color <- readIORef currentLC
        modifyIORef currentState (\es -> createEdges es nid I.empty True estyle color)
        return True
      _ -> do
        tg <- readIORef typeGraph
        es <- readIORef currentState
        metype <- readIORef currentEdgeType

        -- create edges infering their types
        let sNids = fst $ stateGetSelected es
            g = stateGetGraph es
            srcNodes = catMaybes $ map (\nid -> G.lookupNode nid g) sNids
            tgtNode = fromJust $ G.lookupNode nid g
            tgtType = infoType $ nodeInfo tgtNode
            edgesTs = map (\src -> (src, infereEdgeType tg src tgtNode metype)) srcNodes

        (es', createdEdges) <- foldM
              (\(es,eids) (src, mt) -> do
                            (t,estyle,color) <- getEdgeLayoutPrimitives mt (infoType $ nodeInfo src) tgtType possibleEdgeTypes currentStyle currentLC
                            let srcId = nodeId src
                                es' = createEdge es Nothing srcId nid (infoSetType I.empty t) auto estyle color
                                eids' = (snd $ stateGetSelected es') ++ eids
                            return (es',eids'))
              (es,[])
              edgesTs

        writeIORef currentState $ stateSetSelected ([],createdEdges) es'
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
    mergeM <- readIORef mergeMapping
    let leftButton = Gdk.ModifierTypeButton1Mask `elem` ms
        (sNodes, sEdges) = stateGetSelected es
    if leftButton && ((length sNodes) + (length sEdges) > 0) then
      do
        mv <- readIORef movingGI
        if not mv then
          do
            writeIORef movingGI True
            indicateChanges window store storeIORefs changesIORefs undoStack redoStack mergeM es
        else
          return ()
    else
      return ()

    return True


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


setNewColor :: Gtk.DrawingArea -> Gtk.ColorButton -> IORef GIColor -> Bool -> IORef GraphState -> IO ()
setNewColor canvas colorBtn colorIORef isLineC currentState =
  do
    gtkcolor <- Gtk.colorChooserGetRgba colorBtn
    r <- get gtkcolor #red
    g <- get gtkcolor #green
    b <- get gtkcolor #blue
    let color = (r,g,b)
    writeIORef colorIORef color

    es <- readIORef currentState
    let (nds,eds) = stateGetSelected es
        (ngiM,egiM) = stateGetGI es
        newGI = case (isLineC, null nds && null eds, null nds) of
                  (True,False,_) ->
                      let newngiM = M.mapWithKey (\k ngi -> if NodeId k `elem` nds then ngi {lineColor = color} else ngi) ngiM
                          newegiM = M.mapWithKey (\k egi -> if EdgeId k `elem` eds then egi {color = color} else egi) egiM
                      in Just (newngiM,newegiM)
                  (False,_,False) ->
                      let newngiM = M.mapWithKey (\k ngi -> if NodeId k `elem` nds then ngi {fillColor = color} else ngi) ngiM
                      in Just (newngiM,egiM)
                  _ -> Nothing
    case newGI of
      Nothing -> return ()
      Just newgi -> do
        modifyIORef currentState $ stateSetGI newgi
        Gtk.widgetQueueDraw canvas



setNewShape :: Gtk.RadioButton -> Gtk.DrawingArea -> NodeShape -> IORef NodeShape -> IORef GraphState -> IO Bool
setNewShape radioBtn canvas newShape currentShape currentState =
  do
    active <- get radioBtn #active
    es <- readIORef currentState
    let nds = fst $ stateGetSelected es
        giM = fst $ stateGetGI es
    if not active || M.null (M.filterWithKey (\k gi -> NodeId k `elem` nds && shape gi /= newShape) giM)
      then return False
      else do
        writeIORef currentShape newShape
        modifyIORef currentState (\es -> changeNodeShape es newShape)
        Gtk.widgetQueueDraw canvas
        return True

setNewStyle :: Gtk.RadioButton -> Gtk.DrawingArea -> EdgeStyle -> IORef EdgeStyle -> IORef GraphState -> IO Bool
setNewStyle radioBtn canvas newStyle currentStyle currentState =
  do
    active <- get radioBtn #active
    es <- readIORef currentState
    let edgs = snd $ stateGetSelected es
        giM = snd $ stateGetGI es
    if not active || M.null (M.filterWithKey (\k gi -> EdgeId k `elem` edgs && style gi /= newStyle) giM)
      then return False
      else do
        writeIORef currentStyle newStyle
        modifyIORef currentState (\es -> changeEdgeStyle es newStyle)
        Gtk.widgetQueueDraw canvas
        return True




---------------------------------------------------------------------------------------------------------------------------------
--  Auxiliar Functions  ---------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------


-- indicate if the was changes in the project and add 'oldSt' to the undoStack
indicateChanges :: Gtk.Window -> Gtk.TreeStore -> StoreIORefs -> ChangesIORefs
                -> IORef (M.Map Int32 ChangeStack) -> IORef (M.Map Int32 ChangeStack) -> Maybe MergeMapping -> GraphState -> IO ()
indicateChanges window store (_,currentPath,currentGraph,currentGraphType) (changedProject, changedGraph, _)
                undoStack redoStack oldMergeM oldSt =
  do
    setChangeFlags window store changedProject changedGraph currentPath currentGraph True
    stackUndo undoStack redoStack currentGraph oldSt oldMergeM

getNodeLayoutPrimitives :: IORef (Maybe String)
                        -> IORef (M.Map String (NodeGI, Int32))
                        -> IORef NodeShape
                        -> IORef GIColor
                        -> IORef GIColor
                        -> IO (String, NodeShape, GIColor, GIColor)
getNodeLayoutPrimitives currentNodeType possibleNodeTypes currentShape currentC currentLC =
      do
        cShape <- readIORef currentShape
        cColor <- readIORef currentC
        cLColor <- readIORef currentLC
        mntype <- readIORef currentNodeType
        case mntype of
          Nothing -> return ("", cShape, cColor, cLColor)
          Just t -> do
            possibleNT <- readIORef possibleNodeTypes
            let possibleNT' = M.map (\(gi,i) -> gi) possibleNT
                mngi = M.lookup t possibleNT'
            case mngi of
              Nothing -> return ("", cShape, cColor, cLColor)
              Just gi -> return (t, shape gi, fillColor gi, lineColor gi)


-- auxiliar function that checks if the type mt, exist in possibleEdgeTypes.
-- returns the correspondent triple (type, style, color)
getEdgeLayoutPrimitives :: Maybe String
                        -> String
                        -> String
                        -> IORef (M.Map String (M.Map (String, String) EdgeGI, Int32))
                        -> IORef EdgeStyle
                        -> IORef GIColor
                        -> IO (String,EdgeStyle,GIColor)
getEdgeLayoutPrimitives mt srcType tgtType possibleEdgeTypes currentStyle currentLC = do
        pet <- readIORef possibleEdgeTypes
        pet' <- return $ M.map fst pet
        cEstyle <- readIORef currentStyle
        cColor <- readIORef currentLC
        return $ case mt of
            Nothing -> ("", cEstyle, cColor)
            Just t -> let megi = M.lookup t pet'
                      in case megi of
                            Nothing -> ("", cEstyle, cColor)
                            Just sm -> case M.lookup (srcType,tgtType) sm of
                                          Nothing -> ("", cEstyle, cColor)
                                          Just gi -> (t, style gi, color gi)


updateNacInfo :: IORef (M.Map Int32 NacInfo) -> IORef Int32 -> IORef (Maybe MergeMapping) -> IORef GraphState -> IO ()
updateNacInfo nacInfoMap currentGraph mergeMapping currentState = do
      mergeM <- readIORef mergeMapping >>= return . fromJust
      st <- readIORef currentState
      let g = stateGetGraph st
          gi = stateGetGI st
          nacInfo = extractNac g gi mergeM
      index <- readIORef currentGraph
      modifyIORef nacInfoMap $ M.insert index nacInfo




-- auxiliar function to check if the project was changed
-- it does the checking and if no, ask the user if them want to save.
-- returns True if there's no changes, if the user don't wanted to save or if he wanted and the save operation was successfull
-- returns False if the user wanted to save and the save operation failed or opted to cancel.
confirmOperation :: Gtk.Window
                 -> Gtk.TreeStore
                 -> IORef Bool
                 -> IORef GraphState
                 -> IORef (M.Map Int32 (DiaGraph, MergeMapping))
                 -> IORef (Maybe String)
                 -> StoreIORefs
                 -> IO Bool
confirmOperation window store changedProject currentState nacInfoMap fileName storeIORefs@(graphStates,_,_,_)= do
  changed <- readIORef changedProject
  response <- if changed
    then createConfirmDialog window "The project was changed, do you want to save?"
    else return Gtk.ResponseTypeNo
  case response of
    Gtk.ResponseTypeCancel -> return False
    Gtk.ResponseTypeNo -> return True
    Gtk.ResponseTypeYes -> do
      storeCurrentES window currentState storeIORefs nacInfoMap
      structs <- getStructsToSave store graphStates nacInfoMap
      saveFile structs fileName window -- returns True if saved the file
    _ -> return False

prepToExport :: Gtk.TreeStore
             -> IORef (M.Map Int32 GraphState)
             -> IORef (M.Map Int32 (DiaGraph, MergeMapping))
             -> IO (Either String (Grammar (TGM.TypedGraphMorphism Info Info)))
prepToExport store graphStates nacInfoMap = do
  sts <- readIORef graphStates

  let tg = stateGetGraph . fromJust $ M.lookup 0 sts
      hg = stateGetGraph . fromJust $ M.lookup 1 sts

  rules <- getRules store graphStates nacInfoMap
  let rulesNames = map (\(_,_,name) -> name) rules
      rulesNnacs = map (\(r,ns,_) -> (r,ns)) rules

  let efstOrderGG = makeGrammar tg hg rulesNnacs rulesNames
  return efstOrderGG







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
                                            let srcType = infoLabelStr . nodeInfo . fromJust $ lookupNode s g
                                                tgtType = infoLabelStr . nodeInfo . fromJust $ lookupNode t g
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
  let defaultAction = do
        writeIORef possibleSelectableEdgeTypes pET
        updateComboBoxText edgeTypeCBox (map T.pack $ M.keys pET)
  case (G.null tg, sEdges) of
    -- typegraph is null -> impossible to do anything
    (True,_) -> return ()
    -- no edge selected -> all edge types are selectable
    (False, []) -> defaultAction
    -- one edge selected -> modify comboboxes to show what kind of types this edge can have
    (False,[eid]) -> do
      let g = stateGetGraph es
      case lookupEdge eid g of
        Just e -> do
          let
            mSrc = lookupNode (sourceId e) g
            mTgt = lookupNode (targetId e) g
          case (mSrc,mTgt) of
            (Just src, Just tgt) -> do
              let
                possibleTypes = listPossibleEdgeTypes tg src tgt
                pEST = M.filterWithKey (\k _ -> k `elem` possibleTypes) pET
                posF acc v = (acc+1,(fst v,acc))
                pEST' = snd $ M.mapAccum posF 0 pEST
              writeIORef possibleSelectableEdgeTypes pEST'
              updateComboBoxText edgeTypeCBox (map T.pack $ M.keys pEST')
            _ -> do
              putStrLn "Error: lookupNode returning Nothing"
              print e
              print g
              defaultAction
        _ -> do
          putStrLn "Error: lookupEdge returning Nothing"
          print eid
          print g
          defaultAction
    -- many edges selected
    (False,eids) -> do
      let
        g = stateGetGraph es
        edgs = catMaybes $ map (\eid -> lookupEdge eid g) eids
        checkEndings e ends =
          case ends of
            (False,_) -> (False,Nothing)
            (True,Nothing) -> dfRes
            (True,Just (s,t)) ->
              let
                srcMatch = case mSrc of
                  Nothing -> False
                  Just src -> (infoType $ nodeInfo s) == (infoType $ nodeInfo src)
                tgtMatch = case mTgt of
                  Nothing -> False
                  Just tgt -> (infoType $ nodeInfo t) == (infoType $ nodeInfo tgt)
              in (srcMatch && tgtMatch, Just (s,t))
          where
            mSrc = lookupNode (sourceId e) g
            mTgt = lookupNode (targetId e) g
            dfRes = case (mSrc,mTgt) of
                      (Just s, Just t) -> (True,Just (s,t))
                      _ -> (False,Nothing)

        endings = foldr checkEndings (True,Nothing) edgs
      case endings of
        -- all selected edges have source and target nodes of same types ->
        --      modify comboboxes to show what kind of types these edges can have
        (True,Just (src,tgt)) -> do
          let
            possibleTypes = listPossibleEdgeTypes tg src tgt
            pEST = M.filterWithKey (\k _ -> k `elem` possibleTypes) pET
            posF acc v = (acc+1,(fst v,acc))
            pEST' = snd $ M.mapAccum posF 0 pEST
          writeIORef possibleSelectableEdgeTypes pEST'
          updateComboBoxText edgeTypeCBox (map T.pack $ M.keys pEST')
        -- the selected edges have source and target nodes of different types ->
        --      all edge types are selectable
        (False,_) -> defaultAction

-- create a new node, auto-generating it's name and dimensions
createNode' :: IORef GraphState -> Info -> Bool -> GIPos -> NodeShape -> GIColor -> GIColor -> P.Context ->  IO NodeId
createNode' currentState info autoNaming pos nshape color lcolor context = do
  es <- readIORef currentState
  let nid = head $ newNodes (stateGetGraph es)
      info' = if infoLabelStr info == "" && autoNaming then infoSetLabel info (show $ fromEnum nid) else info
  dim <- getStringDims (infoVisible info') context Nothing
  writeIORef currentState $ createNode es pos dim info' nshape color lcolor

  return nid


-- auxiliar function to prepare the treeStore to save
-- auxiliar function, add the current editor state in the graphStates list
storeCurrentES :: Gtk.Window -> IORef GraphState -> StoreIORefs -> IORef (M.Map Int32 (DiaGraph, MergeMapping)) -> IO ()
storeCurrentES window currentState (graphStates, _, currentGraph, currentGraphType) nacInfoMap = do
  es <- readIORef currentState
  index <- readIORef currentGraph
  modifyIORef graphStates $ M.insert index es
  gtype <- readIORef currentGraphType
  -- if the current graph is a NAC, then update the nacInfo
  if gtype == 4
    then do
      nacInfo <- readIORef nacInfoMap >>= return . M.lookup index
      case nacInfo of
        Nothing -> showError window (T.pack $ "error: could not retrieve nacInfo of nac" ++ (show index))
        Just ((ng,_), mergeM) -> do
          let nacGI = extractNacGI (stateGetGraph es) (stateGetGI es) mergeM
          modifyIORef nacInfoMap $ M.insert index ((ng,nacGI),mergeM)
    else return ()


afterSave :: Gtk.TreeStore
          -> Gtk.Window
          -> IORef (M.Map Int32 GraphState)
          -> ChangesIORefs
          -> IORef (Maybe String)
          -> IO ()
afterSave store window graphStates (changedProject, changedGraph, lastSavedState) fileName  =
        do
          -- update changes IORefs so that the project appear as not changed
          writeIORef changedProject False
          states <- readIORef graphStates
          writeIORef changedGraph (take (length states) (repeat False))
          writeIORef lastSavedState (M.map (\es -> (stateGetGraph es, stateGetGI es)) states)
          -- clean the changed flag foreach graph in treeStore
          gvChanged <- toGValue False
          Gtk.treeModelForeach store $ \model path iter -> do
            Gtk.treeStoreSetValue store iter 1 gvChanged
            return False
          indicateProjChanged window False
          filename <- readIORef fileName
          case filename of
            Nothing -> set window [#title := "Verigraph-GUI"]
            Just fn -> set window [#title := T.pack ("Verigraph-GUI - " ++ fn)]
