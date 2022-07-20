{-|
  This module provides the functions used as callbacks for the Graph Grammar editor
-}

{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module GUI.Editor(
  startEditor
, confirmOperation
, storeCurrentES
, afterSave
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
import           GUI.Data.Diagram hiding (empty)
import qualified GUI.Data.Diagram as DG
import           GUI.Data.GraphState
import           GUI.Data.GraphicalInfo
import           GUI.Data.Info hiding (empty)
import qualified GUI.Data.Info as I
import           GUI.Data.Nac
import           GUI.Helper.Dialogs
import           GUI.Helper.BasicCanvasCallbacks
import           GUI.Helper.Geometry
import           GUI.Helper.GrammarMaker
import           GUI.Helper.GraphicalInfo
import           GUI.Helper.GraphValidation
import           GUI.Helper.List
import           GUI.Helper.GrammarConverter
import           GUI.Helper.SaveLoad
import           GUI.Render.Render
import           GUI.Render.GraphElements
import           GUI.Render.GraphDraw


-- Editor modules
import GUI.Editor.Helper.Callbacks
import GUI.Editor.Helper.Clipboard
import GUI.Editor.Helper.Nac
import GUI.Editor.Helper.TypeInfer
import GUI.Editor.Helper.TreeStore
import GUI.Editor.Helper.UndoRedo
import GUI.Editor.UI.UIBuilders
import GUI.Editor.UI.UpdateInspector

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
               -> IO (Gtk.Paned, Gtk.DrawingArea, Gtk.TreeView, IORef GraphState, IORef (M.Map Int32 ChangeStack), IORef (M.Map Int32 ChangeStack))
startEditor window store
            fileName typeGraph
            storeIORefs changesIORefs nacIORefs
            fileItems editItems viewItems
            focusedCanvas focusedStateIORef = do

  -- create the ui of the editor
  (mainPane, treeview, ruleButtons, canvas, nameEntry, entryLabel, layoutWidgets, typeSelectionWidgets) <- buildEditor

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
  (nameRenderer, activeRenderer) <- configureTreeView treeview
  let [createRBtn, createNBtn, removeBtn] = ruleButtons
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
  clipboard       <- newIORef DG.empty -- clipboard - Diagram

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

  let selectableTypesIORefs = (possibleNodeTypes, possibleSelectableEdgeTypes, currentNodeType, currentEdgeType)


  -- "unpack" IORefs
  let (graphStates,currentPath,currentGraph,currentGraphType) = storeIORefs
      (changedProject, changedGraph, lastSavedState) = changesIORefs
      (nacInfoMap, mergeMapping) = nacIORefs

  -- set IORefs to make the zoom controls work on the canvas
  writeIORef focusedCanvas $ Just canvas
  writeIORef focusedStateIORef $ Just currentState


  ----------------------------------------------------------------------------------------------------------------------------
  -- Event Bindings for the Canvas -------------------------------------------------------------------------------------------
  ----------------------------------------------------------------------------------------------------------------------------

  -- drawing event
  on canvas #draw $ drawGraphByType canvas currentState typeGraph squareSelection currentGraphType mergeMapping

  -- mouse button pressed on canvas
  on canvas #buttonPressEvent $
    canvasButtonPressedCallback
      canvas window nameEntry autoLabelNCheckBtn autoLabelECheckBtn
      nodeTypeCBox edgeTypeCBox
      store storeIORefs
      currentState typeGraph nacInfoMap mergeMapping
      changesIORefs undoStack redoStack
      currentNodeType possibleNodeTypes currentEdgeType possibleEdgeTypes possibleSelectableEdgeTypes
      currentShape currentStyle currentC currentLC
      oldPoint squareSelection

  -- mouse motion on canvas
  -- set callback to move elements or expand the square selection area
  on canvas #motionNotifyEvent $
    basicCanvasMotionCallBack currentState oldPoint squareSelection canvas
  -- if the left button is hold while moving, indicate changes and add the previous diagram to the undo stack
  on canvas #motionNotifyEvent $
    indicateChangesWhenMovingElements window store movingGI currentState mergeMapping undoStack redoStack storeIORefs changesIORefs

  on canvas #buttonReleaseEvent $
    canvasButtonReleasedCallback
      canvas
      currentGraphType currentState typeGraph mergeMapping
      selectableTypesIORefs possibleEdgeTypes edgeTypeCBox
      squareSelection movingGI
      currentC currentLC
      typeInspWidgets hostInspWidgets ruleInspWidgets nacInspWidgets
      (nodeTypeBox, edgeTypeBox)


  -- mouse wheel scroll on canvas
  on canvas #scrollEvent $ basicCanvasScrollCallback currentState canvas

  -- keyboard
  on canvas #keyPressEvent $ canvasKeyPressCallback nameEntry del

  -- if the canvas is focused, then set this canvas to respond to the events of the view menu, like zoom in and zoom out
  on canvas #focusInEvent $ \event -> do
    writeIORef focusedCanvas $ Just canvas
    writeIORef focusedStateIORef $ Just currentState
    return False

  ----------------------------------------------------------------------------------------------------------------------------
  -- Event Bindings for the Inspector Panel  ---------------------------------------------------------------------------------
  ----------------------------------------------------------------------------------------------------------------------------

  -- if the user press enter, then change focus to the canvas so that the selected elements are renamed
  on nameEntry #keyPressEvent $
    nameEntryKeyPressedCallback
      canvas window nameEntry
      nodeTypeCBox edgeTypeCBox
      store storeIORefs
      currentState typeGraph nacInfoMap mergeMapping
      changesIORefs undoStack redoStack
      selectableTypesIORefs possibleEdgeTypes
      currentC currentLC
      typeInspWidgets hostInspWidgets ruleInspWidgets nacInspWidgets
      nodeTypeBox edgeTypeBox


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



  -- auxiliar function to change the selected nodes or edges to new shapes or styles according to the toggled RadioButton.
  -- it receives a RadioButton, the new shape or style desired, an IORef with the current shape or style and a function to change the selected elements.
  let
    setSelectedS :: Gtk.RadioButton -> a -> IORef a -> (Gtk.RadioButton -> Gtk.DrawingArea -> a -> IORef a -> IORef GraphState -> IO Bool) -> IO ()
    setSelectedS radioBtn newS currentS setS =
      do
        st <- readIORef currentState
        changed <- setS radioBtn canvas newS currentS currentState
        if changed then
          do indicateChanges window store storeIORefs changesIORefs undoStack redoStack Nothing st
             updateTG currentState typeGraph possibleNodeTypes possibleEdgeTypes possibleSelectableEdgeTypes graphStates nodeTypeCBox edgeTypeCBox store
        else return ()


  -- toogle the radio buttons for node shapes
  -- change the shape of the selected nodes and set the current shape for new nodes
  circleRadioBtn `on` #toggled $ setSelectedS circleRadioBtn NCircle currentShape setNewShape
  rectRadioBtn `on` #toggled $ setSelectedS rectRadioBtn NRect currentShape setNewShape
  squareRadioBtn `on` #toggled $ setSelectedS squareRadioBtn NSquare currentShape setNewShape


  -- toogle the radio buttons for edge styles
  -- change the style of the selected edges and set the current style for new edges

  normalRadioBtn `on` #toggled $  setSelectedS normalRadioBtn ENormal currentStyle setNewStyle
  pointedRadioBtn `on` #toggled $ setSelectedS pointedRadioBtn EPointed currentStyle setNewStyle
  slashedRadioBtn `on` #toggled $ setSelectedS slashedRadioBtn ESlashed currentStyle setNewStyle


  -- choose a type in the type nodeTypeCBox for nodes
  on nodeTypeCBox #changed $ do
    gt <- readIORef currentGraphType
    index <- Gtk.comboBoxGetActive nodeTypeCBox
    case (index<0,gt<2) of
      (True,_) -> return ()
      (False,True) -> do
        typeInfo <- Gtk.comboBoxTextGetActiveText nodeTypeCBox >>= \mt -> return (T.unpack <$> mt)
        writeIORef currentNodeType $ typeInfo
      (False,False) -> do
        typeInfo <- Gtk.comboBoxTextGetActiveText nodeTypeCBox >>= \mt -> return . fromMaybe "" $ (T.unpack <$> mt)
        selectedType <- readIORef possibleNodeTypes >>= return . M.lookup typeInfo
        case selectedType of
          Nothing -> return ()
          Just (typeNGI,_) -> do
            writeIORef currentNodeType $ Just typeInfo
            es <- readIORef currentState
            mergeM <- readIORef mergeMapping
            changed <- changeNodesType typeGraph possibleEdgeTypes currentState typeInfo typeNGI
            when changed $
              do
                when (gt == 4) $
                  updateNacInfo nacInfoMap currentGraph mergeMapping currentState
                indicateChanges window store storeIORefs changesIORefs undoStack redoStack mergeM es
                Gtk.widgetQueueDraw canvas


  -- choose a type in the type comboBox for edges
  on edgeTypeCBox #changed $ do
    gt <- readIORef currentGraphType
    index <- Gtk.comboBoxGetActive edgeTypeCBox
    case (index<0, gt<2) of
      (True,_) -> return ()
      (False,True) -> return ()
      (False,False) -> do
            typeInfo <- Gtk.comboBoxTextGetActiveText edgeTypeCBox >>= \mt -> return . fromMaybe "" $ T.unpack <$> mt
            typeEntry <- readIORef possibleEdgeTypes >>= return . M.lookup typeInfo
            case typeEntry of
              Nothing -> return()
              Just (pET,_) -> do
                writeIORef currentEdgeType $ Just typeInfo
                es <- readIORef currentState
                mergeM <- readIORef mergeMapping
                changed <- changeEdgesType currentState typeInfo pET
                if changed then
                  do
                    if gt == 4
                      then updateNacInfo nacInfoMap currentGraph mergeMapping currentState
                      else return ()
                    indicateChanges window store storeIORefs changesIORefs undoStack redoStack mergeM es
                    Gtk.widgetQueueDraw canvas
                else
                  return ()


  -- choose a operaion in the operation comboBox
  on operationCBox #changed $ do
    gt <- readIORef currentGraphType
    index <- Gtk.comboBoxGetActive operationCBox
    case (index<0 || index > 2, gt/=3) of
      (True,_) -> return ()
      (_,True) -> return ()
      (False,False) -> do
          operationInfo <- return $ case index of
            0 -> Preserve
            1 -> Create
            2 -> Delete
          es <- readIORef currentState
          mergeM <- readIORef mergeMapping
          changed <- changeElementsOperation canvas currentState operationInfo
          if changed then
            do
              if gt == 4
                then updateNacInfo nacInfoMap currentGraph mergeMapping currentState
                else return ()
              indicateChanges window store storeIORefs changesIORefs undoStack redoStack mergeM es
              Gtk.widgetQueueDraw canvas
          else
            return ()


  -- merge or split buttons pressed: merge or split elements in nac
  on mergeBtn #clicked $ Gtk.menuItemActivate mrg
  on splitBtn #clicked $ Gtk.menuItemActivate spt

  ----------------------------------------------------------------------------------------------------------------------------
  -- Event Bindings for the TreeView and TreeStore ---------------------------------------------------------------------------
  ----------------------------------------------------------------------------------------------------------------------------

  -- when the typeGraph row is changed or created, update the typeGraph
  after store #rowChanged $ \path iter -> do
    gid <- Gtk.treeModelGetValue store iter 2 >>= fromGValue :: IO Int32
    t   <- Gtk.treeModelGetValue store iter 3 >>= fromGValue :: IO Int32
    case t of
      1 -> do
        gt <- readIORef currentGraphType
        if gt == t then do
          updateTG currentState typeGraph possibleNodeTypes possibleEdgeTypes possibleSelectableEdgeTypes graphStates nodeTypeCBox edgeTypeCBox store
        else return ()
      _ -> return ()

  -- event: changed the selected graph
  on treeview #cursorChanged $ do
    selection <- Gtk.treeViewGetSelection treeview
    (sel,model,iter) <- Gtk.treeSelectionGetSelected selection
    case sel of
      False -> return ()
      True -> do
        gType <- Gtk.treeModelGetValue model iter 3 >>= fromGValue  :: IO Int32
        path <- Gtk.treeModelGetPath model iter >>= Gtk.treePathGetIndices >>= return . fromMaybe [0]
        if gType /= 0 then do
          -- update the current path
          writeIORef currentPath path
        else return ()

        cIndex <- readIORef currentGraph
        index <- Gtk.treeModelGetValue model iter 2 >>= fromGValue  :: IO Int32
        -- compare the selected graph with the current one and change the graph according to selection
        case (cIndex == index, gType) of
          -- case the index did not change or the graph is a topic, then do nothing
          (True, _)  -> return ()
          (False, 0) -> return ()

          -- case the selection is a NAC, mount the graph with the LHS part, the additional elements and the merge information
          (False, 4) -> do

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


        -- set the GI of new elements to a default and update the elments of a graph according to the current typegraph
        -- when the graph is not the typegraph
        if gType > 1 then do
          writeIORef currentShape NCircle
          writeIORef currentStyle ENormal
          writeIORef currentC (1,1,1)
          writeIORef currentLC (0,0,0)
          updateElements currentState possibleNodeTypes possibleEdgeTypes
        else return ()

        -- draw the graph
        Gtk.widgetQueueDraw canvas

        -- update the inspector pane according to the current graph type
        changeInspector layoutBox typeSelectionBox operationBox mergeBtn splitBtn mrg spt gType

        -- show the create Nac button and the remove button when the current graph is a Rule or Nac
        if gType == 3 || gType == 4 then do
            #show createNBtn
            #show removeBtn
            case gType of
              3 -> set removeBtn [#label := T.pack "Remove Rule"]
              4 -> set removeBtn [#label := T.pack "Remove NAC"]
        else do
            #hide createNBtn
            #hide removeBtn

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
        currentGraphType
        store graphStates nacInfoMap mergeMapping currentState typeGraph currentGraph currentPath
        possibleNodeTypes possibleEdgeTypes possibleSelectableEdgeTypes
        nodeTypeCBox edgeTypeCBox
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
        setChangeFlags window store changedProject changedGraph currentPath currentGraph $ not (isDiagrEqual (g,gi) x)
        Gtk.widgetQueueDraw canvas
        updateByType
            currentGraphType
            store graphStates nacInfoMap mergeMapping currentState typeGraph currentGraph currentPath
            possibleNodeTypes possibleEdgeTypes possibleSelectableEdgeTypes
            nodeTypeCBox edgeTypeCBox

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
        setChangeFlags window store changedProject changedGraph currentPath currentGraph $ not (isDiagrEqual (g,gi) x)
        Gtk.widgetQueueDraw canvas
        updateByType
            currentGraphType
            store graphStates nacInfoMap mergeMapping currentState typeGraph currentGraph currentPath
            possibleNodeTypes possibleEdgeTypes possibleSelectableEdgeTypes
            nodeTypeCBox edgeTypeCBox

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
        currentGraphType
        store graphStates nacInfoMap mergeMapping currentState typeGraph currentGraph currentPath
        possibleNodeTypes possibleEdgeTypes possibleSelectableEdgeTypes
        nodeTypeCBox edgeTypeCBox

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
        currentGraphType
        store graphStates nacInfoMap mergeMapping currentState typeGraph currentGraph currentPath
        possibleNodeTypes possibleEdgeTypes possibleSelectableEdgeTypes
        nodeTypeCBox edgeTypeCBox

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

  return (mainPane, canvas, treeview, currentState, undoStack, redoStack)






---------------------------------------------------------------------------------------------------------------------------------
-- Auxiliar functions for the canvas callbacks ----------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------




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

changeNodesType :: IORef (Graph Info Info) -> IORef (M.Map String (M.Map (String, String) EdgeGI, Int32)) ->
                  IORef GraphState -> String -> NodeGI -> IO Bool
changeNodesType typeGraph possibleEdgeTypes currentState typeInfo typeNGI = do
          es <- readIORef currentState
          let (sNids,sEids) = stateGetSelected es
              g = stateGetGraph es
              -- foreach selected node, change their types
              unlockedSNids = filter (\nid -> case lookupNode nid g of
                                                  Nothing -> False
                                                  Just n -> not $ infoLocked (nodeInfo n)) sNids
          if length unlockedSNids > 0 then
            do
              let giM = stateGetGI es
                  g' = foldr (\nid g -> updateNodePayload nid g (\info -> infoSetType info typeInfo)) g unlockedSNids
                  newNGI = foldr  (\nid giM -> let ngi = getNodeGI (fromEnum nid) giM
                                               in M.insert (fromEnum nid) (typeNGI {position = position ngi, dims = dims ngi}) giM)
                                  (fst giM)
                                  unlockedSNids
                  es' = stateSetGraph g' . stateSetGI (newNGI, snd giM) . stateSetSelected (unlockedSNids,sEids) $ es

                  -- foreach changed node, change the type of the edges connected to it
              typesE <- readIORef possibleEdgeTypes >>= return . M.map fst
              tg <- readIORef typeGraph
              let es'' = infereEdgesTypesAfterNodeChange es' tg typesE
              writeIORef currentState es''


              return True
          else
            return False

changeEdgesType :: IORef GraphState -> String -> M.Map (String, String) EdgeGI -> IO Bool
changeEdgesType currentState typeInfo pET = do
      es <- readIORef currentState
      let (sNids,sEids) = stateGetSelected es
          g = stateGetGraph es
          giM = stateGetGI es
          unlockedSEids = filter (\eid -> case lookupEdge eid g of
                                                Nothing -> False
                                                Just e -> (not $ infoLocked (edgeInfo e)) &&
                                                          (infoType $ edgeInfo e) /= typeInfo) sEids
          edgesInContext = catMaybes $ map (\eid -> lookupEdgeInContext eid g) unlockedSEids
      if length edgesInContext > 0 then
          do

            let changeEdgeGI ((src,_),e,(tgt,_)) giM =
                      let eid = fromEnum $ edgeId e
                          egi = getEdgeGI eid giM
                      in case M.lookup (infoType $ nodeInfo src, infoType $ nodeInfo tgt) pET of
                              Nothing -> giM
                              Just typeGI -> M.insert eid (typeGI {cPosition = cPosition egi}) giM
                newEGI = foldr changeEdgeGI (snd giM) edgesInContext
                newGI = (fst giM, newEGI)
                newGraph = foldr (\eid g -> updateEdgePayload eid g (\info -> infoSetType info typeInfo)) g unlockedSEids

            writeIORef currentState (stateSetGI newGI . stateSetGraph newGraph $ es)
            return True
      else return False


changeElementsOperation :: Gtk.DrawingArea -> IORef GraphState -> InfoOperation -> IO Bool
changeElementsOperation canvas currentState operationInfo = do
        es <- readIORef currentState

        let (sNids,sEids) = stateGetSelected es

        if (length sNids) + (length sEids) > 0 then
          do
            let g = stateGetGraph es
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
            return True
        else
          return False


-- auxiliar functions used in changedCursor callback --------------------------------------------------------------------------
-- auxiliar function to update nodes and edges elements according to the active typeGraph
updateElements :: IORef GraphState -> IORef (M.Map String (NodeGI, Int32)) -> IORef (M.Map String (M.Map (String,String) EdgeGI, Int32)) -> IO ()
updateElements currentState possibleNodeTypes possibleEdgeTypes= do
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

-- change the Inspector elements according to the selected graph
changeInspector :: Gtk.Box -> Gtk.Box -> Gtk.Box -> Gtk.Button -> Gtk.Button -> Gtk.MenuItem -> Gtk.MenuItem -> Int32 -> IO ()
changeInspector layoutBox typeSelectionBox operationBox mergeBtn splitBtn mrg spt gType =
  if gType == 0 then
    return ()
  else
    do
      -- if the current graph is the typeGraph then show the layout box else show the typeSelectionBox
      if gType == 1 then do
        #show layoutBox
        #hide typeSelectionBox
      else do
        #hide layoutBox
        #show typeSelectionBox

      -- if the current graph is a ruleGraph, then show the operation box
      if gType == 3 then
        #show operationBox
      else
        #hide operationBox

      -- if the currentGraph is a nac, then show the merge and split options
      if gType == 4 then do
        #show mergeBtn
        #show splitBtn
        mapM_ (\m -> Gtk.widgetSetSensitive m True) [mrg,spt]
      else do
        #hide mergeBtn
        #hide splitBtn
        mapM_ (\m -> Gtk.widgetSetSensitive m False) [mrg,spt]


---------------------------------------------------------------------------------------------------------------------------------
--  Auxiliar Functions  ---------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------


-- auxiliar function to check if the project was changed
-- it does the checking and if no, ask the user if them want to save.
-- returns True if there's no changes, if the user don't wanted to save or if he wanted and the save operation was successfull
-- returns False if the user wanted to save and the save operation failed or opted to cancel.
confirmOperation :: Gtk.Window
                 -> Gtk.TreeStore
                 -> IORef Bool
                 -> IORef GraphState
                 -> IORef (M.Map Int32 (Diagram, MergeMapping))
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


-- auxiliar function to prepare the treeStore to save
-- auxiliar function, add the current editor state in the graphStates list
storeCurrentES :: Gtk.Window -> IORef GraphState -> StoreIORefs -> IORef (M.Map Int32 (Diagram, MergeMapping)) -> IO ()
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
