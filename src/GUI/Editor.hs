{-|
  This module provides the functions used as callbacks for the Graph Grammar editor
-}

{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module GUI.Editor(
  startEditor
, basicCanvasButtonPressedCallback
, basicCanvasMotionCallBack
, basicCanvasButtonReleasedCallback
, prepToExport
, confirmOperation
)where

import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import qualified GI.Pango as P
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

-- verigraph modules
import Abstract.Category
import Abstract.Rewriting.DPO
import Data.Graphs hiding (null, empty)
import qualified Data.Graphs as G
import qualified Data.TypedGraph.Morphism as TGM

-- Verigraph-GUI modules
import           GUI.Data.DiaGraph hiding (empty)
import qualified GUI.Data.DiaGraph as DG
import           GUI.Data.EditorState
import           GUI.Data.GraphicalInfo
import           GUI.Data.Info hiding (empty)
import qualified GUI.Data.Info as I
import           GUI.Data.Nac
import           GUI.Dialogs
import           GUI.Helper.List
import           GUI.Helper.Geometry
import           GUI.Helper.GrammarMaker
import           GUI.Helper.GraphValidation
import           GUI.Helper.TreeStore
import           GUI.Render.Render

-- Editor modules
import GUI.Editor.Helper.Clipboard
import GUI.Editor.Helper.GraphicalInfo
import GUI.Editor.Helper.Nac
import GUI.Editor.Helper.SaveLoad
import GUI.Editor.Helper.TypeInfer
import GUI.Editor.Helper.UndoRedo
import GUI.Editor.Render.GraphDraw
import GUI.Editor.UI.UIBuilders
import GUI.Editor.UI.RuleViewer
import GUI.Editor.UI.UpdateInspector

---------------------------------------------------------------------------------------------------------------------------------
--  IORefs Used in Editor  ------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------

type BasicEditIORefs  = ( IORef EditorState, IORef (Double,Double), IORef (Maybe (Double,Double,Double,Double)), IORef Bool)
type StoreIORefs      = ( IORef (M.Map Int32 EditorState), IORef [Int32], IORef Int32, IORef Int32 )
type ChangesIORefs    = ( IORef Bool, IORef [Bool], IORef (M.Map Int32 DiaGraph))
type NacIORefs        = ( IORef (M.Map Int32 (DiaGraph, MergeMapping)), IORef (Maybe MergeMapping))

---------------------------------------------------------------------------------------------------------------------------------
--  Editor Construction  --------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------

{- | Create the editor widgets (the treeView at left, the canvas at center and the inpector panel at right) 
    and set the callbacks for them.
    Returns the gtkPaned parent to those widgets.
-}
startEditor :: Gtk.Window -> [Gtk.MenuItem] -> [Gtk.MenuItem] -> [Gtk.MenuItem]
               -> Gtk.TreeStore
               -> BasicEditIORefs -> StoreIORefs -> ChangesIORefs 
               -> IORef (Maybe String) -> IORef (Graph Info Info) -> NacIORefs
               -> IO Gtk.Paned
startEditor window fileItems editItems viewItems 
                store
                basicEditIORefs storeIORefs changesIORefs
                fileName activeTypeGraph nacIORefs = do

  -- create the ui of the editor
  (mainPane, treeFrame, canvas, nameEntry, entryLabel, layoutWidgets, typeSelectionWidgets) <- buildEditor

  let (layoutBox, fillColorBox, fillColorBtn, lineColorBox, lineColorBtn, nodeShapeFrame, radioShapes, edgeStyleFrame, radioStyles) = layoutWidgets
      (typeSelectionBox, autoLabelNCheckBtn, autoLabelECheckBtn, nodeTypeBox, nodeTypeCBox, edgeTypeBox, edgeTypeCBox, operationBox, operationCBox, mergeBtn, splitBtn) = typeSelectionWidgets
      [circleRadioBtn, rectRadioBtn, squareRadioBtn] = radioShapes
      [normalRadioBtn, slashedRadioBtn, pointedRadioBtn] = radioStyles
  let
    typeInspWidgets = (nameEntry, fillColorBtn, lineColorBtn, radioShapes, radioStyles)
    hostInspWidgets = (nameEntry, nodeTypeCBox, edgeTypeCBox)
    ruleInspWidgets = (nameEntry, nodeTypeCBox, edgeTypeCBox, operationCBox)
    nacInspWidgets  = (nameEntry, nodeTypeCBox, edgeTypeCBox, mergeBtn, splitBtn)

  -- create the treePanel and append it to the treeFrame
  (treeBox, treeview, changesRenderer, nameRenderer, activeRenderer, createRBtn, removeBtn, createNBtn) <- buildTreePanel
  Gtk.containerAdd treeFrame treeBox
  Gtk.treeViewSetModel treeview (Just store)

  changesCol <- Gtk.treeViewGetColumn treeview 0
  namesCol <- Gtk.treeViewGetColumn treeview 1
  activeCol <- Gtk.treeViewGetColumn treeview 2

  -- set the information renderered by each column of the treeView
  case changesCol of
    Nothing -> return ()
    Just col -> Gtk.treeViewColumnSetCellDataFunc col changesRenderer $ Just (\column renderer model iter -> do
      changed <- Gtk.treeModelGetValue model iter 1 >>= fromGValue:: IO Int32
      valid <- Gtk.treeModelGetValue model iter 5 >>= fromGValue :: IO Bool
      renderer' <- castTo Gtk.CellRendererText renderer
      case (renderer', changed, valid) of
        (Just r, 0, True)  -> set r [#text := ""  ]
        (Just r, 1, True)  -> set r [#text := "*" ]
        (Just r, 0, False) -> set r [#text := "!" ]
        (Just r, 1, False) -> set r [#text := "!*"]
        _ -> return ()

      )

  case namesCol of
    Nothing -> return ()
    Just col -> do
      #addAttribute col nameRenderer "text" 0
      path <- Gtk.treePathNewFromIndices [0,0]
      Gtk.treeViewExpandToPath treeview path
      Gtk.treeViewSetCursor treeview path (Nothing :: Maybe Gtk.TreeViewColumn) False

  case activeCol of
    Nothing -> return ()
    Just col -> Gtk.treeViewColumnSetCellDataFunc col activeRenderer $ Just (\column renderer model iter -> do
      gType <- Gtk.treeModelGetValue model iter 3 >>= \gv -> (fromGValue gv :: IO Int32)
      active <- Gtk.treeModelGetValue model iter 4 >>= \gv -> (fromGValue gv :: IO Bool)
      renderer' <- castTo Gtk.CellRendererToggle renderer
      case (renderer', gType) of
        (Just r, 3) -> set r [#visible := True, #radio := False, #active := active, #activatable:=True]
        (Just r, _) -> set r [#visible := False]
        _ -> return ()
      )
  
  -- "unpack" menuItems
  let [newm,opn,svn,sva,eggx,evgg] = fileItems
      [del,udo,rdo,cpy,pst,cut,sla,sln,sle,mrg,spt] = editItems
      [zin,zut,z50,zdf,z150,z200,vdf] = viewItems

  #showAll mainPane
  #hide typeSelectionBox
  #hide createNBtn
  #hide removeBtn

  ----------------------------------------------------------------------------------------------------------------------------
  -- IORefs  -----------------------------------------------------------------------------------------------------------------
  ----------------------------------------------------------------------------------------------------------------------------

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
  

  -- "unpack" IORefs
  let (st, oldPoint, squareSelection, movingGI) = basicEditIORefs
      (graphStates,currentPath,currentGraph,currentGraphType) = storeIORefs
      (changedProject, changedGraph, lastSavedState) = changesIORefs
      (nacInfoMapIORef, mergeMappingIORef) = nacIORefs

  

  ----------------------------------------------------------------------------------------------------------------------------
  -- Auxiliar Functions for Bindings  ----------------------------------------------------------------------------------------
  ----------------------------------------------------------------------------------------------------------------------------

  let updateByType = do
        gt <- readIORef currentGraphType
        case gt of
          1 -> updateTG st activeTypeGraph possibleNodeTypes possibleEdgeTypes possibleSelectableEdgeTypes graphStates nodeTypeCBox edgeTypeCBox store
          2 -> setCurrentValidFlag store st activeTypeGraph currentPath
          3 -> setCurrentValidFlag store st activeTypeGraph currentPath
          4 -> setCurrentValidFlag store st activeTypeGraph currentPath
          _ -> return ()

  
  
  ----------------------------------------------------------------------------------------------------------------------------
  -- Event Bindings for the Canvas -------------------------------------------------------------------------------------------
  ----------------------------------------------------------------------------------------------------------------------------
  
  -- drawing event
  on canvas #draw $ \context -> do
    es <- readIORef st
    sq <- readIORef squareSelection
    t <- readIORef currentGraphType
    case t of
      0 -> return ()
      1 -> renderWithContext context $ drawTypeGraph es sq
      2 -> do
        tg <- readIORef activeTypeGraph
        renderWithContext context $ drawHostGraph es sq tg
      3 -> do
        tg <- readIORef activeTypeGraph
        renderWithContext context $ drawRuleGraph es sq tg
      4 -> do
        tg <- readIORef activeTypeGraph
        mm <- readIORef mergeMappingIORef >>= return . fromMaybe (M.empty, M.empty)
        renderWithContext context $ drawNACGraph es sq tg mm
      _ -> return ()
    return False

  -- mouse button pressed on canvas
  -- set callback to select elements on canvas
  on canvas #buttonPressEvent $ basicCanvasButtonPressedCallback st oldPoint squareSelection canvas
  -- edition only callback
  on canvas #buttonPressEvent $ \eventButton -> do
    b <- get eventButton #button
    x <- get eventButton #x
    y <- get eventButton #y
    ms <- get eventButton #state
    click <- get eventButton #type
    es <- readIORef st
    gType <- readIORef currentGraphType
    tg <- readIORef activeTypeGraph
    
    if gType > 1
      then changeEdgeTypeCBoxByContext possibleEdgeTypes possibleSelectableEdgeTypes edgeTypeCBox es tg []
      else return ()

    let z = editorGetZoom es
        (px,py) = editorGetPan es
        (x',y') = (x/z - px, y/z - py)

    case (b, click == Gdk.EventType2buttonPress) of
      --double click with left button : rename selection
      (1, True) -> do
        let (n,e) = editorGetSelected es
        if null n && null e
          then return ()
          else Gtk.widgetGrabFocus nameEntry
      -- right button click: create nodes and insert edges
      (3, False) -> do
        let g = editorGetGraph es
            gi = editorGetGI es
            dstNode = selectNodeInPosition gi (x',y')
        context <- Gtk.widgetGetPangoContext canvas
        -- if current graph is a nac, then add mergeM to the undoStack
        case gType of
          4 -> do mergeM <- readIORef mergeMappingIORef
                  stackUndo undoStack redoStack currentGraph es mergeM
          _ -> stackUndo undoStack redoStack currentGraph es Nothing
        cShape <- readIORef currentShape
        cColor <- readIORef currentC
        cLColor <- readIORef currentLC
        case (dstNode) of
          -- no selected node: create node
          Nothing -> case gType of
              0 -> return ()
              1 -> do
                createNode' st I.empty True (x',y') cShape cColor cLColor context
                setChangeFlags window store changedProject changedGraph currentPath currentGraph True
                updateTG st activeTypeGraph possibleNodeTypes possibleEdgeTypes possibleSelectableEdgeTypes graphStates nodeTypeCBox edgeTypeCBox store
              _ -> do
                auto <- Gtk.toggleButtonGetActive autoLabelNCheckBtn
                mntype <- readIORef currentNodeType
                (t, shape, c, lc) <- case mntype of
                  Nothing -> return ("", cShape, cColor, cLColor)
                  Just t -> do
                    possibleNT <- readIORef possibleNodeTypes
                    let possibleNT' = M.map (\(gi,i) -> gi) possibleNT
                        mngi = M.lookup t possibleNT'
                    case mngi of
                      Nothing -> return ("", cShape, cColor, cLColor)
                      Just gi -> return (t, shape gi, fillColor gi, lineColor gi)
                createNode' st (infoSetType I.empty t) auto (x',y') shape c lc context
                setChangeFlags window store changedProject changedGraph currentPath currentGraph True
                setCurrentValidFlag store st activeTypeGraph currentPath

                case gType of
                  -- if the current graph is a nac, then add the node in nacg
                  4 -> do
                    -- get the node and it's gi
                    es <- readIORef st
                    let g = editorGetGraph es
                        gi = editorGetGI es
                        addedNodeId = maximum (nodeIds g)
                        addedNode = fromJust $ lookupNode addedNodeId g
                        addedNodeGI = getNodeGI (fromEnum addedNodeId) (fst gi)
                    -- get the nacg and add the node
                    nacInfoMap <- readIORef nacInfoMapIORef
                    index <- readIORef currentGraph
                    let ((nacg,nacgi), mapping) = fromMaybe (DG.empty, (M.empty,M.empty)) $ M.lookup index nacInfoMap
                        nacg' = insertNodeWithPayload addedNodeId (nodeInfo addedNode) nacg
                        nacNgi' = M.insert (fromEnum addedNodeId) addedNodeGI (fst nacgi)
                    modifyIORef nacInfoMapIORef $ M.insert index ((nacg',(nacNgi', snd nacgi)), mapping)
                  _ -> return ()


          -- one node selected: create edges targeting this node
          Just nid -> case gType of
            0 -> return ()
            1 -> do
              estyle <- readIORef currentStyle
              color <- readIORef currentLC
              modifyIORef st (\es -> createEdges es nid I.empty True estyle color)
              setChangeFlags window store changedProject changedGraph currentPath currentGraph True
              updateTG st activeTypeGraph possibleNodeTypes possibleEdgeTypes possibleSelectableEdgeTypes graphStates nodeTypeCBox edgeTypeCBox store
            _ -> do
              metype <- readIORef currentEdgeType
              cEstyle <- readIORef currentStyle
              cColor <- readIORef currentLC
              auto <- Gtk.toggleButtonGetActive autoLabelECheckBtn
              es <- readIORef st
              tg <- readIORef activeTypeGraph
              pet <- readIORef possibleEdgeTypes 
              pet' <- return $ M.map fst pet

              -- create edges infering their types
              let sNids = fst $ editorGetSelected es
                  g = editorGetGraph es
                  srcNodes = map (\nid -> fromJust $ G.lookupNode nid g) sNids
                  tgtNode = fromJust $ G.lookupNode nid g
                  tgtType = infoType $ nodeInfo tgtNode
                  edgesTs = map (\src -> (src, infereEdgeType tg src tgtNode metype)) srcNodes
                  -- auxiliar function that checks if the type mt, exist in possibleEdgeTypes.
                    -- returns the correspondent triple (type, style, color)
                  checkType mt srcType = case mt of
                                        Nothing -> ("", cEstyle, cColor)
                                        Just t -> let megi = M.lookup t pet'
                                                  in case megi of
                                                        Nothing -> ("", cEstyle, cColor)
                                                        Just sm -> case M.lookup (srcType,tgtType) sm of
                                                                      Nothing -> ("", cEstyle, cColor)
                                                                      Just gi -> (t, style gi, color gi)
                  (es', createdEdges) = 
                            foldr (\(src, mt) (es,eids) -> 
                                      let srcId = nodeId src
                                          (t,estyle,color) = checkType mt (infoType $ nodeInfo src)
                                          es' = createEdge es srcId nid (infoSetType I.empty t) auto estyle color
                                          eids' = (snd $ editorGetSelected es') ++ eids
                                      in (es',eids'))
                              (es,[]) edgesTs
              
              writeIORef st $ editorSetSelected ([],createdEdges) es' 

              if gType > 1
                then changeEdgeTypeCBoxByContext possibleEdgeTypes possibleSelectableEdgeTypes edgeTypeCBox es' tg createdEdges
                else return ()

              setChangeFlags window store changedProject changedGraph currentPath currentGraph True
              setCurrentValidFlag store st activeTypeGraph currentPath

              -- if the current graph is a nac, then add the created edges in the nacg
              if gType /= 4
                then return ()
                else do
                  -- get the difference between the new graph and the old one
                  newEs <- readIORef st
                  let g = editorGetGraph newEs
                      gi = editorGetGI newEs
                      oldg = editorGetGraph es
                      oldgi = editorGetGI es
                      (g',gi') = diagrSubtract (g,gi) (oldg,oldgi)
                  -- get nac information
                  nacInfoMap <- readIORef nacInfoMapIORef
                  index <- readIORef currentGraph
                  let ((nacg,nacgi), (nM, eM)) = fromMaybe (DG.empty, (M.empty,M.empty)) $ M.lookup index nacInfoMap
                  -- insert nodes in nacg if they aren't already present
                  let nacgWithNodes = foldr (\n g -> if nodeId n `elem` (nodeIds g)
                                                  then g
                                                  else insertNodeWithPayload (nodeId n) (nodeInfo n) g)
                                        nacg (nodes g')
                  -- insert created edges in nacg
                  let nacg' = foldr (\e g -> insertEdgeWithPayload (edgeId e) (sourceId e) (targetId e) (edgeInfo e) g)
                                nacgWithNodes (edges g')
                  let nacEgi' = foldr (\(eid, egi) giM -> M.insert eid egi giM) (snd nacgi) (M.toList $ snd gi')
                  -- modify mapping if g' have any node from lhs
                  let nodesFromLHS = filter (\n -> infoLocked $ nodeInfo n) (nodes g')
                      nM' = foldr (\n m -> if n `elem` (M.elems m) then m else M.insert n n m) nM (map nodeId nodesFromLHS)
                  modifyIORef nacInfoMapIORef $ M.insert index ((nacg', (fst nacgi, nacEgi')), (nM',eM))

        Gtk.widgetQueueDraw canvas
      _           -> return ()

    updateInspector currentGraphType st currentC currentLC typeInspWidgets (fillColorBox, nodeShapeFrame, edgeStyleFrame)
                    possibleNodeTypes possibleSelectableEdgeTypes currentNodeType currentEdgeType mergeMappingIORef
                    hostInspWidgets ruleInspWidgets nacInspWidgets (nodeTypeBox, edgeTypeBox)

    return True

  -- mouse motion on canvas
  -- set callback to move elements or expand the square selection area
  on canvas #motionNotifyEvent $ basicCanvasMotionCallBack st oldPoint squareSelection canvas
  -- add colateral actions such as modiffing the undo stack and setting the 'changeFlags'
  on canvas #motionNotifyEvent $ \eventMotion -> do
    ms <- get eventMotion #state
    es <- readIORef st
    gtype <- readIORef currentGraphType
    let leftButton = Gdk.ModifierTypeButton1Mask `elem` ms
        middleButton = Gdk.ModifierTypeButton2Mask `elem` ms || Gdk.ModifierTypeButton3Mask `elem` ms && Gdk.ModifierTypeControlMask `elem` ms
        (sNodes, sEdges) = editorGetSelected es
    case (leftButton, middleButton, sNodes, sEdges) of
      (True, False, n, e) -> do
        mv <- readIORef movingGI
        if not mv
          then do
            writeIORef movingGI True
            setChangeFlags window store changedProject changedGraph currentPath currentGraph True
            mergeM <- case gtype of
                        4 -> readIORef mergeMappingIORef
                        _ -> return Nothing
            stackUndo undoStack redoStack currentGraph es mergeM
          else return ()
      _ -> return ()
    return True

  -- mouse button release on canvas
  -- set callback to select elements that are inside squareSelection
  on canvas #buttonReleaseEvent $ basicCanvasButtonReleasedCallback st squareSelection canvas
  -- colateral effects to selected elements
  on canvas #buttonReleaseEvent $ \eventButton -> do
    b <- get eventButton #button
    gType <- readIORef currentGraphType
    tg <- readIORef activeTypeGraph
    es <- readIORef st
    case b of
      1 -> do
        writeIORef movingGI False
        let (sNodes, sEdges) = editorGetSelected es
        case (length sNodes > 0 || length sEdges > 0) of 
          True -> do
            if gType > 1
              then changeEdgeTypeCBoxByContext possibleEdgeTypes possibleSelectableEdgeTypes edgeTypeCBox es tg sEdges
              else return ()
            updateInspector currentGraphType st currentC currentLC typeInspWidgets (fillColorBox, nodeShapeFrame, edgeStyleFrame)
                            possibleNodeTypes possibleSelectableEdgeTypes currentNodeType currentEdgeType mergeMappingIORef
                            hostInspWidgets ruleInspWidgets nacInspWidgets (nodeTypeBox, edgeTypeBox)
          False -> return ()
      _ -> return ()
    return True

  -- mouse wheel scroll on canvas
  on canvas #scrollEvent $ \eventScroll -> do
    d <- get eventScroll #direction
    ms <- get eventScroll #state
    case (Gdk.ModifierTypeControlMask `elem` ms, d) of
      -- when control is pressed,
      -- if the direction is up, then zoom in
      (True, Gdk.ScrollDirectionUp)  -> liftIO $ Gtk.menuItemActivate zin
      -- if the direction is down, then zoom out
      (True, Gdk.ScrollDirectionDown) -> liftIO $ Gtk.menuItemActivate zut
      _ -> return ()
    return True

  -- keyboard
  on canvas #keyPressEvent $ \eventKey -> do
    k <- get eventKey #keyval >>= return . chr . fromIntegral
    ms <- get eventKey #state
    case (Gdk.ModifierTypeControlMask `elem` ms, Gdk.ModifierTypeShiftMask `elem` ms, toLower k) of
      -- F2 - rename selection
      (False,False,'\65471') -> Gtk.widgetGrabFocus nameEntry
      -- 'delete' whne the focus is on canvas - delete elements
      (False,False,'\65535') -> Gtk.menuItemActivate del
      _ -> return ()
    return True

  ----------------------------------------------------------------------------------------------------------------------------
  -- Event Bindings for the Inspector Panel  ---------------------------------------------------------------------------------
  ----------------------------------------------------------------------------------------------------------------------------
  
  on nameEntry #keyPressEvent $ \eventKey -> do
    k <- get eventKey #keyval >>= return . chr . fromIntegral
    --if it's Return or Enter (Numpad), then change the name of the selected elements
    case k of
       '\65293' -> Gtk.widgetGrabFocus canvas
       '\65421' -> Gtk.widgetGrabFocus canvas
       _       -> return ()
    return False

  -- when the entry lose focus
  on nameEntry #focusOutEvent $ \event -> do
    -- rename selected element(s)
    es <- readIORef st
    stackUndo undoStack redoStack currentGraph es Nothing
    setChangeFlags window store changedProject changedGraph currentPath currentGraph True
    name <- Gtk.entryGetText nameEntry >>= return . T.unpack
    context <- Gtk.widgetGetPangoContext canvas
    es <- readIORef st
    es' <- renameSelected es name context

    -- infere the types of edge(s) connected to the renamed node(s)
    typesE <- readIORef possibleEdgeTypes
    tg <- readIORef activeTypeGraph
    let 
      typesE' = M.map fst typesE
      es'' = infereEdgesTypesAfterNodeChange es' tg typesE'

    writeIORef st es''
    Gtk.widgetQueueDraw canvas
    updateInspector currentGraphType st currentC currentLC typeInspWidgets (fillColorBox, nodeShapeFrame, edgeStyleFrame)
                    possibleNodeTypes possibleSelectableEdgeTypes currentNodeType currentEdgeType mergeMappingIORef
                    hostInspWidgets ruleInspWidgets nacInspWidgets (nodeTypeBox, edgeTypeBox)
    updateByType

    gt <- readIORef currentGraphType
    if gt == 4
      then do
        index <- readIORef currentGraph
        nacInfoMap <- readIORef nacInfoMapIORef
        es <- readIORef st
        let (_,mapping) = fromMaybe (DG.empty,(M.empty,M.empty)) $ M.lookup index nacInfoMap
            g = editorGetGraph es
            nacg = extractNacGraph g mapping
            nacgi = extractNacGI g (editorGetGI es) mapping
        modifyIORef nacInfoMapIORef $ M.insert index ((nacg,nacgi),mapping)
      else return ()
    return False

  -- select a fill color
  -- change the selection fill color and
  -- set the current fill color as the selected color
  on fillColorBtn #colorSet $ do
    gtkcolor <- Gtk.colorChooserGetRgba fillColorBtn
    es <- readIORef st
    r <- get gtkcolor #red
    g <- get gtkcolor #green
    b <- get gtkcolor #blue
    let color = (r,g,b)
        (nds,edgs) = editorGetSelected es
    writeIORef currentC color
    if null nds
      then return ()
      else do
        let (ngiM, egiM) = editorGetGI es
            newngiM = M.mapWithKey (\k ngi -> if NodeId k `elem` nds then nodeGiSetColor color ngi else ngi) ngiM
        stackUndo undoStack redoStack currentGraph es Nothing
        setChangeFlags window store changedProject changedGraph currentPath currentGraph True
        modifyIORef st (\es -> editorSetGI (newngiM, egiM) es)
        Gtk.widgetQueueDraw canvas
        updateTG st activeTypeGraph possibleNodeTypes possibleEdgeTypes possibleSelectableEdgeTypes graphStates nodeTypeCBox edgeTypeCBox store

  -- select a line color
  -- same as above, except it's for the line color
  on lineColorBtn #colorSet $ do
    gtkcolor <- Gtk.colorChooserGetRgba lineColorBtn
    es <- readIORef st
    r <- get gtkcolor #red
    g <- get gtkcolor #green
    b <- get gtkcolor #blue
    let color = (r,g,b)
        (nds,edgs) = editorGetSelected es
    writeIORef currentLC color
    if null nds && null edgs
      then return ()
      else do
        let (ngiM, egiM) = editorGetGI es
            newngiM = M.mapWithKey (\k ngi -> if NodeId k `elem` nds then nodeGiSetLineColor color ngi else ngi) ngiM
            newegiM = M.mapWithKey (\k egi -> if EdgeId k `elem` edgs then edgeGiSetColor color egi else egi) egiM
        stackUndo undoStack redoStack currentGraph es Nothing
        setChangeFlags window store changedProject changedGraph currentPath currentGraph True
        modifyIORef st (\es -> editorSetGI (newngiM, newegiM) es)
        Gtk.widgetQueueDraw canvas
        updateTG st activeTypeGraph possibleNodeTypes possibleEdgeTypes possibleSelectableEdgeTypes graphStates nodeTypeCBox edgeTypeCBox store

  -- toogle the radio buttons for node shapes
  -- change the shape of the selected nodes and set the current shape for new nodes
  circleRadioBtn `on` #toggled $ do
    writeIORef currentShape NCircle
    es <- readIORef st
    active <- get circleRadioBtn #active
    let nds = fst $ editorGetSelected es
        giM = fst $ editorGetGI es
    if not active || M.null (M.filterWithKey (\k gi -> NodeId k `elem` nds && shape gi /= NCircle) giM)
      then return ()
      else do
        stackUndo undoStack redoStack currentGraph es Nothing
        setChangeFlags window store changedProject changedGraph currentPath currentGraph True
        modifyIORef st (\es -> changeNodeShape es NCircle)
        Gtk.widgetQueueDraw canvas
        updateTG st activeTypeGraph possibleNodeTypes possibleEdgeTypes possibleSelectableEdgeTypes graphStates nodeTypeCBox edgeTypeCBox store

  rectRadioBtn `on` #toggled $ do
    writeIORef currentShape NRect
    es <- readIORef st
    active <- get rectRadioBtn #active
    let nds = fst $ editorGetSelected es
        giM = fst $ editorGetGI es
    if not active || M.null (M.filterWithKey (\k gi -> NodeId k `elem` nds && shape gi /= NRect) giM)
      then return ()
      else do
        stackUndo undoStack redoStack currentGraph es Nothing
        setChangeFlags window store changedProject changedGraph currentPath currentGraph True
        modifyIORef st (\es -> changeNodeShape es NRect)
        Gtk.widgetQueueDraw canvas
        updateTG st activeTypeGraph possibleNodeTypes possibleEdgeTypes possibleSelectableEdgeTypes graphStates nodeTypeCBox edgeTypeCBox store

  squareRadioBtn `on` #toggled $ do
    writeIORef currentShape NSquare
    es <- readIORef st
    active <- get squareRadioBtn #active
    let nds = fst $ editorGetSelected es
        giM = fst $ editorGetGI es
    if not active || M.null (M.filterWithKey (\k gi -> NodeId k `elem` nds && shape gi /= NSquare) giM)
      then return ()
      else do
        stackUndo undoStack redoStack currentGraph es Nothing
        setChangeFlags window store changedProject changedGraph currentPath currentGraph True
        modifyIORef st (\es -> changeNodeShape es NSquare)
        Gtk.widgetQueueDraw canvas
        updateTG st activeTypeGraph possibleNodeTypes possibleEdgeTypes possibleSelectableEdgeTypes graphStates nodeTypeCBox edgeTypeCBox store

  -- toogle the radio buttons for edge styles
  -- change the style of the selected edges and set the current style for new edges
  normalRadioBtn `on` #toggled $ do
    writeIORef currentStyle ENormal
    es <- readIORef st
    active <- get normalRadioBtn #active
    let edgs = snd $ editorGetSelected es
        giM = snd $ editorGetGI es
    if not active || M.null (M.filterWithKey (\k gi -> EdgeId k `elem` edgs && style gi /= ENormal) giM)
      then return ()
      else do
        stackUndo undoStack redoStack currentGraph es Nothing
        setChangeFlags window store changedProject changedGraph currentPath currentGraph True
        modifyIORef st (\es -> changeEdgeStyle es ENormal)
        Gtk.widgetQueueDraw canvas
        updateTG st activeTypeGraph possibleNodeTypes possibleEdgeTypes possibleSelectableEdgeTypes graphStates nodeTypeCBox edgeTypeCBox store

  pointedRadioBtn `on` #toggled $ do
    writeIORef currentStyle EPointed
    es <- readIORef st
    active <- get pointedRadioBtn #active
    let edgs = snd $ editorGetSelected es
        giM = snd $ editorGetGI es
    if not active || M.null (M.filterWithKey (\k gi -> EdgeId k `elem` edgs && style gi /= EPointed) giM)
      then return ()
      else do
        stackUndo undoStack redoStack currentGraph es Nothing
        setChangeFlags window store changedProject changedGraph currentPath currentGraph True
        modifyIORef st (\es -> changeEdgeStyle es EPointed)
        Gtk.widgetQueueDraw canvas
        updateTG st activeTypeGraph possibleNodeTypes possibleEdgeTypes possibleSelectableEdgeTypes graphStates nodeTypeCBox edgeTypeCBox store

  slashedRadioBtn `on` #toggled $ do
    writeIORef currentStyle ESlashed
    es <- readIORef st
    active <- get slashedRadioBtn #active
    let edgs = snd $ editorGetSelected es
        giM = snd $ editorGetGI es
    if not active || M.null (M.filterWithKey (\k gi -> EdgeId k `elem` edgs && style gi /= ESlashed) giM)
      then return ()
      else do
        stackUndo undoStack redoStack currentGraph es Nothing
        setChangeFlags window store changedProject changedGraph currentPath currentGraph True
        modifyIORef st (\es -> changeEdgeStyle es ESlashed)
        Gtk.widgetQueueDraw canvas
        updateTG st activeTypeGraph possibleNodeTypes possibleEdgeTypes possibleSelectableEdgeTypes graphStates nodeTypeCBox edgeTypeCBox store


  -- choose a type in the type nodeTypeCBox for nodes
  on nodeTypeCBox #changed $ do
    gt <- readIORef currentGraphType
    if gt < 2
      then return () -- if the current graph type is 
      else do
        index <- Gtk.comboBoxGetActive nodeTypeCBox
        if index == (-1)
          then return ()
          else do
            typeInfo <- Gtk.comboBoxTextGetActiveText nodeTypeCBox >>= return . T.unpack
            typeNGI <- readIORef possibleNodeTypes >>= return . fst . fromJust . M.lookup typeInfo
            es <- readIORef st
            
            let (sNids,sEids) = editorGetSelected es
                g = editorGetGraph es
                -- foreach selected node, change their types
                acceptableSNids = filter (\nid -> case lookupNode nid g of
                                                    Nothing -> False
                                                    Just n -> not $ infoLocked (nodeInfo n)) sNids
                giM = editorGetGI es
                g' = foldr (\nid g -> updateNodePayload nid g (\info -> infoSetType info typeInfo)) g acceptableSNids
                newNGI = foldr  (\nid giM -> let ngi = getNodeGI (fromEnum nid) giM
                                            --in M.insert (fromEnum nid) (nodeGiSetPosition (position ngi) . nodeGiSetDims (dims ngi) $ typeNGI) gi) (fst giM) acceptableSNids
                                            in M.insert (fromEnum nid) (typeNGI {position = position ngi, dims = dims ngi}) giM) 
                                (fst giM)
                                acceptableSNids
                es' = editorSetGraph g' . editorSetGI (newNGI, snd giM) $ es

                -- foreach changed node, change the type of the edges connected to it
            typesE <- readIORef possibleEdgeTypes >>= return . M.map fst
            tg <- readIORef activeTypeGraph
            let es'' = infereEdgesTypesAfterNodeChange es' tg typesE
              
            case gt of
              4 -> do
                nacInfoMap <- readIORef nacInfoMapIORef
                index <- readIORef currentGraph
                let ((ng,ngiM), nacM) = fromJust $ M.lookup index nacInfoMap
                    newNG = extractNacGraph (editorGetGraph es'') nacM
                    newNGIM = extractNacGI (editorGetGraph es'') (editorGetGI es'') nacM
                modifyIORef nacInfoMapIORef $ M.insert index ((newNG, newNGIM), nacM)
              _ -> return ()
            writeIORef st es''
            writeIORef currentNodeType $ Just typeInfo
            Gtk.widgetQueueDraw canvas
            setCurrentValidFlag store st activeTypeGraph currentPath

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
            es <- readIORef st
            typeInfo <- Gtk.comboBoxTextGetActiveText edgeTypeCBox >>= return . T.unpack
            pET <- readIORef possibleEdgeTypes >>= return . fst . fromJust . M.lookup typeInfo
            let (sNids,sEids) = editorGetSelected es
                g = editorGetGraph es
                giM = editorGetGI es
                acceptableSEids = filter (\eid -> case lookupEdge eid g of
                                                      Nothing -> False
                                                      Just e -> not $ infoLocked (edgeInfo e)) sEids
                edgesInContext = map (\eid -> fromJust $ lookupEdgeInContext eid g) acceptableSEids

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
                nacInfoMap <- readIORef nacInfoMapIORef
                index <- readIORef currentGraph
                let ((ng,ngiM), nacM) = fromJust $ M.lookup index nacInfoMap
                    newNG = extractNacGraph newGraph nacM
                    newNacGI = extractNacGI newGraph newGI nacM
                    newNacdg = (newNG, newNacGI)
                modifyIORef nacInfoMapIORef $ M.insert index (newNacdg, nacM)
              _ -> return ()
            writeIORef st (editorSetGI newGI . editorSetGraph newGraph $ es)
            writeIORef currentEdgeType $ Just typeInfo
            Gtk.widgetQueueDraw canvas
            setCurrentValidFlag store st activeTypeGraph currentPath

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
            es <- readIORef st
            operationInfo <- return $ case index of
              0 -> Preserve
              1 -> Create
              2 -> Delete
            let (sNids,sEids) = editorGetSelected es
                g = editorGetGraph es
                gi = editorGetGI es
                newGraph  = foldl (\g nid -> updateNodePayload nid g (\info -> infoSetOperation info operationInfo)) g sNids
                newGraph' = foldl (\g eid -> updateEdgePayload eid g (\info -> infoSetOperation info operationInfo)) newGraph sEids
            context <- Gtk.widgetGetPangoContext canvas
            font <- case operationInfo == Preserve of
              True -> return Nothing
              False -> return $ Just "Sans Bold 10"
            ndims <- forM sNids $ \nid -> do
              dim <- getStringDims (infoVisible . nodeInfo . fromJust . G.lookupNode nid $ newGraph') context font
              return (nid, dim)
            let newNgiM = foldl (\giM (nid, dim) -> let gi = nodeGiSetDims dim $ getNodeGI (fromEnum nid) giM
                                                    in M.insert (fromEnum nid) gi giM) (fst gi) ndims
            writeIORef st (editorSetGI (newNgiM, snd gi) . editorSetGraph newGraph' $ es)
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
            storeCurrentES window st storeIORefs nacInfoMapIORef

            -- build the graph for the nac
            writeIORef currentGraphType gType
            states <- readIORef graphStates
            let maybeState = M.lookup index states
            case maybeState of
              Just es -> do
                tg <- readIORef activeTypeGraph
                -- load lhs diagraph
                (lhsg,lhsgi) <- getParentDiaGraph store path graphStates
                let ruleLGNodesLock = foldr (\n g -> G.updateNodePayload n g (\info -> infoSetOperation (infoSetLocked info True) Preserve)) lhsg (nodeIds lhsg)
                    ruleLG = foldr (\e g -> G.updateEdgePayload e g (\info -> infoSetLocked info True)) ruleLGNodesLock (edgeIds lhsg)
                    lhsIsValid = isGraphValid ruleLG tg

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
                    lhsNgi' <- updateNodesGiDims (fst lhsgi) ruleLG context
                    let lhsgi' = (lhsNgi', snd lhsgi)
                    nacInfoMap <- readIORef nacInfoMapIORef
                    (nG,gi) <- case M.lookup index $ nacInfoMap of
                      Nothing -> do -- if the nac' diagraph is not found, then the nac must contain the lhs
                        return (ruleLG,lhsgi')
                      Just (nacdg,(nM,eM)) -> do
                        (nacdg',(nM',eM')) <- applyLhsChangesToNac lhsg (nacdg,(nM,eM)) (Just context)
                        writeIORef mergeMappingIORef $ Just (nM',eM')
                        modifyIORef nacInfoMapIORef $ M.insert index (nacdg', (nM',eM'))
                        case (G.null $ fst nacdg') of
                          True -> return (ruleLG, lhsgi') -- if there's no nac' diagraph, then the nac is just the lhs
                          False -> do
                            -- if there's a nac' diagraph, check if the graph is correct
                            let nacValid = isGraphValid (fst nacdg') tg
                            case nacValid of
                              True -> 
                                -- join nac
                                joinNAC (nacdg',(nM',eM')) (ruleLG, lhsgi') tg 
                              False -> do 
                                -- remove all the elements with type error
                                let validG = correctTypeGraph (fst nacdg') tg
                                    validNids = foldr (\n ns -> if nodeInfo n then (nodeId n):ns else ns) [] (nodes validG)
                                    validEids = foldr (\e es -> if edgeInfo e then (edgeId e):es else es) [] (edges validG)
                                    newNodes = filter (\n -> nodeId n `elem` validNids) (nodes $ fst nacdg')
                                    newEdges = filter (\e -> edgeId e `elem` validEids) (edges $ fst nacdg')
                                    newNG = fromNodesAndEdges newNodes newEdges
                                    newNNGI = M.filterWithKey (\k a -> NodeId k `elem` validNids) (fst . snd $ nacdg')
                                    newNEGI = M.filterWithKey (\k a -> EdgeId k `elem` validEids) (snd . snd $ nacdg')
                                    newNacdg = (newNG,(newNNGI, newNEGI))
                                -- join nac
                                joinNAC (newNacdg, (nM',eM')) (ruleLG, lhsgi') tg
                    writeIORef st $ editorSetGI gi . editorSetGraph nG $ es
                    writeIORef currentGraph index
              Nothing -> return ()

          -- case the selection is another type of graph, get the graph from the map
          (False, _) -> do
            -- update the current path
            writeIORef currentPath path
            -- update the current graph in the tree
            storeCurrentES window st storeIORefs nacInfoMapIORef
            -- load the selected graph from the tree
            writeIORef currentGraphType gType
            states <- readIORef graphStates
            let maybeState = M.lookup index states
            case maybeState of
              Just es -> do
                writeIORef st es
                writeIORef currentGraph index
                writeIORef mergeMappingIORef Nothing
              Nothing -> return ()

        -- auxiliar function to update nodes and edges elements according to the active typeGraph
        let updateElements = do
              pnt <- readIORef possibleNodeTypes
              pet <- readIORef possibleEdgeTypes
              es <- readIORef st
              let g = editorGetGraph es
                  (ngi, egi) = editorGetGI es
                  fn node = (nid, infoType (nodeInfo node), getNodeGI nid ngi)
                            where nid = fromEnum (nodeId node)
                  gn (i,t,gi) = case M.lookup t pnt of
                                  Nothing -> (i,gi)
                                  Just (gi',_) -> (i,nodeGiSetColor (fillColor gi') . nodeGiSetShape (shape gi') $ gi)
                  fe ((src,_), edge, (tgt,_)) = (eid, eType, srcType, tgtType, getEdgeGI eid egi)
                            where eid = fromEnum (edgeId edge)
                                  eType = infoType (edgeInfo edge)
                                  srcType = infoType $ nodeInfo src
                                  tgtType = infoType $ nodeInfo tgt
                  ge (i,et,st,tt,gi) = case M.lookup et pet of
                                  Nothing -> (i,gi)
                                  Just (sm,_) -> case M.lookup (st,tt) sm of 
                                    Nothing -> (i,gi)
                                    Just gi' -> (i, gi' {cPosition = cPosition gi})
                  newNodeGI = M.fromList . map gn . map fn $ nodes g
                  newEdgeGI = M.fromList . map ge . map fe $ edgesInContext g
              writeIORef st (editorSetGI (newNodeGI, newEdgeGI) es)

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
          2 -> do
            #hide layoutBox
            #show typeSelectionBox
            #hide operationBox
            #hide mergeBtn
            #hide splitBtn
            resetCurrentGI
            updateElements
          3 -> do
            #hide layoutBox
            #show typeSelectionBox
            #show operationBox
            #hide mergeBtn
            #hide splitBtn
            resetCurrentGI
            updateElements
          4 -> do
            #hide layoutBox
            #show typeSelectionBox
            #hide operationBox
            #show mergeBtn
            #show splitBtn
            resetCurrentGI
            updateElements
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
        storeSetGraphStore store iter ("Rule" ++ (show n), 0, newKey, 3, True, True)
        modifyIORef graphStates (M.insert newKey emptyES)
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
        gtype <- Gtk.treeModelGetValue store iter 3 >>= fromGValue :: IO Int32
        case gtype of
          3 -> do
            index <- Gtk.treeModelGetValue store iter 2 >>= fromGValue
            Gtk.treeStoreRemove store iter
            modifyIORef graphStates $ M.delete index
          4 -> do
            index <- Gtk.treeModelGetValue store iter 2 >>= fromGValue
            Gtk.treeStoreRemove store iter
            modifyIORef graphStates $ M.delete index
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
            storeCurrentES window st storeIORefs nacInfoMapIORef
            states <- readIORef graphStates
            let es = fromMaybe emptyES $ M.lookup index states
                (lhs,_,_) = graphToRuleGraphs (editorGetGraph es)
                gi = editorGetGI es
                ngi' = M.filterWithKey (\k a -> (NodeId k) `elem` (nodeIds lhs)) (fst gi)
                egi' = M.filterWithKey (\k a -> (EdgeId k) `elem` (edgeIds lhs)) (snd gi)
                nodeMap = M.empty 
                edgeMap = M.empty
                newKey = if M.size states > 0 then maximum (M.keys states) + 1 else 0
            n <- Gtk.treeModelIterNChildren store (Just iterR)
            iterN <- Gtk.treeStoreAppend store (Just iterR)
            storeSetGraphStore store iterN ("NAC" ++ (show n), 0, newKey, 4, True, True)
            modifyIORef nacInfoMapIORef (M.insert newKey (DG.empty,(nodeMap,edgeMap)))
            modifyIORef graphStates (M.insert newKey (editorSetGraph lhs . editorSetGI (ngi',egi') $ emptyES))
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
          _ -> return ()
      else return ()

  ----------------------------------------------------------------------------------------------------------------------------
  -- Event Bindings for the MenuItems  ---------------------------------------------------------------------------------------
  ----------------------------------------------------------------------------------------------------------------------------

  -- File Menu Items ---------------------------------------------------------------------------------------------------------
  
  -- new project
  on newm #activate $ do
    continue <- confirmOperation window store changedProject st nacInfoMapIORef fileName storeIORefs
    if continue
      then do
        Gtk.treeStoreClear store
        initStore store
        initTreeView treeview
        writeIORef st emptyES
        writeIORef undoStack $ M.fromList [(a, []) | a <- [0..2]]
        writeIORef redoStack $ M.fromList [(a, []) | a <- [0..2]]
        writeIORef fileName Nothing
        writeIORef currentPath [0]
        writeIORef currentGraphType 1
        writeIORef currentGraph 0
        writeIORef graphStates $ M.fromList [(a, emptyES) | a <- [0..2]]
        writeIORef lastSavedState M.empty
        writeIORef changedProject False
        writeIORef changedGraph [False]
        writeIORef nacInfoMapIORef M.empty
        writeIORef mergeMappingIORef Nothing
        set window [#title := "Verigraph-GUI"]
        Gtk.widgetQueueDraw canvas
      else return ()

   --open project
  
  -- open project
  on opn #activate $ do
    continue <- confirmOperation window store changedProject st nacInfoMapIORef fileName storeIORefs
    if continue
      then do
        mg <- loadFile window loadProject
        case mg of
          Nothing -> return ()
          Just (forest,fn) -> do
                Gtk.treeStoreClear store
                let toGSandStates n i = case n of
                              Topic name -> ((name,0,0,0,False), (0,(i,emptyES)))
                              TypeGraph name es -> ((name,0,i,1,True), (1, (i,es)))
                              HostGraph name es -> ((name,0,i,2,True), (2, (i,es)))
                              RuleGraph name es a -> ((name,0,i,3,a), (3, (i,es)))
                              NacGraph name _ -> ((name,0,i,4,True), (4,(i,emptyES)))
                let toNACInfos n i = case n of
                              NacGraph name nacInfo -> (i,nacInfo)
                              _ -> (0,(DG.empty,(M.empty,M.empty)))
                    idForest = genForestIds forest 0
                    infoForest = zipWith (mzipWith toGSandStates) forest idForest
                    nameForest = map (fmap fst) infoForest
                    statesForest = map (fmap snd) infoForest
                    statesList = map snd . filter (\st -> fst st /= 0) . concat . map Tree.flatten $ statesForest
                    nacInfos = filter (\ni -> fst ni /= 0). concat . map Tree.flatten $ zipWith (mzipWith toNACInfos) forest idForest
                let putInStore (Tree.Node (name,c,i,t,a) fs) mparent = do
                        iter <- Gtk.treeStoreAppend store mparent
                        storeSetGraphStore store iter (name,c,i,t,a,True)
                        case t of
                          0 -> mapM_ (\n -> putInStore n (Just iter)) fs
                          3 -> mapM_ (\n -> putInStore n (Just iter)) fs
                          _ -> return ()
                mapM (\n -> putInStore n Nothing) nameForest
                let (i,es) = if length statesList > 0 then statesList!!0 else (0,emptyES)
                writeIORef st es
                writeIORef undoStack $ M.fromList [(i,[]) | i <- [0 .. (maximum $ map fst statesList)]]
                writeIORef redoStack $ M.fromList [(i,[]) | i <- [0 .. (maximum $ map fst statesList)]]
                writeIORef graphStates $ M.fromList statesList
                writeIORef fileName $ Just fn
                writeIORef currentGraph i
                writeIORef currentPath [0]
                writeIORef currentGraphType 1
                writeIORef mergeMappingIORef Nothing
                writeIORef nacInfoMapIORef $ M.fromList nacInfos
                p <- Gtk.treePathNewFromIndices [0]
                Gtk.treeViewExpandToPath treeview p
                Gtk.treeViewSetCursor treeview p namesCol False
                afterSave store window graphStates changesIORefs fileName
                updateTG st activeTypeGraph possibleNodeTypes possibleEdgeTypes possibleSelectableEdgeTypes graphStates nodeTypeCBox edgeTypeCBox store
                Gtk.widgetQueueDraw canvas
      else return ()

  -- save project
  on svn #activate $ do
    storeCurrentES window st storeIORefs nacInfoMapIORef
    context <- Gtk.widgetGetPangoContext canvas
    updateAllNacs store graphStates nacInfoMapIORef context
    structs <- getStructsToSave store graphStates nacInfoMapIORef
    saved <- saveFile structs saveProject fileName window True
    if saved
      then do afterSave store window graphStates changesIORefs fileName
      else return ()

  -- save project as
  sva `on` #activate $ do
    storeCurrentES window st storeIORefs nacInfoMapIORef
    context <- Gtk.widgetGetPangoContext canvas
    updateAllNacs store graphStates nacInfoMapIORef context
    structs <- getStructsToSave store graphStates nacInfoMapIORef
    saved <- saveFileAs structs saveProject fileName window True
    if saved
      then afterSave store window graphStates changesIORefs fileName
      else return ()

  -- export grammar to .ggx (AGG format)
  eggx `on` #activate $ do
    efstOrderGG <- prepToExport store graphStates nacInfoMapIORef
    sts <- readIORef graphStates
    let tes= fromJust $ M.lookup 0 sts
        tg = editorGetGraph tes
    case efstOrderGG of
      Left msg -> showError window (T.pack msg)
      Right fstOrderGG -> do
        saveFileAs (fstOrderGG,tg) exportGGX fileName window False
        return ()

  -- export grammar to .vgg (new format containing the grammar)
  evgg `on` #activate $ do
    efstOrderGG <- prepToExport store graphStates nacInfoMapIORef
    case efstOrderGG of
      Left msg -> showError window (T.pack msg)
      Right fstOrderGG -> do
        saveFileAs fstOrderGG exportVGG fileName window False
        return ()
  

  -- Edit Menu ---------------------------------------------------------------------------------------------------------------

  -- delete item
  on del #activate $ do
    es <- readIORef st
    gtype <- readIORef currentGraphType
    case gtype of
      4 -> do
        -- remove elements from nacg
        index <- readIORef currentGraph
        nacInfoMap <- readIORef nacInfoMapIORef
        let ((nacg,nacgi), mapping) = fromJust $ M.lookup index nacInfoMap
            selected = editorGetSelected es
            nacES = editorSetSelected selected . editorSetGraph nacg . editorSetGI nacgi $ emptyES
            nacES' = deleteSelected nacES
            nacg' = editorGetGraph nacES'
            nacgi' = editorGetGI nacES'
        modifyIORef nacInfoMapIORef $ M.insert index ((nacg',nacgi'), mapping)
        -- add mergeMapping information to undo stack
        mergeM <- readIORef mergeMappingIORef
        stackUndo undoStack redoStack currentGraph es mergeM
      _ -> stackUndo undoStack redoStack currentGraph es Nothing
    modifyIORef st (\es -> deleteSelected es)
    setChangeFlags window store changedProject changedGraph currentPath currentGraph True
    updateByType
    Gtk.widgetQueueDraw canvas

  -- undo
  on udo #activate $ do
    -- apply undo
    applyUndo undoStack redoStack currentGraph st mergeMappingIORef
    -- reset nac diagraph
    index <- readIORef currentGraph
    gtype <- readIORef currentGraphType
    es <- readIORef st
    let (g,gi) = (editorGetGraph es, editorGetGI es)
    if gtype /= 4
      then return ()
      else do
        mergeMapping <- readIORef mergeMappingIORef
        path <- readIORef currentPath
        lhsdg <- getParentDiaGraph store path graphStates
        let nacdg' = diagrSubtract (g,gi) lhsdg
            um = fromMaybe (M.empty,M.empty) mergeMapping
        modifyIORef nacInfoMapIORef (M.insert index (nacdg', um))
    -- indicate changes
    sst <- readIORef lastSavedState
    let x = fromMaybe DG.empty $ M.lookup index sst
    setChangeFlags window store changedProject changedGraph currentPath currentGraph $ not (isDiaGraphEqual (g,gi) x)
    Gtk.widgetQueueDraw canvas
    updateByType

  -- redo
  on rdo #activate $ do
    -- apply redo
    applyRedo undoStack redoStack currentGraph st mergeMappingIORef
    -- change nac diagraph
    index <- readIORef currentGraph
    gtype <- readIORef currentGraphType
    es <- readIORef st
    let (g,gi) = (editorGetGraph es, editorGetGI es)
    if gtype /= 4
      then return ()
      else do
        mergeMapping <- readIORef mergeMappingIORef
        path <- readIORef currentPath
        lhsdg <- getParentDiaGraph store path graphStates
        let nacdg' = diagrSubtract (g,gi) lhsdg
            rm = fromMaybe (M.empty,M.empty) mergeMapping
        modifyIORef nacInfoMapIORef (M.insert index (nacdg', rm))
    -- indicate changes
    sst <- readIORef lastSavedState
    let x = fromMaybe DG.empty $ M.lookup index sst
    setChangeFlags window store changedProject changedGraph currentPath currentGraph $ not (isDiaGraphEqual (g,gi) x)
    Gtk.widgetQueueDraw canvas
    updateByType

  -- copy
  on cpy #activate $ do
    es <- readIORef st
    let copy = copySelected es
    writeIORef clipboard $ copy

  -- paste
  on pst #activate $ do
    es <- readIORef st
    clip <- readIORef clipboard
    gtype <- readIORef currentGraphType
    case gtype of
      4 -> do
        mergeM <- readIORef mergeMappingIORef
        stackUndo undoStack redoStack currentGraph es mergeM
      _ -> stackUndo undoStack redoStack currentGraph es Nothing
    setChangeFlags window store changedProject changedGraph currentPath currentGraph True
    modifyIORef st (pasteClipBoard clip)
    Gtk.widgetQueueDraw canvas
    updateByType

  -- cut
  on cut #activate $ do
    es <- readIORef st
    gtype <- readIORef currentGraphType
    writeIORef clipboard $ copySelected es
    modifyIORef st (\es -> deleteSelected es)
    gtype <- readIORef currentGraphType
    case gtype of
      4 -> do
        mergeM <- readIORef mergeMappingIORef
        stackUndo undoStack redoStack currentGraph es mergeM
      _ -> stackUndo undoStack redoStack currentGraph es Nothing
    setChangeFlags window store changedProject changedGraph currentPath currentGraph  True
    Gtk.widgetQueueDraw canvas
    updateByType

  -- select all
  on sla #activate $ do
    modifyIORef st (\es -> let g = editorGetGraph es
                           in editorSetSelected (nodeIds g, edgeIds g) es)
    Gtk.widgetQueueDraw canvas

  -- select edges
  on sle #activate $ do
    es <- readIORef st
    let selected = editorGetSelected es
        g = editorGetGraph es
    case selected of
      ([],[]) -> writeIORef st $ editorSetSelected ([], edgeIds g) es
      ([], e) -> return ()
      (n,e) -> writeIORef st $ editorSetSelected ([],e) es
    Gtk.widgetQueueDraw canvas

  -- select nodes
  on sln #activate $ do
    es <- readIORef st
    let selected = editorGetSelected es
        g = editorGetGraph es
    case selected of
      ([],[]) -> writeIORef st $ editorSetSelected (nodeIds g, []) es
      (n, []) -> return ()
      (n,e) -> writeIORef st $ editorSetSelected (n,[]) es
    Gtk.widgetQueueDraw canvas

  -- merge elements in NACs
  on mrg #activate $ do
    gtype <- readIORef currentGraphType
    if (gtype /= 4)
      then return ()
      else do
        es <- readIORef st
        context <- Gtk.widgetGetPangoContext canvas
        path <- readIORef currentPath
        tg <- readIORef activeTypeGraph

        -- load NAC
        index <- readIORef currentGraph
        nacInfoMap <- readIORef nacInfoMapIORef
        let nacInfo = fromMaybe (DG.empty, (M.empty,M.empty)) $ M.lookup index nacInfoMap
            
        -- merge elements
        merging <- mergeNACElements es nacInfo tg context

        case merging of
          Nothing -> return ()
          Just (((ng,ngi),mergeM),es') -> do
            -- modify IORefs and update the UI
            modifyIORef nacInfoMapIORef $ M.insert index ((ng,ngi),mergeM)
            writeIORef mergeMappingIORef $ Just (mergeM)
            writeIORef st $ es'
            stackUndo undoStack redoStack currentGraph es (Just $ snd nacInfo)
            Gtk.widgetQueueDraw canvas
            updateInspector currentGraphType st currentC currentLC typeInspWidgets (fillColorBox, nodeShapeFrame, edgeStyleFrame)
                            possibleNodeTypes possibleSelectableEdgeTypes currentNodeType currentEdgeType mergeMappingIORef
                            hostInspWidgets ruleInspWidgets nacInspWidgets (nodeTypeBox, edgeTypeBox)

  -- split elements in NACs
  on spt #activate $ do
    gtype <- readIORef currentGraphType
    if (gtype /= 4)
      then return ()
      else do
        es <- readIORef st
        tg <- readIORef activeTypeGraph
        context <- Gtk.widgetGetPangoContext canvas

        -- load lhs
        path <- readIORef currentPath
        (lhsg, lhsgi) <- getParentDiaGraph store path graphStates

        -- load nac information
        index <- readIORef currentGraph
        nacInfoMap <- readIORef nacInfoMapIORef
        let ((nacg, nacgi), (nM,eM)) = fromMaybe (DG.empty, (M.empty,M.empty)) $ M.lookup index nacInfoMap

        -- split elements
        ((nacdg',(nM',eM')),es') <- splitNACElements es ((nacg,nacgi),(nM,eM)) (lhsg, lhsgi) tg context

        -- update IORefs
        modifyIORef nacInfoMapIORef (M.insert index (nacdg', (nM',eM')))
        writeIORef mergeMappingIORef $ Just (nM',eM')
        writeIORef st es'
        stackUndo undoStack redoStack currentGraph es (Just (nM,eM))
        Gtk.widgetQueueDraw canvas


  -- View Menu ---------------------------------------------------------------------------------------------------------------

  -- zoom in
  zin `on` #activate $ do
    modifyIORef st (\es -> editorSetZoom (editorGetZoom es * 1.1) es )
    Gtk.widgetQueueDraw canvas

  -- zoom out
  zut `on` #activate $ do
    modifyIORef st (\es -> let z = editorGetZoom es * 0.9 in if z >= 0.5 then editorSetZoom z es else es)
    Gtk.widgetQueueDraw canvas

  -- 50% zoom
  z50 `on` #activate $ do 
    modifyIORef st (\es -> editorSetZoom 0.5 es )
    Gtk.widgetQueueDraw canvas

  -- reset zoom to defaults (100%)
  zdf `on` #activate $ do
    modifyIORef st (\es -> editorSetZoom 1.0 es )
    Gtk.widgetQueueDraw canvas

  -- 150% zoom
  z150 `on` #activate $ do
    modifyIORef st (\es -> editorSetZoom 1.5 es )
    Gtk.widgetQueueDraw canvas

  -- 200% zoom
  z200 `on` #activate $ do
    modifyIORef st (\es -> editorSetZoom 2.0 es )
    Gtk.widgetQueueDraw canvas

  -- reset view to defaults (reset zoom and pan)
  vdf `on` #activate $ do
    modifyIORef st (\es -> editorSetZoom 1 $ editorSetPan (0,0) es )
    Gtk.widgetQueueDraw canvas

  return mainPane







---------------------------------------------------------------------------------------------------------------------------------
--  Exported Callbacks  ---------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------

{-| 
    basic callback to handle the buttonPressed event in a canvas that shows a graph.
    This callback will mark a element of the graph as selected
-}
basicCanvasButtonPressedCallback :: IORef EditorState -> IORef (Double,Double) -> IORef (Maybe (Double,Double,Double,Double))
                            -> Gtk.DrawingArea -> Gdk.EventButton -> IO Bool
basicCanvasButtonPressedCallback st oldPoint squareSelection canvas eventButton = do
  b <- get eventButton #button
  x <- get eventButton #x
  y <- get eventButton #y
  ms <- get eventButton #state
  click <- get eventButton #type
  es <- readIORef st
  let z = editorGetZoom es
      (px,py) = editorGetPan es
      (x',y') = (x/z - px, y/z - py)
  writeIORef oldPoint (x',y')
  case (b, click == Gdk.EventType2buttonPress) of
    -- left button: select nodes and edges
    (1, False) -> do
      let (oldSN,oldSE) = editorGetSelected es
          graph = editorGetGraph es
          gi = editorGetGI es
          sNode = case selectNodeInPosition gi (x',y') of
            Nothing -> []
            Just nid -> [nid]
          sEdge = case selectEdgeInPosition graph gi (x',y') of
              Nothing -> []
              Just eid -> [eid]
      -- add/remove elements of selection
      case (sNode, sEdge, Gdk.ModifierTypeShiftMask `elem` ms, Gdk.ModifierTypeControlMask `elem` ms) of
        -- clicked in blank space with Shift not pressed -> clean selection, start square seleciton
        ([], [], False, _) -> do
          modifyIORef st (editorSetSelected ([],[]))
          writeIORef squareSelection $ Just (x',y',0,0)
        -- selected nodes or edges without modifier key:
        (n, e, False, _) -> do
          let nS = if null n then False else n!!0 `elem` oldSN
              eS = if null e then False else e!!0 `elem` oldSE
          if nS || eS
          then return ()
          else do 
            modifyIORef st (editorSetSelected (n, e))
        -- selected nodes or edges with Shift pressed -> add to selection
        (n, e, True, False) -> do
          let jointSN = removeDuplicates $ sNode ++ oldSN
              jointSE = removeDuplicates $ sEdge ++ oldSE
          modifyIORef st (editorSetGraph graph . editorSetSelected (jointSN,jointSE))
        -- selected nodes or edges with Shift + Ctrl pressed -> remove from selection
        (n, e, True, True) -> do
          let jointSN = if null n then oldSN else delete (n!!0) oldSN
              jointSE = if null e then oldSE else delete (e!!0) oldSE
          modifyIORef st (editorSetGraph graph . editorSetSelected (jointSN,jointSE))
    _ -> return () 
  Gtk.widgetQueueDraw canvas
  return False


basicCanvasMotionCallBack :: IORef EditorState -> IORef (Double,Double) -> IORef (Maybe (Double,Double,Double,Double))
                          -> Gtk.DrawingArea -> Gdk.EventMotion -> IO Bool
basicCanvasMotionCallBack st oldPoint squareSelection canvas eventMotion = do
  ms <- get eventMotion #state
  x <- get eventMotion #x
  y <- get eventMotion #y
  (ox,oy) <- readIORef oldPoint
  es <- readIORef st

  let leftButton = Gdk.ModifierTypeButton1Mask `elem` ms
      -- in case of the editor being used in a notebook or with a mouse with just two buttons, ctrl + right button can be used instead of the middle button.
      middleButton = (Gdk.ModifierTypeButton2Mask `elem` ms) || (Gdk.ModifierTypeButton3Mask `elem` ms && Gdk.ModifierTypeControlMask `elem` ms)
      (sNodes, sEdges) = editorGetSelected es
      z = editorGetZoom es
      (px,py) = editorGetPan es
      (x',y') = (x/z - px, y/z - py)

  case (leftButton, middleButton, sNodes, sEdges) of
    -- if left button is pressed and no node is selected, update square selection
    (True, False, [], []) -> do
      modifyIORef squareSelection $ liftM $ (\(a,b,c,d) -> (a,b,x'-a,y'-b))
      sq <- readIORef squareSelection
      Gtk.widgetQueueDraw canvas
    -- if left button is pressed with some elements selected, then move them
    (True, False, n, e) -> do
      modifyIORef st (\es -> moveNodes es (ox,oy) (x',y') )
      modifyIORef st (\es -> if (>0) . length . fst . editorGetSelected $ es
                              then es 
                              else moveEdges es (ox,oy) (x',y'))
      writeIORef oldPoint (x',y')
      Gtk.widgetQueueDraw canvas
    -- if middle button is pressed, then move the view
    (False ,True, _, _) -> do
      let (dx,dy) = (x'-ox,y'-oy)
      modifyIORef st (editorSetPan (px+dx, py+dy))
      Gtk.widgetQueueDraw canvas
    _ -> return ()
  return False

basicCanvasButtonReleasedCallback :: IORef EditorState -> IORef (Maybe (Double,Double,Double,Double)) -> Gtk.DrawingArea
                                  -> Gdk.EventButton -> IO Bool
basicCanvasButtonReleasedCallback st squareSelection canvas eventButton = do
  b <- get eventButton #button
  case b of
    1 -> do
      es <- readIORef st
      sq <- readIORef squareSelection
      let (n,e) = editorGetSelected es
      case (editorGetSelected es,sq) of
        -- if release the left button when there's a square selection,
        -- select the elements that are inside the selection
        (([],[]), Just (x,y,w,h)) -> do
          let graph = editorGetGraph es
              (ngiM, egiM) = editorGetGI es
              sNodes = map NodeId $ M.keys $
                                    M.filter (\ngi -> let pos = position ngi
                                                      in pointInsideRectangle pos (x + (w/2), y + (h/2), abs w, abs h)) ngiM
              sEdges = map edgeId $ filter (\e -> let pos = getEdgePosition graph (ngiM,egiM) e
                                                  in pointInsideRectangle pos (x + (w/2), y + (h/2), abs w, abs h)) $ edges graph
              newEs = editorSetSelected (sNodes, sEdges) $ es
          writeIORef st newEs
        ((n,e), Nothing) -> return ()
        _ -> return ()
    _ -> return ()
  writeIORef squareSelection Nothing
  Gtk.widgetQueueDraw canvas
  return False





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
                 -> IORef EditorState
                 -> IORef (M.Map Int32 (DiaGraph, MergeMapping))
                 -> IORef (Maybe String)
                 -> StoreIORefs
                 -> IO Bool
confirmOperation window store changedProject st nacInfoMapIORef fileName storeIORefs@(graphStates,_,_,_)= do 
  changed <- readIORef changedProject
  response <- if changed
    then createConfirmDialog window "The project was changed, do you want to save?"
    else return Gtk.ResponseTypeNo
  case response of
    Gtk.ResponseTypeCancel -> return False
    Gtk.ResponseTypeNo -> return True
    Gtk.ResponseTypeYes -> do
      storeCurrentES window st storeIORefs nacInfoMapIORef
      structs <- getStructsToSave store graphStates nacInfoMapIORef
      saveFile structs saveProject fileName window True -- returns True if saved the file
    _ -> return False

prepToExport :: Gtk.TreeStore
             -> IORef (M.Map Int32 EditorState)
             -> IORef (M.Map Int32 (DiaGraph, MergeMapping))
             -> IO (Either String (Grammar (TGM.TypedGraphMorphism Info Info)))
prepToExport store graphStates nacInfoMapIORef = do
  sts <- readIORef graphStates

  let tes = fromJust $ M.lookup 0 sts
      hes = fromJust $ M.lookup 1 sts
      tg = editorGetGraph tes
      hg = editorGetGraph hes

  rules <- getRules store graphStates nacInfoMapIORef
  let rulesNames = map (\(_,_,name) -> name) rules
      rulesNnacs = map (\(r,ns,_) -> (r,ns)) rules
  
  let efstOrderGG = makeGrammar tg hg rulesNnacs rulesNames
  return efstOrderGG



 



-- update the active typegraph if the corresponding diagraph is valid, set it as empty if not
updateTG :: IORef EditorState 
          -> IORef (Graph Info Info) 
          -> IORef (M.Map String (NodeGI, Int32))
          -> IORef (M.Map String (M.Map (String, String) EdgeGI, Int32)) 
          -> IORef (M.Map String (M.Map (String, String) EdgeGI, Int32))
          -> IORef (M.Map Int32 EditorState)
          -> Gtk.ComboBoxText
          -> Gtk.ComboBoxText
          -> Gtk.TreeStore
          -> IO ()
updateTG st activeTypeGraph possibleNodeTypes possibleEdgeTypes possibleSelectableEdgeTypes graphStates nodeTypeCBox edgeTypeCBox store = 
  do
    es <- readIORef st
    -- check if all edges and nodes have different names
    let g = editorGetGraph es
        giM = editorGetGI es
        nds = nodes g
        edgs = edges g

        nameConflictG = nameConflictGraph g
        diffNames = and (map nodeInfo (nodes nameConflictG)) &&
                    and (map edgeInfo (edges nameConflictG))

    let (tg, pNT, pET) = if diffNames
              then  let
                      pNT =
                        M.fromList $ 
                        zipWith (\i (k,gi) -> (k, (gi, i)) ) [0..] $
                        M.toList $
                        foldr (\(Node nid info) m -> let ngi = getNodeGI (fromEnum nid) (fst giM)
                                                        in M.insert (infoLabelStr info) ngi m) 
                              M.empty nds
                      pET =
                        M.fromList $
                        zipWith (\i (k,sm) -> (k, (sm, i)) ) [0..] $
                        M.toList $
                        foldr (\(Edge eid s t info) m -> let 
                                                            sourceT = infoLabelStr . nodeInfo . fromJust $ lookupNode s g
                                                            targetT = infoLabelStr . nodeInfo . fromJust $ lookupNode t g
                                                            edgeT = infoLabelStr info
                                                            egi = getEdgeGI (fromEnum eid) (snd giM)
                                                            subM = case M.lookup edgeT m of 
                                                              Nothing -> M.singleton (sourceT,targetT) egi
                                                              Just sm -> M.insert (sourceT,targetT) egi sm
                                                          in M.insert edgeT subM m) 
                              M.empty edgs
                    in (g,pNT, pET)
              else (G.empty, M.empty, M.empty)

    writeIORef activeTypeGraph tg
    writeIORef possibleNodeTypes pNT
    writeIORef possibleEdgeTypes pET
    writeIORef possibleSelectableEdgeTypes pET

    -- update the comboBoxes
    updateComboBoxText nodeTypeCBox (map T.pack $ M.keys pNT)
    updateComboBoxText edgeTypeCBox (map T.pack $ M.keys pET)
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
                            -> EditorState 
                            -> Graph Info Info 
                            -> [EdgeId]
                            -> IO ()
changeEdgeTypeCBoxByContext possibleEdgeTypes possibleSelectableEdgeTypes edgeTypeCBox es tg sEdges = do
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
      let g = editorGetGraph es
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
        g = editorGetGraph es
        edgs = map fromJust . filter (\e -> not $ null e) $ map (\eid -> lookupEdge eid g) eids
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
createNode' :: IORef EditorState -> Info -> Bool -> GIPos -> NodeShape -> GIColor -> GIColor -> P.Context ->  IO ()
createNode' st info autoNaming pos nshape color lcolor context = do
  es <- readIORef st
  let nid = head $ newNodes (editorGetGraph es)
      info' = if infoLabelStr info == "" && autoNaming then infoSetLabel info (show $ fromEnum nid) else info
  dim <- getStringDims (infoVisible info') context Nothing
  writeIORef st $ createNode es pos dim info' nshape color lcolor    

-- auxiliar function to prepare the treeStore to save
-- auxiliar function, add the current editor state in the graphStates list
storeCurrentES :: Gtk.Window -> IORef EditorState -> StoreIORefs -> IORef (M.Map Int32 (DiaGraph, MergeMapping)) -> IO ()
storeCurrentES window st (graphStates, _, currentGraph, currentGraphType) nacInfoMapIORef = do
  es <- readIORef st
  index <- readIORef currentGraph
  modifyIORef graphStates $ M.insert index es
  gtype <- readIORef currentGraphType
  -- if the current graph is a NAC, then update the nacInfo
  if gtype == 4 
    then do
      nacInfo <- readIORef nacInfoMapIORef >>= return . M.lookup index
      case nacInfo of
        Nothing -> showError window (T.pack $ "error: could not retrieve nacInfo of nac" ++ (show index))
        Just ((ng,_), mergeM) -> do
          let nacGI = extractNacGI (editorGetGraph es) (editorGetGI es) mergeM
          modifyIORef nacInfoMapIORef $ M.insert index ((ng,nacGI),mergeM)
    else return ()


-- Tree generation/manipulation ------------------------------------------------
-- given a forest, assign an id foreach node of the trees contained in it
genForestIds :: Tree.Forest a -> Int32 -> Tree.Forest Int32
genForestIds [] i = []
genForestIds (t:ts) i = t' : (genForestIds ts i')
  where (t',i') = genTreeId t i

genTreeId :: Tree.Tree a -> Int32 -> (Tree.Tree Int32, Int32)
genTreeId (Tree.Node x []) i = (Tree.Node i [], i + 1)
genTreeId (Tree.Node x f) i = (Tree.Node i f', i')
  where
    f' = genForestIds f (i+1)
    i' = (maximum (fmap maximum f')) + 1




afterSave :: Gtk.TreeStore 
          -> Gtk.Window 
          -> IORef (M.Map Int32 EditorState) 
          -> ChangesIORefs
          -> IORef (Maybe String)
          -> IO ()
afterSave store window graphStates (changedProject, changedGraph, lastSavedState) fileName  = 
        do  
          -- update changes IORefs so that the project appear as not changed
          writeIORef changedProject False
          states <- readIORef graphStates
          writeIORef changedGraph (take (length states) (repeat False))
          writeIORef lastSavedState (M.map (\es -> (editorGetGraph es, editorGetGI es)) states)
          -- clean the changed flag foreach graph in treeStore
          gvChanged <- toGValue (0::Int32)
          Gtk.treeModelForeach store $ \model path iter -> do
            Gtk.treeStoreSetValue store iter 1 gvChanged
            return False
          indicateProjChanged window False
          filename <- readIORef fileName
          case filename of
            Nothing -> set window [#title := "Verigraph-GUI"]
            Just fn -> set window [#title := T.pack ("Verigraph-GUI - " ++ fn)]


