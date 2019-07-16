{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module Editor.GraphEditor(
  startGUI
)where

-- Gtk modules
import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import qualified GI.Pango as P
import Data.GI.Base
import Data.GI.Base.GValue
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.Rendering.Cairo
import Graphics.Rendering.Pango.Layout
import Graphics.Rendering.Pango

import Data.List
import Data.Int
import Data.Char
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Tree as Tree
import Data.Monoid
import Control.Monad.Zip

-- verigraph modules
import Abstract.Category
import Abstract.Rewriting.DPO
import Data.Graphs hiding (null, empty)
import Data.TypedGraph.Morphism
import qualified Data.Graphs as G
import qualified Data.Graphs.Morphism as Morph
import qualified Data.TypedGraph as TG




-- editor modules
import Editor.Data.GraphicalInfo
import Editor.Render.Render
import Editor.Render.Geometry
import Editor.GraphEditor.UIBuilders
import Editor.Data.DiaGraph hiding (empty)
import qualified Editor.Data.DiaGraph as DG
import Editor.Data.EditorState
import Editor.GraphEditor.SaveLoad
import Editor.Data.Info
import Editor.GraphValidation
import Editor.GraphEditor.GrammarMaker
--------------------------------------------------------------------------------
-- MODULE STRUCTURES -----------------------------------------------------------
--------------------------------------------------------------------------------
{- |GraphStore
 A tuple representing what is showed in each node of the tree in the treeview
 It contains the informations:
 * name,
 * graph changed (0 - no, 1 - yes),
 * graph id,
 * type (0 - topic, 1 - typeGraph, 2 - hostGraph) and
 * active (valid for rules only)
 * valid (if the current graph is correctly mapped to the typegraph)
-}
type GraphStore = (String, Int32, Int32, Int32, Bool, Bool)


--------------------------------------------------------------------------------
-- FUNCTIONS ------------------------------------------------------------
--------------------------------------------------------------------------------

-- startGUI
-- creates the Graphical User Interface using the UIBuilders module and do the bindings
startGUI :: IO()
startGUI = do
  -- init GTK
  Gtk.init Nothing

  ------------------------------------------------------------------------------
  -- GUI definition ------------------------------------------------------------
  -- help window ---------------------------------------------------------------
  helpWindow <- buildHelpWindow

  -- main window ---------------------------------------------------------------
  -- creates the main window, containing the canvas and the slots to place the panels
  -- shows the main window
  (window, canvas, mainBox, treeFrame, inspectorFrame, fileItems, editItems, viewItems, helpItems) <- buildMainWindow
  let (newm,opn,svn,sva,eggx,svg,opg) = fileItems
      (del,udo,rdo,cpy,pst,cut,sla,sln,sle) = editItems
      (zin,zut,z50,zdf,z150,z200,vdf) = viewItems
      (hlp,abt) = helpItems
  -- creates the tree panel
  (treeBox, treeview, changesRenderer, nameRenderer, activeRenderer, createBtn, btnRmv) <- buildTreePanel
  Gtk.containerAdd treeFrame treeBox
  -- creates the inspector panel and add to the window

  (typeInspBox, typeNameBox, colorBtn, lineColorBtn, radioShapes, radioStyles, tPropBoxes) <- buildTypeInspector

  nameEntry <- new Gtk.Entry []
  Gtk.widgetSetCanFocus nameEntry True
  Gtk.boxPackStart typeNameBox nameEntry True True 0

  Gtk.containerAdd inspectorFrame typeInspBox
  let
    typeInspWidgets = (nameEntry, colorBtn, lineColorBtn, radioShapes, radioStyles)
    [radioCircle, radioRect, radioSquare] = radioShapes
    [radioNormal, radioPointed, radioSlashed] = radioStyles

  (hostInspBox, hostNameBox, autoLabelN, autoLabelE, nodeTCBox, edgeTCBox, hostInspBoxes) <- buildHostInspector
  (ruleInspBox, ruleNameBox, autoLabelNR, autoLabelER, nodeTCBoxR, edgeTCBoxR, operationCBox, ruleInspBoxes) <- buildRuleInspector
  let
    hostInspWidgets = (nameEntry, nodeTCBox, edgeTCBox)
    ruleInspWidgets = (nameEntry, nodeTCBoxR, edgeTCBoxR, operationCBox)

  (rvwindow, lhsCanvas, rhsCanvas) <- buildRuleViewWindow window

  #showAll window

  -- init an model to display in the tree panel --------------------------------
  store <- Gtk.treeStoreNew [gtypeString, gtypeInt, gtypeInt, gtypeInt, gtypeBoolean, gtypeBoolean]
  Gtk.treeViewSetModel treeview (Just store)
  initStore store treeview

  changesCol <- Gtk.treeViewGetColumn treeview 0
  namesCol <- Gtk.treeViewGetColumn treeview 1
  activeCol <- Gtk.treeViewGetColumn treeview 2

  case changesCol of
    Nothing -> return ()
    Just col -> Gtk.treeViewColumnSetCellDataFunc col changesRenderer $ Just (\column renderer model iter -> do
      changed <- Gtk.treeModelGetValue model iter 1 >>= fromGValue:: IO Int32
      valid <- Gtk.treeModelGetValue model iter 5 >>= fromGValue :: IO Bool
      renderer' <- castTo Gtk.CellRendererText renderer
      case (renderer', changed, valid) of
        (Just r, 0, True)  -> set r [#text := ""  ]
        (Just r, 1, True) -> set r [#text := "*" ]
        (Just r, 0, False)  -> set r [#text := "!" ]
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

  ------------------------------------------------------------------------------
  -- init the editor variables  ------------------------------------------------
  st              <- newIORef emptyES -- actual state: all the necessary info to draw the graph
  oldPoint        <- newIORef (0.0,0.0) -- last point where a mouse button was pressed
  squareSelection <- newIORef Nothing -- selection box : Maybe (x,y,w,h)
  undoStack       <- newIORef ([] :: [DiaGraph])
  redoStack       <- newIORef ([] :: [DiaGraph])
  movingGI        <- newIORef False -- if the user started moving some object - necessary to add a position to the undoStack
  clipboard       <- newIORef DG.empty -- clipboard - DiaGraph
  fileName        <- newIORef (Nothing :: Maybe String) -- name of the opened file
  currentPath     <- newIORef [0] -- indices of path to current graph being edited
  currentGraph    <- newIORef 0 -- indice of the current graph being edited
  graphStates     <- newIORef $ M.fromList [(0, (emptyES,[],[])), (1, (emptyES, [], [])), (2, (emptyES, [], []))] -- map of states foreach graph in the editor
  changedProject  <- newIORef False -- set this flag as True when the graph is changed somehow
  changedGraph    <- newIORef [False] -- when modify a graph, set the flag in the 'currentGraph' to True
  lastSavedState  <- newIORef (M.empty :: M.Map Int32 DiaGraph)
  currentGraphType <- newIORef 1

  -- variables to specify graphs
  currentShape    <- newIORef NCircle -- the shape that all new nodes must have
  currentStyle    <- newIORef ENormal -- the style that all new edges must have
  currentC        <- newIORef (1,1,1) -- the color to init new nodes
  currentLC       <- newIORef (0,0,0) -- the color to init new edges and the line and text of new nodes

  -- variables to specify hostGraphs
  possibleNodeTypes   <- newIORef ( M.empty :: M.Map String (NodeGI, Int32)) -- possible types that a node can have in a hostGraph. Each node type is identified by a string and specifies a pair with Graphical information and the position of the entry in the comboBox
  possibleEdgeTypes   <- newIORef ( M.empty :: M.Map String (EdgeGI, Int32)) -- similar to above.
  activeTypeGraph     <- newIORef G.empty  -- the connection information from the active typeGraph
  currentNodeType     <- newIORef Nothing
  currentEdgeType     <- newIORef Nothing

  ------------------------------------------------------------------------------
  -- EVENT BINDINGS ------------------------------------------------------------
  -- event bindings for the canvas ---------------------------------------------
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
      _ -> return ()
    return False

  -- auxiliar function to update the active type graph
  let updateTG = do
        curr <- readIORef currentGraph
        if curr /= 0 -- if the current graph is the active typeGraph, update
          then return ()
          else do
                updateActiveTG st activeTypeGraph possibleNodeTypes possibleEdgeTypes
                possibleNT <- readIORef possibleNodeTypes
                possibleET <- readIORef possibleEdgeTypes
                tg <- readIORef activeTypeGraph
                -- update the comboBoxes
                Gtk.comboBoxTextRemoveAll nodeTCBox
                Gtk.comboBoxTextRemoveAll edgeTCBox
                Gtk.comboBoxTextRemoveAll nodeTCBoxR
                Gtk.comboBoxTextRemoveAll edgeTCBoxR
                forM_ (M.keys possibleNT) $ \k -> do
                    Gtk.comboBoxTextAppendText nodeTCBox (T.pack k)
                    Gtk.comboBoxTextAppendText nodeTCBoxR (T.pack k)
                forM_ (M.keys possibleET) $ \k -> do
                    Gtk.comboBoxTextAppendText edgeTCBox (T.pack k)
                    Gtk.comboBoxTextAppendText edgeTCBoxR (T.pack k)

                -- update the valid flags
                states <- readIORef graphStates
                setValidFlags store tg states

  -- mouse button pressed on canvas
  on canvas #buttonPressEvent $ \eventButton -> do
    b <- get eventButton #button
    x <- get eventButton #x
    y <- get eventButton #y
    ms <- get eventButton #state
    click <- get eventButton #type
    es <- readIORef st
    gType <- readIORef currentGraphType
    let z = editorGetZoom es
        (px,py) = editorGetPan es
        (x',y') = (x/z - px, y/z - py)
    liftIO $ do
      writeIORef oldPoint (x',y')
      Gtk.widgetGrabFocus canvas
      case (b, click == Gdk.EventType2buttonPress) of
        --double click with left button : rename selection
        (1, True) -> do
          let (n,e) = editorGetSelected es
          if null n && null e
            then return ()
            else liftIO $ Gtk.widgetGrabFocus nameEntry
        -- left button: select nodes and edges
        (1, False)  -> liftIO $ do
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
            -- clicked in blank space with Shift not pressed
            ([], [], False, _) -> do
              modifyIORef st (editorSetSelected ([],[]))
              writeIORef squareSelection $ Just (x',y',0,0)
            -- selected nodes or edges with shift pressed:
            (n, e, False, _) -> do
              let nS = if null n then False else n!!0 `elem` oldSN
                  eS = if null e then False else e!!0 `elem` oldSE
              if nS || eS
              then return ()
              else modifyIORef st (editorSetSelected (n, e))
            -- selected nodes or edges with Shift pressed -> add to selection
            (n, e, True, False) -> do
              let jointSN = foldl (\ns n -> if n `notElem` ns then n:ns else ns) [] $ sNode ++ oldSN
                  jointSE = foldl (\ns n -> if n `notElem` ns then n:ns else ns) [] $ sEdge ++ oldSE
              modifyIORef st (editorSetGraph graph . editorSetSelected (jointSN,jointSE))
            -- selected nodes or edges with Shift + Ctrl pressed -> remove from selection
            (n, e, True, True) -> do
              let jointSN = if null n then oldSN else delete (n!!0) oldSN
                  jointSE = if null e then oldSE else delete (e!!0) oldSE
              modifyIORef st (editorSetGraph graph . editorSetSelected (jointSN,jointSE))
          Gtk.widgetQueueDraw canvas
          updateTypeInspector st currentC currentLC typeInspWidgets tPropBoxes
          case gType of
            2 -> updateHostInspector st possibleNodeTypes possibleEdgeTypes currentNodeType currentEdgeType hostInspWidgets hostInspBoxes
            3 -> updateRuleInspector st possibleNodeTypes possibleEdgeTypes currentNodeType currentEdgeType ruleInspWidgets ruleInspBoxes
            _ -> return ()
        -- right button click: create nodes and insert edges
        (3, False) -> liftIO $ do
          let g = editorGetGraph es
              gi = editorGetGI es
              dstNode = selectNodeInPosition gi (x',y')
          context <- Gtk.widgetGetPangoContext canvas
          stackUndo undoStack redoStack es
          cShape <- readIORef currentShape
          cColor <- readIORef currentC
          cLColor <- readIORef currentLC
          case (dstNode) of
            -- no selected node: create node
            Nothing -> case gType of
                0 -> return ()
                1 -> do
                  createNode' st "" True (x',y') cShape cColor cLColor context
                  setChangeFlags window store changedProject changedGraph currentPath currentGraph True
                  updateTG
                _ -> do
                  auto <- Gtk.toggleButtonGetActive autoLabelN
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
                  createNode' st (infoSetType "" t) auto (x',y') shape c lc context
                  setChangeFlags window store changedProject changedGraph currentPath currentGraph True
                  setCurrentValidFlag store st activeTypeGraph currentPath


            -- one node selected: create edges targeting this node
            Just nid -> case gType of
              0 -> return ()
              1 -> do
                estyle <- readIORef currentStyle
                color <- readIORef currentLC
                modifyIORef st (\es -> createEdges es nid "" True estyle color)
                setChangeFlags window store changedProject changedGraph currentPath currentGraph True
                updateTG
              _ -> do
                metype <- readIORef currentEdgeType
                cEstyle <- readIORef currentStyle
                cColor <- readIORef currentLC
                auto <- Gtk.toggleButtonGetActive autoLabelE
                (t,estyle,color) <- case metype of
                  Nothing -> return ("", cEstyle, cColor)
                  Just t -> do
                    pet <- readIORef possibleEdgeTypes
                    let pet' = M.map (\(gi,i) -> gi) pet
                        megi = M.lookup t pet'
                    case megi of
                      Nothing -> return ("", cEstyle, cColor)
                      Just gi -> return (t, style gi, color gi)
                modifyIORef st (\es -> createEdges es nid (infoSetType "" t) auto estyle color)
                setChangeFlags window store changedProject changedGraph currentPath currentGraph True
                setCurrentValidFlag store st activeTypeGraph currentPath

          Gtk.widgetQueueDraw canvas
          updateTypeInspector st currentC currentLC typeInspWidgets tPropBoxes
          case gType of
            2 -> updateHostInspector st possibleNodeTypes possibleEdgeTypes currentNodeType currentEdgeType hostInspWidgets hostInspBoxes
            3 -> updateRuleInspector st possibleNodeTypes possibleEdgeTypes currentNodeType currentEdgeType ruleInspWidgets ruleInspBoxes
            _ -> return ()
        _           -> return ()
      return True

  -- mouse motion on canvas
  on canvas #motionNotifyEvent $ \eventMotion -> do
    ms <- get eventMotion #state
    x <- get eventMotion #x
    y <- get eventMotion #y
    (ox,oy) <- liftIO $ readIORef oldPoint
    es <- liftIO $ readIORef st
    let leftButton = Gdk.ModifierTypeButton1Mask `elem` ms
        -- in case of the editor being used in a notebook or with a mouse with just two buttons, ctrl + right button can be used instead of the middle button.
        middleButton = Gdk.ModifierTypeButton2Mask `elem` ms || Gdk.ModifierTypeButton3Mask `elem` ms && Gdk.ModifierTypeControlMask `elem` ms
        (sNodes, sEdges) = editorGetSelected es
        z = editorGetZoom es
        (px,py) = editorGetPan es
        (x',y') = (x/z - px, y/z - py)
    case (leftButton, middleButton, sNodes, sEdges) of
      -- if left button is pressed and no node is selected, update square selection
      (True, False, [], []) -> liftIO $ do
        modifyIORef squareSelection $ liftM $ (\(a,b,c,d) -> (a,b,x'-a,y'-b))
        sq <- readIORef squareSelection
        Gtk.widgetQueueDraw canvas
      -- if left button is pressed with some elements selected, then move them
      (True, False, n, e) -> liftIO $ do
        modifyIORef st (\es -> moveNodes es (ox,oy) (x',y'))
        modifyIORef st (\es -> if (>0) . length . fst . editorGetSelected $ es then es else moveEdges es (ox,oy) (x',y'))
        writeIORef oldPoint (x',y')
        setChangeFlags window store changedProject changedGraph currentPath currentGraph True
        mv <- readIORef movingGI
        if not mv
          then do
            writeIORef movingGI True
            stackUndo undoStack redoStack es
          else return ()
        Gtk.widgetQueueDraw canvas
      -- if middle button is pressed, then move the view
      (False ,True, _, _) -> liftIO $ do
        let (dx,dy) = (x'-ox,y'-oy)
        modifyIORef st (editorSetPan (px+dx, py+dy))
        Gtk.widgetQueueDraw canvas
      _ -> return ()
    return True

  -- mouse button release on canvas
  on canvas #buttonReleaseEvent $ \eventButton -> do
    b <- get eventButton #button
    gType <- readIORef currentGraphType
    case b of
      1 -> liftIO $ do
        writeIORef movingGI False
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
            updateTypeInspector st currentC currentLC typeInspWidgets tPropBoxes
            case gType of
              2 -> updateHostInspector st possibleNodeTypes possibleEdgeTypes currentNodeType currentEdgeType hostInspWidgets hostInspBoxes
              3 -> updateRuleInspector st possibleNodeTypes possibleEdgeTypes currentNodeType currentEdgeType ruleInspWidgets ruleInspBoxes
              _ -> return ()
          ((n,e), Nothing) -> return ()
          _ -> return ()
      _ -> return ()
    liftIO $ do
      writeIORef squareSelection Nothing
      Gtk.widgetQueueDraw canvas
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
      (True,False,'p') -> do
        gt <- readIORef currentGraphType
        if gt == 3
          then do
            g <- readIORef st >>= \es -> return $ editorGetGraph es
            (lhs,k,rhs) <- return $ graphToRuleGraphs g
            putStrLn $ "lhs: " ++ show lhs ++ "\n"
            putStrLn $ "k: " ++ show k ++ "\n"
            putStrLn $ "rhs: " ++ show rhs ++ "\n"
            #showAll rvwindow
          else return ()
      _ -> return ()
    return True

  -- event bindings for the menu toolbar ---------------------------------------
  -- auxiliar functions to create/open/save the project
      -- auxiliar function to prepare the treeStore to save
  let prepToSave = do es <- readIORef st
                      undo <- readIORef undoStack
                      redo <- readIORef redoStack
                      index <- readIORef currentGraph
                      modifyIORef graphStates $ M.insert index (es,undo,redo)

      -- auxiliar function to clean the flags after saving
  let afterSave = do  writeIORef changedProject False
                      states <- readIORef graphStates
                      writeIORef changedGraph (take (length states) (repeat False))
                      writeIORef lastSavedState (M.map (\(es,_,_) -> (editorGetGraph es, editorGetGI es)) states)
                      gvChanged <- toGValue (0::Int32)
                      Gtk.treeModelForeach store $ \model path iter -> do
                        Gtk.treeStoreSetValue store iter 1 gvChanged
                        return False
                      indicateProjChanged window False
                      updateSavedState lastSavedState graphStates
                      filename <- readIORef fileName
                      case filename of
                        Nothing -> set window [#title := "Graph Editor"]
                        Just fn -> set window [#title := T.pack ("Graph Editor - " ++ fn)]

  -- auxiliar function to check if the project was changed
  -- it does the checking and if no, ask the user if them want to save.
  -- returns True if there's no changes, if the user don't wanted to save or if he wanted and the save operation was successfull
  -- returns False if the user wanted to save and the save operation failed or opted to cancel.
  let confirmOperation = do changed <- readIORef changedProject
                            response <- if changed
                              then createConfirmDialog window "The project was changed, do you want to save?"
                              else return Gtk.ResponseTypeNo
                            case response of
                              Gtk.ResponseTypeCancel -> return False
                              r -> case r of
                                Gtk.ResponseTypeNo -> return True
                                Gtk.ResponseTypeYes -> do
                                  prepToSave
                                  structs <- getStructsToSave store graphStates
                                  saveFile structs saveProject fileName window True -- returns True if saved the file

  -- new project action activated
  on newm #activate $ do
    continue <- confirmOperation
    if continue
      then do
        Gtk.treeStoreClear store
        initStore store treeview
        writeIORef st emptyES
        writeIORef undoStack []
        writeIORef redoStack []
        writeIORef fileName Nothing
        writeIORef currentPath [0]
        writeIORef currentGraphType 1
        writeIORef currentGraph 0
        writeIORef graphStates $ M.fromList [(0, (emptyES,[],[])), (1, (emptyES, [], []))]
        writeIORef lastSavedState M.empty
        writeIORef changedProject False
        writeIORef changedGraph [False]
        set window [#title := "Graph Editor"]
        Gtk.widgetQueueDraw canvas
      else return ()



  --open project
  on opn #activate $ do
    continue <- confirmOperation
    if continue
      then do
        mg <- loadFile window loadProject
        case mg of
          Nothing -> return ()
          Just (forest,fn) -> do
                Gtk.treeStoreClear store
                let emptyGSt = (emptyES, [], [])
                let toGSandStates n i = case n of
                              Topic name -> ((name,0,0,0,False), (0,(i,emptyGSt)))
                              TypeGraph name es -> ((name,0,i,1,True), (1, (i,(es,[],[]))))
                              HostGraph name es -> ((name,0,i,2,True), (2, (i,(es,[],[]))))
                              RuleGraph name es a -> ((name,0,i,3,a), (3, (i,(es,[],[]))))
                    idForest = genForestIds forest 0
                    infoForest = zipWith (mzipWith toGSandStates) forest idForest
                    nameForest = map (fmap fst) infoForest
                    statesForest = map (fmap snd) infoForest
                    statesList = map snd . filter (\st -> fst st /= 0) . concat . map Tree.flatten $ statesForest

                let putInStore (Tree.Node (name,c,i,t,a) fs) mparent =
                        case t of
                          0 -> do iter <- Gtk.treeStoreAppend store mparent
                                  storeSetGraphStore store iter (name,c,i,t,a,True)
                                  mapM (\n -> putInStore n (Just iter)) fs
                                  return ()
                          _ -> do iter <- Gtk.treeStoreAppend store mparent
                                  storeSetGraphStore store iter (name,c,i,t,a,True)
                                  return ()
                mapM (\n -> putInStore n Nothing) nameForest
                let (i,(es, _,_)) = if length statesList > 0 then statesList!!0 else (0,(emptyES,[],[]))
                writeIORef st es
                writeIORef undoStack []
                writeIORef redoStack []
                writeIORef graphStates $ M.fromList statesList
                writeIORef fileName $ Just fn
                writeIORef currentGraph i
                writeIORef currentPath [0]
                writeIORef currentGraphType 1
                p <- Gtk.treePathNewFromIndices [0]
                Gtk.treeViewExpandToPath treeview p
                Gtk.treeViewSetCursor treeview p namesCol False
                afterSave
                updateTG
                Gtk.widgetQueueDraw canvas
      else return ()

  -- save project
  on svn #activate $ do
    prepToSave
    structs <- getStructsToSave store graphStates
    saved <- saveFile structs saveProject fileName window True
    if saved
      then do afterSave
      else return ()

  -- save project as
  sva `on` #activate $ do
    prepToSave
    structs <- getStructsToSave store graphStates
    saved <- saveFileAs structs saveProject fileName window True
    if saved
      then afterSave
      else return ()

  eggx `on` #activate $ do
    sts <- readIORef graphStates

    let (tes,_,_) = fromJust $ M.lookup 0 sts
        (hes,_,_) = fromJust $ M.lookup 1 sts
        tg = editorGetGraph tes
        hg = editorGetGraph hes

    rules <- getRules store graphStates
    let rulesNames = map snd rules
        rgs = map fst rules

    mfstOrderGG <- makeGrammar tg hg rgs rulesNames
    case mfstOrderGG of
      Nothing -> showError window "No first-order productions were found, at least one is needed."
      Just fstOrderGG -> do
        saveFileAs (fstOrderGG,tg) exportGGX fileName window False
        return ()

  -- open graph
  on opg #activate $ do
    mg <- loadFile window loadGraph
    case mg of
      Just ((g,gi),path) -> do
        let splitAtToken str tkn = splitAt (1 + (fromMaybe (-1) $ findIndex (==tkn) str)) str
            getLastPart str = let splited = (splitAtToken str '/') in if fst splited == "" then str else getLastPart (snd splited)
            getName str = if (tails str)!!(length str - 3) == ".gr" then take (length str - 3) str else str
        -- add the loaded diagraph to the graphStates
        newKey <- readIORef graphStates >>= return . (+1) . maximum . M.keys
        modifyIORef graphStates $ M.insert newKey (editorSetGI gi . editorSetGraph g $ emptyES, [],[])
        -- update the treeview
        (valid,parentIter) <- Gtk.treeModelIterNthChild store Nothing 2
        if not valid
          then return ()
          else do
            iter <- Gtk.treeStoreAppend store (Just parentIter)
            let valid = True -- check if the graph is valid according to the typeGraph
            storeSetGraphStore store iter (getName . getLastPart $ path, 2, newKey, 2, True, valid)
            path <- Gtk.treeModelGetPath store iter
            Gtk.treeViewExpandToPath treeview path
            Gtk.treeViewSetCursor treeview path (Nothing :: Maybe Gtk.TreeViewColumn) False
            -- update the IORefs
            pathIndices <- Gtk.treePathGetIndices path >>= return . fromJust
            writeIORef currentPath pathIndices
            writeIORef currentGraph newKey
            modifyIORef changedGraph (\xs -> xs ++ [True])
            writeIORef changedProject True
            indicateProjChanged window True
            Gtk.widgetQueueDraw canvas
      _      -> return ()

  -- save graph
  on svg #activate $ do
    es <- readIORef st
    let g  = editorGetGraph es
        gi = editorGetGI es
    saveFileAs (g,gi) saveGraph fileName window False
    return ()

  let updateByType = do
        gt <- readIORef currentGraphType
        case gt of
          1 -> updateTG
          2 -> setCurrentValidFlag store st activeTypeGraph currentPath
          3 -> setCurrentValidFlag store st activeTypeGraph currentPath
          _ -> return ()

  -- delete item
  on del #activate $ do
    es <- readIORef st
    stackUndo undoStack redoStack es
    modifyIORef st (\es -> deleteSelected es)
    setChangeFlags window store changedProject changedGraph currentPath currentGraph True
    updateByType
    Gtk.widgetQueueDraw canvas


  -- undo
  on udo #activate $ do
    applyUndo undoStack redoStack st
    -- indicate changes
    sst <- readIORef lastSavedState
    index <- readIORef currentGraph
    es <- readIORef st
    let (g,gi) = (editorGetGraph es, editorGetGI es)
        x = case M.lookup (fromIntegral index) $ sst of
              Just diag -> diag
              Nothing -> DG.empty
    setChangeFlags window store changedProject changedGraph currentPath currentGraph $ not (isDiaGraphEqual (g,gi) x)
    Gtk.widgetQueueDraw canvas
    updateByType

  -- redo
  on rdo #activate $ do
    applyRedo undoStack redoStack st
    -- indicate changes
    sst <- readIORef lastSavedState
    index <- readIORef currentGraph
    es <- readIORef st
    let (g,gi) = (editorGetGraph es, editorGetGI es)
        x = case M.lookup (fromIntegral index) $ sst of
              Just diag -> diag
              Nothing -> DG.empty
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
    stackUndo undoStack redoStack es
    setChangeFlags window store changedProject changedGraph currentPath currentGraph True
    modifyIORef st (pasteClipBoard clip)
    Gtk.widgetQueueDraw canvas
    updateByType

  -- cut
  on cut #activate $ do
    es <- readIORef st
    writeIORef clipboard $ copySelected es
    modifyIORef st (\es -> deleteSelected es)
    stackUndo undoStack redoStack es
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

  -- zoom in
  zin `on` #activate $ do
    modifyIORef st (\es -> editorSetZoom (editorGetZoom es * 1.1) es )
    Gtk.widgetQueueDraw canvas

  -- zoom out
  zut `on` #activate $ do
    modifyIORef st (\es -> let z = editorGetZoom es * 0.9 in if z >= 0.5 then editorSetZoom z es else es)
    Gtk.widgetQueueDraw canvas

  z50 `on` #activate $ do
    modifyIORef st (\es -> editorSetZoom 0.5 es )
    Gtk.widgetQueueDraw canvas

  -- reset zoom to defaults
  zdf `on` #activate $ do
    modifyIORef st (\es -> editorSetZoom 1.0 es )
    Gtk.widgetQueueDraw canvas

  z150 `on` #activate $ do
    modifyIORef st (\es -> editorSetZoom 1.5 es )
    Gtk.widgetQueueDraw canvas

  z200 `on` #activate $ do
    modifyIORef st (\es -> editorSetZoom 2.0 es )
    Gtk.widgetQueueDraw canvas

  -- reset view to defaults (reset zoom and pan)
  vdf `on` #activate $ do
    modifyIORef st (\es -> editorSetZoom 1 $ editorSetPan (0,0) es )
    Gtk.widgetQueueDraw canvas

  -- help
  hlp `on` #activate $ do
    #showAll helpWindow



  -- event bindings -- inspector panel -----------------------------------------
  -- pressed a key when editing the nameEntry
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
    es <- readIORef st
    stackUndo undoStack redoStack es
    setChangeFlags window store changedProject changedGraph currentPath currentGraph True
    name <- Gtk.entryGetText nameEntry >>= return . T.unpack
    context <- Gtk.widgetGetPangoContext canvas
    renameSelected st name context
    Gtk.widgetQueueDraw canvas
    updateHostInspector st possibleNodeTypes possibleEdgeTypes currentNodeType currentEdgeType hostInspWidgets hostInspBoxes
    updateByType
    return False

  on autoLabelN #toggled $ do
    active <- Gtk.toggleButtonGetActive autoLabelN
    set autoLabelNR [#active := active]

  on autoLabelNR #toggled $ do
    active <- Gtk.toggleButtonGetActive autoLabelNR
    set autoLabelN [#active := active]

  on autoLabelE #toggled $ do
    active <- Gtk.toggleButtonGetActive autoLabelE
    set autoLabelER [#active := active]

  on autoLabelER #toggled $ do
    active <- Gtk.toggleButtonGetActive autoLabelER
    set autoLabelE [#active := active]


  -- select a fill color
  -- change the selection fill color and
  -- set the current fill color as the selected color
  on colorBtn #colorSet $ do
    gtkcolor <- Gtk.colorChooserGetRgba colorBtn
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
        stackUndo undoStack redoStack es
        setChangeFlags window store changedProject changedGraph currentPath currentGraph True
        modifyIORef st (\es -> editorSetGI (newngiM, egiM) es)
        Gtk.widgetQueueDraw canvas
        updateTG

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
        stackUndo undoStack redoStack es
        setChangeFlags window store changedProject changedGraph currentPath currentGraph True
        modifyIORef st (\es -> editorSetGI (newngiM, newegiM) es)
        Gtk.widgetQueueDraw canvas
        updateTG

  -- toogle the radio buttons for node shapes
  -- change the shape of the selected nodes and set the current shape for new nodes
  radioCircle `on` #toggled $ do
    writeIORef currentShape NCircle
    es <- readIORef st
    active <- get radioCircle #active
    let nds = fst $ editorGetSelected es
        giM = fst $ editorGetGI es
    if not active || M.null (M.filterWithKey (\k gi -> NodeId k `elem` nds && shape gi /= NCircle) giM)
      then return ()
      else do
        stackUndo undoStack redoStack es
        setChangeFlags window store changedProject changedGraph currentPath currentGraph True
        modifyIORef st (\es -> changeNodeShape es NCircle)
        Gtk.widgetQueueDraw canvas
        updateTG

  radioRect `on` #toggled $ do
    writeIORef currentShape NRect
    es <- readIORef st
    active <- get radioRect #active
    let nds = fst $ editorGetSelected es
        giM = fst $ editorGetGI es
    if not active || M.null (M.filterWithKey (\k gi -> NodeId k `elem` nds && shape gi /= NRect) giM)
      then return ()
      else do
        stackUndo undoStack redoStack es
        setChangeFlags window store changedProject changedGraph currentPath currentGraph True
        modifyIORef st (\es -> changeNodeShape es NRect)
        Gtk.widgetQueueDraw canvas
        updateTG

  radioSquare `on` #toggled $ do
    writeIORef currentShape NSquare
    es <- readIORef st
    active <- get radioSquare #active
    let nds = fst $ editorGetSelected es
        giM = fst $ editorGetGI es
    if not active || M.null (M.filterWithKey (\k gi -> NodeId k `elem` nds && shape gi /= NSquare) giM)
      then return ()
      else do
        stackUndo undoStack redoStack es
        setChangeFlags window store changedProject changedGraph currentPath currentGraph True
        modifyIORef st (\es -> changeNodeShape es NSquare)
        Gtk.widgetQueueDraw canvas
        updateTG

  -- toogle the radio buttons for edge styles
  -- change the style of the selected edges and set the current style for new edges
  radioNormal `on` #toggled $ do
    writeIORef currentStyle ENormal
    es <- readIORef st
    active <- get radioNormal #active
    let edgs = snd $ editorGetSelected es
        giM = snd $ editorGetGI es
    if not active || M.null (M.filterWithKey (\k gi -> EdgeId k `elem` edgs && style gi /= ENormal) giM)
      then return ()
      else do
        stackUndo undoStack redoStack es
        setChangeFlags window store changedProject changedGraph currentPath currentGraph True
        modifyIORef st (\es -> changeEdgeStyle es ENormal)
        Gtk.widgetQueueDraw canvas
        updateTG

  radioPointed `on` #toggled $ do
    writeIORef currentStyle EPointed
    es <- readIORef st
    active <- get radioPointed #active
    let edgs = snd $ editorGetSelected es
        giM = snd $ editorGetGI es
    if not active || M.null (M.filterWithKey (\k gi -> EdgeId k `elem` edgs && style gi /= EPointed) giM)
      then return ()
      else do
        stackUndo undoStack redoStack es
        setChangeFlags window store changedProject changedGraph currentPath currentGraph True
        modifyIORef st (\es -> changeEdgeStyle es EPointed)
        Gtk.widgetQueueDraw canvas
        updateTG

  radioSlashed `on` #toggled $ do
    writeIORef currentStyle ESlashed
    es <- readIORef st
    active <- get radioSlashed #active
    let edgs = snd $ editorGetSelected es
        giM = snd $ editorGetGI es
    if not active || M.null (M.filterWithKey (\k gi -> EdgeId k `elem` edgs && style gi /= ESlashed) giM)
      then return ()
      else do
        stackUndo undoStack redoStack es
        setChangeFlags window store changedProject changedGraph currentPath currentGraph True
        modifyIORef st (\es -> changeEdgeStyle es ESlashed)
        Gtk.widgetQueueDraw canvas
        updateTG



  let nodeTCBoxChangedCallback comboBox= do
          t <- readIORef currentGraphType
          if t < 2
            then return ()
            else do
              index <- Gtk.comboBoxGetActive comboBox
              if index == (-1)
                then return ()
                else do
                  es <- readIORef st
                  typeInfo <- Gtk.comboBoxTextGetActiveText comboBox >>= return . T.unpack
                  typeGI <- readIORef possibleNodeTypes >>= return . fst . fromJust . M.lookup typeInfo
                  let (sNids,sEids) = editorGetSelected es
                      g = editorGetGraph es
                      giM = editorGetGI es
                      newGraph = foldl (\g nid -> updateNodePayload nid g (\info -> infoSetType info typeInfo)) g sNids
                      newNGI = foldl (\gi nid -> let ngi = getNodeGI (fromEnum nid) gi
                                                 in M.insert (fromEnum nid) (nodeGiSetPosition (position ngi) . nodeGiSetDims (dims ngi) $ typeGI) gi) (fst giM) sNids
                  writeIORef st (editorSetGI (newNGI, snd giM) . editorSetGraph newGraph $ es)
                  writeIORef currentNodeType $ Just typeInfo
                  Gtk.widgetQueueDraw canvas
                  setCurrentValidFlag store st activeTypeGraph currentPath

  -- choose a type in the type comboBox for nodes
  on nodeTCBox #changed $ nodeTCBoxChangedCallback nodeTCBox
  on nodeTCBoxR #changed $ nodeTCBoxChangedCallback nodeTCBoxR

  let edgeTCBoxCallback comboBox = do
          t <- readIORef currentGraphType
          if t < 2
            then return ()
            else do
              index <- Gtk.comboBoxGetActive comboBox
              if index == (-1)
                then return ()
                else do
                  es <- readIORef st
                  typeInfo <- Gtk.comboBoxTextGetActiveText comboBox >>= return . T.unpack
                  typeGI <- readIORef possibleEdgeTypes >>= return . fst . fromJust . M.lookup typeInfo
                  let (sNids,sEids) = editorGetSelected es
                      g = editorGetGraph es
                      giM = editorGetGI es
                      newEGI = foldl (\gi eid -> let egi = getEdgeGI (fromEnum eid) gi
                                                 in M.insert (fromEnum eid) (edgeGiSetPosition (cPosition egi) typeGI) gi) (snd giM) sEids
                      newGI = (fst giM, newEGI)
                      newGraph = foldl (\g eid -> updateEdgePayload eid g (\info -> infoSetType info typeInfo)) g sEids
                  writeIORef st (editorSetGI newGI . editorSetGraph newGraph $ es)
                  writeIORef currentEdgeType $ Just typeInfo
                  Gtk.widgetQueueDraw canvas
                  setCurrentValidFlag store st activeTypeGraph currentPath

  -- choose a type in the type comboBox for edges
  on edgeTCBox #changed $ edgeTCBoxCallback edgeTCBox
  on edgeTCBoxR #changed $ edgeTCBoxCallback edgeTCBoxR

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
              0 -> ""
              1 -> "new"
              2 -> "del"
            let (sNids,sEids) = editorGetSelected es
                g = editorGetGraph es
                gi = editorGetGI es
                newGraph  = foldl (\g nid -> updateNodePayload nid g (\info -> infoSetOperation info operationInfo)) g sNids
                newGraph' = foldl (\g eid -> updateEdgePayload eid g (\info -> infoSetOperation info operationInfo)) newGraph sEids
            context <- Gtk.widgetGetPangoContext canvas
            font <- case operationInfo == "" of
              True -> return Nothing
              False -> return $ Just "Sans Bold 10"
            ndims <- forM sNids $ \nid -> do
              dim <- getStringDims (infoVisible . nodeInfo . fromJust . G.lookupNode nid $ newGraph') context font
              return (nid, dim)
            let newNgiM = foldl (\giM (nid, dim) -> let gi = nodeGiSetDims dim $ getNodeGI (fromEnum nid) giM
                                                    in M.insert (fromEnum nid) gi giM) (fst gi) ndims
            writeIORef st (editorSetGI (newNgiM, snd gi) . editorSetGraph newGraph' $ es)
            Gtk.widgetQueueDraw canvas

  -- event bindings for the graphs' tree ---------------------------------------
  -- changed the selected graph
  let changeInspector inspectorBox nameBox = do
        child <- Gtk.containerGetChildren inspectorFrame >>= \a -> return (a!!0)
        Gtk.containerRemove inspectorFrame child
        Gtk.containerAdd inspectorFrame inspectorBox
        entryParent <- Gtk.widgetGetParent nameEntry
        case entryParent of
          Nothing -> return ()
          Just parent -> do
            mp <- castTo Gtk.Box parent
            case mp of
              Nothing -> return ()
              Just p -> Gtk.containerRemove p nameEntry
        Gtk.boxPackStart nameBox nameEntry True True 0
        #showAll inspectorFrame


  on treeview #cursorChanged $ do
    selection <- Gtk.treeViewGetSelection treeview
    (sel,model,iter) <- Gtk.treeSelectionGetSelected selection
    case sel of
      False -> return ()
      True -> do
        -- get the current path for update
        mpath <- Gtk.treeModelGetPath model iter >>= Gtk.treePathGetIndices
        path <- case mpath of
          Nothing -> return [0,0]
          Just p -> return p
        -- compare the selected graph with the current one
        cIndex <- readIORef currentGraph
        index <- Gtk.treeModelGetValue model iter 2 >>= fromGValue  :: IO Int32
        gType <- Gtk.treeModelGetValue model iter 3 >>= fromGValue  :: IO Int32
        case (cIndex == index, gType) of
          (True, _)  -> do
            -- just update the current path
            writeIORef currentPath path
          (False, 0) -> return ()
          (False, _) -> do
            -- update the current path
            writeIORef currentPath path
            -- update the current graph in the tree
            currentES <- readIORef st
            u <- readIORef undoStack
            r <- readIORef redoStack
            modifyIORef graphStates (M.insert cIndex (currentES,u,r))
            -- load the selected graph from the tree
            writeIORef currentGraphType gType
            states <- readIORef graphStates
            let maybeState = M.lookup index states
            case maybeState of
              Just (es,u,r) -> do
                writeIORef st es
                writeIORef undoStack u
                writeIORef redoStack r
                writeIORef currentGraph index
              Nothing -> return ()
        case (gType) of
          0 -> return ()
          1 -> changeInspector typeInspBox typeNameBox
          2 -> do
            changeInspector hostInspBox hostNameBox
            writeIORef currentShape NCircle
            writeIORef currentStyle ENormal
            writeIORef currentC (1,1,1)
            writeIORef currentLC (0,0,0)
            -- update nodes and edges elements according to the active typeGraph
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
                fe edge = (eid, infoType (edgeInfo edge), getEdgeGI eid egi)
                          where eid = fromEnum (edgeId edge)
                ge (i,t,gi) = case M.lookup t pet of
                                Nothing -> (i,gi)
                                Just (gi',_) -> (i,edgeGiSetColor (color gi') . edgeGiSetStyle (style gi') $ gi)
                newNodeGI = M.fromList . map gn . map fn $ nodes g
                newEdgeGI = M.fromList . map ge . map fe $ edges g
            writeIORef st (editorSetGI (newNodeGI, newEdgeGI) es)
          3 -> do
            changeInspector ruleInspBox ruleNameBox
            writeIORef currentShape NCircle
            writeIORef currentStyle ENormal
            writeIORef currentC (1,1,1)
            writeIORef currentLC (0,0,0)
            -- update nodes and edges elements according to the active typeGraph
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
                fe edge = (eid, infoType (edgeInfo edge), getEdgeGI eid egi)
                          where eid = fromEnum (edgeId edge)
                ge (i,t,gi) = case M.lookup t pet of
                                Nothing -> (i,gi)
                                Just (gi',_) -> (i,edgeGiSetColor (color gi') . edgeGiSetStyle (style gi') $ gi)
                newNodeGI = M.fromList . map gn . map fn $ nodes g
                newEdgeGI = M.fromList . map ge . map fe $ edges g
            writeIORef st (editorSetGI (newNodeGI, newEdgeGI) es)
        Gtk.widgetQueueDraw canvas

  -- pressed the 'new' button on the treeview area
  -- create a new Rule
  on createBtn #clicked $ do
    states <- readIORef graphStates
    let newKey = if M.size states > 0 then maximum (M.keys states) + 1 else 0
    (valid,parent) <- Gtk.treeModelIterNthChild store Nothing 2
    if not valid
      then return ()
      else do
        n <- Gtk.treeModelIterNChildren store (Just parent)
        iter <- Gtk.treeStoreAppend store (Just parent)
        storeSetGraphStore store iter ("Rule" ++ (show n), 0, newKey, 3, True, True)
        modifyIORef graphStates (M.insert newKey (emptyES,[],[]))

  -- pressed the 'remove' button on the treeview area
  -- remove a Rule
  on btnRmv #clicked $ do
    selection <- Gtk.treeViewGetSelection treeview
    (sel,model,iter) <- Gtk.treeSelectionGetSelected selection
    if not sel
      then return ()
      else do
        (valid, parent) <- Gtk.treeModelIterParent store iter
        if not valid
          then showError window "Selected Graph is not a rule"
          else do
            index <- Gtk.treeModelGetValue store iter 2 >>= fromGValue
            Gtk.treeStoreRemove store iter
            modifyIORef graphStates $ M.delete index

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

  -- event bindings for the main window ----------------------------------------
  -- when click in the close button, the application must close
  on window #deleteEvent $ return $ do
    continue <- confirmOperation
    if continue
      then do
        Gtk.mainQuit
        return False
      else return True

  -- run the preogram ----------------------------------------------------------
  Gtk.main

------------------------------------------------------------------------------
-- Auxiliar Functions --------------------------------------------------------
------------------------------------------------------------------------------

-- Tree generation/manipulation ------------------------------------------------
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

-- Gtk.treeStore manipulation
initStore :: Gtk.TreeStore -> Gtk.TreeView ->  IO ()
initStore store treeview = do
  fstIter <- Gtk.treeStoreAppend store Nothing
  storeSetGraphStore store fstIter ("TypeGraph", 0, 0, 1, False, True)
  sndIter <- Gtk.treeStoreAppend store Nothing
  storeSetGraphStore store sndIter ("InitialGraph", 0, 1, 2, False, True)
  rulesIter <- Gtk.treeStoreAppend store Nothing
  storeSetGraphStore store rulesIter ("Rules", 0, 0, 0, False, True)
  fstRuleIter <- Gtk.treeStoreAppend store (Just rulesIter)
  storeSetGraphStore store fstRuleIter ("Rule0", 0, 2, 3, True, True)
  path <- Gtk.treeModelGetPath store fstIter
  Gtk.treeViewSetCursor treeview path (Nothing :: Maybe Gtk.TreeViewColumn) False
  rulesPath <- Gtk.treeModelGetPath store rulesIter
  Gtk.treeViewExpandRow treeview rulesPath True
  return ()

storeSetGraphStore :: Gtk.TreeStore -> Gtk.TreeIter -> GraphStore -> IO ()
storeSetGraphStore store iter (n,c,i,t,a,v) = do
  gv0 <- toGValue (Just n)
  gv1 <- toGValue c
  gv2 <- toGValue i
  gv3 <- toGValue t
  gv4 <- toGValue a
  gv5 <- toGValue v
  #set store iter [0,1,2,3,4,5] [gv0,gv1,gv2,gv3,gv4,gv5]

getTreeStoreValues :: Gtk.TreeStore -> Gtk.TreeIter -> IO (Tree.Forest (Int32,(String,Int32,Bool)))
getTreeStoreValues store iter = do
  valT <- Gtk.treeModelGetValue store iter 3 >>= fromGValue :: IO Int32
  valN <- Gtk.treeModelGetValue store iter 0 >>= (\n -> fromGValue n :: IO (Maybe String)) >>= return . fromJust
  valI <- Gtk.treeModelGetValue store iter 2 >>= fromGValue :: IO Int32
  valA <- Gtk.treeModelGetValue store iter 4 >>= fromGValue :: IO Bool
  case valT of
    -- Topic
    0 -> do (valid, childIter) <- Gtk.treeModelIterChildren store (Just iter)
            subForest <- if valid
                          then getTreeStoreValues store childIter
                          else return []
            continue <- Gtk.treeModelIterNext store iter
            if continue
              then do
                newVals <- getTreeStoreValues store iter
                return $ (Tree.Node (valT, (valN, valI, valA)) subForest) : newVals
              else return $ (Tree.Node (valT, (valN, valI, valA)) subForest) : []
    -- Graphs
    _ -> do continue <- Gtk.treeModelIterNext store iter
            if continue
              then do
                newVals <- getTreeStoreValues store iter
                return $ (Tree.Node (valT, (valN, valI, valA)) []) : newVals
              else return $ (Tree.Node (valT, (valN, valI, valA)) []) : []

getStructsToSave :: Gtk.TreeStore -> IORef (M.Map Int32 (EditorState, [DiaGraph], [DiaGraph]))-> IO (Tree.Forest SaveInfo)
getStructsToSave store graphStates = do
  (valid, fstIter) <- Gtk.treeModelGetIterFirst store
  if not valid
    then return []
    else do
      treeNodeList <- getTreeStoreValues store fstIter
      states <- readIORef graphStates
      let structs = map
                    (fmap (\(t, (name, nid, active)) -> case t of
                                0 -> Topic name
                                1 -> let (es,_,_) = fromJust $ M.lookup nid states
                                     in TypeGraph name es
                                2 -> let (es,_,_) = fromJust $ M.lookup nid states
                                     in HostGraph name es
                                3 -> let (es,_,_) = fromJust $ M.lookup nid states
                                     in RuleGraph name es active
                    )) treeNodeList
      return structs


getRuleList :: Gtk.TreeStore ->  Gtk.TreeIter -> M.Map Int32 (EditorState, [DiaGraph], [DiaGraph]) -> IO [(Graph String String, String)]
getRuleList model iter gStates = do
  name <- Gtk.treeModelGetValue model iter 0 >>= fromGValue >>= return . fromJust :: IO String
  index <- Gtk.treeModelGetValue model iter 2 >>= fromGValue :: IO Int32
  res <- case M.lookup index gStates of
    Nothing -> return []
    Just (es, _, _) -> return $ [(editorGetGraph es, name)]
  continue <- Gtk.treeModelIterNext model iter
  if continue
    then do
      rest <- getRuleList model iter gStates
      return $ res ++ rest
    else return res

getRules :: Gtk.TreeStore -> IORef (M.Map Int32 (EditorState, [DiaGraph], [DiaGraph])) -> IO [(Graph String String,String)]
getRules model graphStates = do
  (valid, iter) <- Gtk.treeModelGetIterFromString model "2:0"
  if not valid
    then return []
    else do
      gStates <- readIORef graphStates
      getRuleList model iter gStates

-- update the inspector --------------------------------------------------------
updateTypeInspector :: IORef EditorState -> IORef (Double,Double,Double) -> IORef (Double,Double,Double) ->
                      (Gtk.Entry, Gtk.ColorButton, Gtk.ColorButton, [Gtk.RadioButton], [Gtk.RadioButton]) ->
                      (Gtk.Box, Gtk.Frame, Gtk.Frame)-> IO ()
updateTypeInspector st currentC currentLC (nameEntry, colorBtn, lcolorBtn, radioShapes, radioStyles) (hBoxColor, frameShape, frameStyle) = do
  emptyColor <- new Gdk.RGBA [#red := 0.5, #blue := 0.5, #green := 0.5, #alpha := 1.0]
  est <- readIORef st
  let g = editorGetGraph est
      ns = filter (\n -> elem (nodeId n) $ fst $ editorGetSelected est) $ nodes g
      es = filter (\e -> elem (edgeId e) $ snd $ editorGetSelected est) $ edges g
      (ngiM,egiM) = editorGetGI est
      unifyNames (x:xs) = if all (==x) xs then x else "----"
  case (length ns, length es) of
    (0,0) -> do
      (r, g, b)    <- readIORef currentC
      (r', g', b') <- readIORef currentLC
      set nameEntry [#text := ""]
      color <- new Gdk.RGBA [#red := r, #green := g, #blue := b, #alpha:=1.0]
      lcolor <- new Gdk.RGBA [#red := r', #green := g', #blue := b', #alpha:=1.0]
      Gtk.colorChooserSetRgba colorBtn color
      Gtk.colorChooserSetRgba lcolorBtn lcolor
      set hBoxColor [#visible := True]
      set frameShape [#visible := True]
      set frameStyle [#visible := True]
    (n,0) -> do
      let nid = nodeId (ns!!0)
          info = T.pack . unifyNames $ map (infoLabel . nodeInfo) ns
          --info = T.pack . unifyNames $ map nodeInfo ns
          gi = getNodeGI (fromEnum nid) ngiM
          (r,g,b) = fillColor gi
          (r',g',b') = lineColor gi
          nodeShape = shape gi
      color <- new Gdk.RGBA [#red := r, #green := g, #blue := b, #alpha := 1.0]
      lcolor <- new Gdk.RGBA [#red := r', #green := g', #blue := b', #alpha := 1.0]
      set nameEntry [#text := info]
      Gtk.colorChooserSetRgba colorBtn $ if n==1 then color else emptyColor
      Gtk.colorChooserSetRgba lcolorBtn $ if n==1 then lcolor else emptyColor
      case (n,nodeShape) of
        (1,NCircle) -> Gtk.toggleButtonSetActive (radioShapes!!0) True
        (1,NRect) -> Gtk.toggleButtonSetActive (radioShapes!!1) True
        (1,NSquare) -> Gtk.toggleButtonSetActive (radioShapes!!2) True
        _ -> return ()

      set hBoxColor [#visible := True]
      set frameShape [#visible := True]
      set frameStyle [#visible := False]
    (0,n) -> do
      let eid = edgeId (es!!0)
          info = T.pack . unifyNames $ map (infoLabel . edgeInfo) es
          --info = T.pack . unifyNames $ map edgeInfo es
          gi = getEdgeGI (fromEnum eid) egiM
          (r,g,b) = color gi
          edgeStyle = style gi
      edgeColor <- new Gdk.RGBA [#red := r, #green := g, #blue := b, #alpha := 1.0]
      set nameEntry [#text := info]
      Gtk.colorChooserSetRgba lcolorBtn $ if n == 1 then edgeColor else emptyColor
      case (n,edgeStyle) of
        (1,ENormal) -> Gtk.toggleButtonSetActive (radioStyles!!0) True
        (1,EPointed) -> Gtk.toggleButtonSetActive (radioStyles!!1) True
        (1,ESlashed) -> Gtk.toggleButtonSetActive (radioStyles!!2) True
        _ -> return ()

      set hBoxColor [#visible := False]
      set frameShape [#visible := False]
      set frameStyle [#visible := True]
    _ -> do
      let info = T.pack . unifyNames $ concat [(map (infoLabel . edgeInfo) es), (map (infoLabel . nodeInfo) ns)]
      --let info = T.pack . unifyNames $ concat [(map edgeInfo es), (map nodeInfo ns)]
      set nameEntry [#text := info ]
      Gtk.colorChooserSetRgba colorBtn emptyColor
      Gtk.colorChooserSetRgba lcolorBtn emptyColor
      set hBoxColor [#visible := True]
      set frameShape [#visible := True]
      set frameStyle [#visible := True]

updateHostInspector :: IORef EditorState -> IORef (M.Map String (NodeGI, Int32)) -> IORef (M.Map String (EdgeGI, Int32)) ->
                       IORef (Maybe String) -> IORef (Maybe String) -> (Gtk.Entry, Gtk.ComboBoxText, Gtk.ComboBoxText) ->
                       (Gtk.Box, Gtk.Box) -> IO()
updateHostInspector st possibleNT possibleET currentNodeType currentEdgeType (entry, nodeTCBox, edgeTCBox) (nodeTBox, edgeTBox) = do
  est <- readIORef st
  pNT <- readIORef possibleNT
  pET <- readIORef possibleET
  cNT <- readIORef currentNodeType >>= \x -> return $ fromMaybe "" x
  cET <- readIORef currentEdgeType >>= \x -> return $ fromMaybe "" x
  let g = editorGetGraph est
      ns = filter (\n -> elem (nodeId n) $ fst $ editorGetSelected est) $ nodes g
      es = filter (\e -> elem (edgeId e) $ snd $ editorGetSelected est) $ edges g
      (ngiM,egiM) = editorGetGI est
      unifyNames (x:xs) = if all (==x) xs then x else ""
  case (length ns, length es) of
    (0,0) -> do
      Gtk.comboBoxSetActive nodeTCBox $ fromMaybe (-1) (Just snd <*> (M.lookup cNT pNT))
      Gtk.comboBoxSetActive edgeTCBox $ fromMaybe (-1) (Just snd <*> (M.lookup cET pET))
      set nodeTBox [#visible := True]
      set edgeTBox [#visible := True]
    (n,0) -> do
      let typeL = unifyNames $ map (infoType . nodeInfo) ns
          typeI = case M.lookup typeL pNT of
                  Nothing -> -1
                  Just (gi,i) -> i
      Gtk.comboBoxSetActive nodeTCBox typeI
      set nodeTBox [#visible := True]
      set edgeTBox [#visible := False]
    (0,e) -> do
      let typeL = unifyNames $ map (infoType . edgeInfo) es
          typeI = case M.lookup typeL pET of
                  Nothing -> -1
                  Just (gi,i) -> i

      Gtk.comboBoxSetActive edgeTCBox typeI
      set edgeTBox [#visible := True]
      set nodeTBox [#visible := False]
    (n,e) -> do
      let typeNL = unifyNames $ map (infoType . nodeInfo) ns
          typeNI = case M.lookup typeNL pNT of
                  Nothing -> -1
                  Just (gi,i) -> i
          typeEL = unifyNames $ map (infoType . edgeInfo) es
          typeEI = case M.lookup typeEL pET of
                  Nothing -> -1
                  Just (gi,i) -> i
      Gtk.comboBoxSetActive nodeTCBox typeNI
      Gtk.comboBoxSetActive edgeTCBox typeEI
      set edgeTBox [#visible := True]
      set nodeTBox [#visible := True]

updateRuleInspector :: IORef EditorState -> IORef (M.Map String (NodeGI, Int32)) -> IORef (M.Map String (EdgeGI, Int32)) ->
                       IORef (Maybe String) -> IORef (Maybe String) -> (Gtk.Entry, Gtk.ComboBoxText, Gtk.ComboBoxText, Gtk.ComboBoxText) ->
                       (Gtk.Box, Gtk.Box) -> IO()
updateRuleInspector st possibleNT possibleET currentNodeType currentEdgeType (entry, nodeTCBox, edgeTCBox, operationCBox) (nodeTBox, edgeTBox) = do
  est <- readIORef st
  pNT <- readIORef possibleNT
  pET <- readIORef possibleET
  cNT <- readIORef currentNodeType >>= \x -> return $ fromMaybe "" x
  cET <- readIORef currentEdgeType >>= \x -> return $ fromMaybe "" x
  let g = editorGetGraph est
      ns = filter (\n -> elem (nodeId n) $ fst $ editorGetSelected est) $ nodes g
      es = filter (\e -> elem (edgeId e) $ snd $ editorGetSelected est) $ edges g
      (ngiM,egiM) = editorGetGI est
      unifyNames [] = ""
      unifyNames (x:xs) = if all (==x) xs then x else "------"
      typeNL = unifyNames $ map (infoType . nodeInfo) ns
      typeEL = unifyNames $ map (infoType . edgeInfo) es
      operation = unifyNames $ concat [map (infoOperation . edgeInfo) es, map (infoOperation . nodeInfo) ns]
      typeNI = case M.lookup typeNL pNT of
              Nothing -> -1
              Just (gi,i) -> i
      typeEI = case M.lookup typeEL pET of
              Nothing -> -1
              Just (gi,i) -> i
      opI = case operation of
        "" -> 0
        "new" -> 1
        "del" -> 2
        _ -> -1
  case (length ns, length es) of
    (0,0) -> do
      Gtk.comboBoxSetActive nodeTCBox $ fromMaybe (-1) (Just snd <*> (M.lookup cNT pNT))
      Gtk.comboBoxSetActive edgeTCBox $ fromMaybe (-1) (Just snd <*> (M.lookup cET pET))
      Gtk.comboBoxSetActive operationCBox opI
      set nodeTBox [#visible := True]
      set edgeTBox [#visible := True]
    (n,0) -> do
      Gtk.comboBoxSetActive nodeTCBox typeNI
      Gtk.comboBoxSetActive operationCBox opI
      set nodeTBox [#visible := True]
      set edgeTBox [#visible := False]
    (0,e) -> do
      Gtk.comboBoxSetActive edgeTCBox typeEI
      Gtk.comboBoxSetActive operationCBox opI
      set edgeTBox [#visible := True]
      set nodeTBox [#visible := False]
    (n,e) -> do
      Gtk.comboBoxSetActive nodeTCBox typeNI
      Gtk.comboBoxSetActive edgeTCBox typeEI
      Gtk.comboBoxSetActive operationCBox opI
      set edgeTBox [#visible := True]
      set nodeTBox [#visible := True]




-- draw a graph in the canvas --------------------------------------------------
drawTypeGraph :: EditorState -> Maybe (Double,Double,Double,Double)-> Render ()
drawTypeGraph (g, (nGI,eGI), (sNodes, sEdges), z, (px,py)) sq = do
  scale z z
  translate px py

  let selectColor = (0.29,0.56,0.85)
      errorColor = (0.9,0.2,0.2)
      bothColor = (0.47,0.13,0.87)

  let cg = nameConflictGraph g


  -- draw the edges
  forM (edges g) (\e -> do
    let dstN = M.lookup (fromEnum . targetId $ e) nGI
        srcN = M.lookup (fromEnum . sourceId $ e) nGI
        egi  = M.lookup (fromEnum . edgeId   $ e) eGI
        selected = (edgeId e) `elem` sEdges
        conflict = case lookupEdge (edgeId e) cg of
          Just e' -> not $ edgeInfo e'
          Nothing -> True
        shadowColor = case (selected, conflict) of
          (False,False) -> (0,0,0)
          (False,True) -> errorColor
          (True,False) -> selectColor
          (True,True) -> bothColor
    case (egi, srcN, dstN) of
      (Just gi, Just src, Just dst) -> renderEdge gi (infoLabel $ edgeInfo e) src dst (selected || conflict) shadowColor False (0,0,0)
      _ -> return ())

  -- draw the nodes
  forM (nodes g) (\n -> do
    let ngi = M.lookup (fromEnum . nodeId $ n) nGI
        selected = (nodeId n) `elem` (sNodes)
        conflict = case lookupNode (nodeId n) cg of
          Just n' -> not $ nodeInfo n'
          Nothing -> True
        shadowColor = case (selected, conflict) of
          (False,False) -> (0,0,0)
          (False,True) -> errorColor
          (True,False) -> selectColor
          (True,True) -> bothColor
        info = infoLabel $ nodeInfo n
    case (ngi) of
      Just gi -> renderNode gi info (selected || conflict) shadowColor False (0,0,0)
      Nothing -> return ())

  -- draw the selectionBox
  case sq of
    Just (x,y,w,h) -> do
      rectangle x y w h
      setSourceRGBA 0.29 0.56 0.85 0.5
      fill
      rectangle x y w h
      setSourceRGBA 0.29 0.56 0.85 1
      stroke
    Nothing -> return ()
  return ()

drawHostGraph :: EditorState -> Maybe (Double,Double,Double,Double) -> Graph String String -> Render ()
drawHostGraph (g, (nGI,eGI), (sNodes, sEdges), z, (px,py)) sq tg = do
  scale z z
  translate px py

  -- specify colors for select and error
  let selectColor = (0.29,0.56,0.85)
      errorColor = (0.9,0.2,0.2)
      bothColor = (0.47,0.13,0.87)

  let vg = correctTypeGraph g tg
  -- draw the edges
  forM (edges g) (\e -> do
    let dstN = M.lookup (fromEnum . targetId $ e) nGI
        srcN = M.lookup (fromEnum . sourceId $ e) nGI
        egi  = M.lookup (fromEnum . edgeId   $ e) eGI
        selected = (edgeId e) `elem` sEdges
        typeError = case lookupEdge (edgeId e) vg of
                      Just e' -> not $ edgeInfo e'
                      Nothing -> True
        color = case (selected,typeError) of
                 (False,False) -> (0,0,0)
                 (False,True) -> errorColor
                 (True,False) -> selectColor
                 (True,True) -> bothColor
    case (egi, srcN, dstN) of
      (Just gi, Just src, Just dst) -> renderEdge gi (infoLabel (edgeInfo e)) src dst (selected || typeError) color False (0,0,0)
      _ -> return ())

  -- draw the nodes
  forM (nodes g) (\n -> do
    let ngi = M.lookup (fromEnum . nodeId $ n) nGI
        selected = (nodeId n) `elem` (sNodes)
        info = nodeInfo n
        label = infoLabel info
        typeError = case lookupNode (nodeId n) vg of
                      Just n' -> not $ nodeInfo n'
                      Nothing -> True
        color = case (selected,typeError) of
                  (False,False) -> (0,0,0)
                  (False,True) -> errorColor
                  (True,False) -> selectColor
                  (True,True) -> bothColor
    case (ngi) of
      Just gi -> renderNode gi label (selected || typeError) color False (0,0,0)
      Nothing -> return ())

  -- draw the selectionBox
  case sq of
    Just (x,y,w,h) -> do
      rectangle x y w h
      setSourceRGBA 0.29 0.56 0.85 0.5
      fill
      rectangle x y w h
      setSourceRGBA 0.29 0.56 0.85 1
      stroke
    Nothing -> return ()
  return ()

drawRuleGraph :: EditorState -> Maybe (Double,Double,Double,Double) -> Graph String String -> Render ()
drawRuleGraph (g, (nGI,eGI), (sNodes, sEdges), z, (px,py)) sq tg = do
  scale z z
  translate px py

  -- specify colors for select and error
  let selectColor = (0.29,0.56,0.85)
      errorColor = (0.9,0.2,0.2)
      bothColor = (0.47,0.13,0.87)
      createColor = (0.12, 0.48, 0.10)
      deleteColor = (0.17, 0.28, 0.77)

  let vg = correctTypeGraph g tg
  let ovg = opValidationGraph g

  -- draw the edges
  forM (edges g) (\e -> do
    let dstN = M.lookup (fromEnum . targetId $ e) nGI
        srcN = M.lookup (fromEnum . sourceId $ e) nGI
        egi  = M.lookup (fromEnum . edgeId   $ e) eGI
        info = infoVisible (edgeInfo e)
        selected = (edgeId e) `elem` sEdges
        typeError = case lookupEdge (edgeId e) vg of
                      Just e' -> not $ edgeInfo e'
                      Nothing -> True
        operationError = case lookupEdge (edgeId e) ovg of
                          Just e' -> not $ edgeInfo e'
                          Nothing -> True
        color = case (selected,typeError || operationError) of
                 (False,False) -> (0,0,0)
                 (False,True) -> errorColor
                 (True,False) -> selectColor
                 (True,True) -> bothColor
        (highlight, textColor) = case (infoOperation (edgeInfo e)) of
          "new" -> (True, createColor)
          "del" -> (True, deleteColor)
          ""    -> (False, (0,0,0))
          _     -> (True, errorColor)
    case (egi, srcN, dstN) of
      (Just gi, Just src, Just dst) -> renderEdge gi info src dst (selected || typeError || operationError) color highlight textColor
      _ -> return ())

  -- draw the nodes
  forM (nodes g) (\n -> do
    let ngi = M.lookup (fromEnum . nodeId $ n) nGI
        selected = (nodeId n) `elem` (sNodes)
        info = nodeInfo n
        label = infoVisible info
        typeError = case lookupNode (nodeId n) vg of
                      Just n' -> not $ nodeInfo n'
                      Nothing -> True
        color = case (selected,typeError) of
                  (False,False) -> (0,0,0)
                  (False,True) -> errorColor
                  (True,False) -> selectColor
                  (True,True) -> bothColor
        (highlight, textColor) = case (infoOperation info) of
          "new" -> (True, createColor)
          "del" -> (True, deleteColor)
          ""    -> (False, (0,0,0))
          _     -> (True, errorColor)
    case (ngi) of
      Just gi -> renderNode gi label (selected || typeError) color highlight textColor
      Nothing -> return ())

  -- draw the selectionBox
  case sq of
    Just (x,y,w,h) -> do
      rectangle x y w h
      setSourceRGBA 0.29 0.56 0.85 0.5
      fill
      rectangle x y w h
      setSourceRGBA 0.29 0.56 0.85 1
      stroke
    Nothing -> return ()
  return ()

-- update the active typegraph if the corresponding diagraph is valid, set it as empty if not
updateActiveTG :: IORef EditorState -> IORef (Graph String String) -> IORef (M.Map String (NodeGI, Int32)) -> IORef (M.Map String (EdgeGI, Int32)) -> IO ()
updateActiveTG st activeTypeGraph possibleNodeTypes possibleEdgeTypes = do
        es <- readIORef st
        -- check if all edges and nodes have different names
        let g = editorGetGraph es
            giM = editorGetGI es
            nds = nodes g
            edgs = edges g
            allDiff l = case l of
                          [] -> True
                          x:xs -> (notElem x xs) && (allDiff xs)
            diffNames = (allDiff (map (infoLabel . nodeInfo) nds)) &&
                        (allDiff (map (infoLabel . edgeInfo) edgs)) &&
                        notElem "" (concat [(map (infoLabel . edgeInfo) edgs), (map (infoLabel . nodeInfo) nds)])
        if diffNames
          then do -- load the variables with the info from the typeGraph
            writeIORef activeTypeGraph g
            writeIORef possibleNodeTypes $
                M.fromList $
                zipWith (\i (k,gi) -> (k, (gi, i)) ) [0..] $
                M.toList $
                foldr (\(Node nid info) m -> let ngi = getNodeGI (fromEnum nid) (fst giM)
                                                 in M.insert (infoLabel info) ngi m) M.empty nds
            writeIORef possibleEdgeTypes $
                M.fromList $
                zipWith (\i (k,gi) -> (k, (gi, i)) ) [0..] $
                M.toList $
                foldr (\(Edge eid _ _ info) m -> let egi = getEdgeGI (fromEnum eid) (snd giM)
                                                     in M.insert (infoLabel info) egi m) M.empty edgs
          else do
            writeIORef activeTypeGraph (G.empty)
            writeIORef possibleNodeTypes (M.empty)
            writeIORef possibleEdgeTypes (M.empty)

-- graph interaction
-- create a new node, auto-generating it's name and dimensions
createNode' :: IORef EditorState -> String -> Bool -> GIPos -> NodeShape -> GIColor -> GIColor -> P.Context ->  IO ()
createNode' st content autoNaming pos nshape color lcolor context = do
  es <- readIORef st
  let nid = head $ newNodes (editorGetGraph es)
      content' = if infoVisible content == "" && autoNaming then infoSetLabel content (show nid) else content
  dim <- getStringDims (infoVisible content') context Nothing
  writeIORef st $ createNode es pos dim content' nshape color lcolor

-- rename the selected itens
renameSelected:: IORef EditorState -> String -> P.Context -> IO()
renameSelected state content context = do
  es <- readIORef state
  dim <- getStringDims (infoVisible content) context Nothing
  let graph = editorGetGraph es
      (nids,eids) = editorGetSelected es
      (ngiM,egiM) = editorGetGI es
  let rename oldInfo = if (infoType content) /= ""
                        then content
                        else infoSetType content (infoType oldInfo)
  let graph' = foldl (\g nid -> updateNodePayload nid g rename) graph nids
      newGraph  = foldl (\g eid -> updateEdgePayload eid g rename) graph' eids
      newNgiM = M.mapWithKey (\k gi -> if NodeId k `elem` nids then nodeGiSetDims dim gi else gi) ngiM
      newEs   = editorSetGI (newNgiM,egiM) . editorSetGraph newGraph $ es
  writeIORef state newEs

-- auxiliar function used by createNode' and renameSelected
-- given a text, compute the size of it's bounding box
-- uses the pango lib
getStringDims :: String -> P.Context -> Maybe T.Text -> IO (Double, Double)
getStringDims str context font = do
  desc <- case font of
    Just f -> P.fontDescriptionFromString f
    Nothing -> P.fontDescriptionFromString "Sans Regular 10"
  pL <- P.layoutNew context
  P.layoutSetFontDescription pL (Just desc)
  P.layoutSetText pL (T.pack str) (fromIntegral . length $ str)
  (_,rect) <- P.layoutGetExtents pL
  w <- get rect #width >>= (\n -> return . fromIntegral . quot n $ P.SCALE)
  h <- get rect #height >>= (\n -> return . fromIntegral . quot n $ P.SCALE)
  return (w +4, h + 4)


-- Undo / Redo -----------------------------------------------------------------
stackUndo :: IORef [DiaGraph] -> IORef [DiaGraph] -> EditorState -> IO ()
stackUndo undo redo es = do
  let g = editorGetGraph es
      gi = editorGetGI es
  modifyIORef undo (\u -> (g,gi):u )
  modifyIORef redo (\_ -> [])

applyUndo :: IORef [DiaGraph] -> IORef [DiaGraph] -> IORef EditorState -> IO ()
applyUndo undoStack redoStack st = do
  es <- readIORef st
  undo <- readIORef undoStack
  redo <- readIORef redoStack
  let apply [] r es = ([],r, es)
      apply ((g,gi):u) r es = (u, (eg,egi):r, editorSetGI gi . editorSetGraph g $ es)
                            where
                              eg = editorGetGraph es
                              egi = editorGetGI es
      (nu, nr, nes) = apply undo redo es
  writeIORef undoStack nu
  writeIORef redoStack nr
  writeIORef st nes

applyRedo :: IORef [DiaGraph] -> IORef [DiaGraph] -> IORef EditorState -> IO ()
applyRedo undoStack redoStack st = do
  undo <- readIORef undoStack
  redo <- readIORef redoStack
  es <- readIORef st
  let apply u [] es = (u, [], es)
      apply u ((g,gi):r) es = ((eg,egi):u, r, editorSetGI gi . editorSetGraph g $ es)
                            where
                              eg = editorGetGraph es
                              egi = editorGetGI es
      (nu, nr, nes) = apply undo redo es
  writeIORef undoStack nu
  writeIORef redoStack nr
  writeIORef st nes


-- Copy / Paste / Cut ----------------------------------------------------------
copySelected :: EditorState -> DiaGraph
copySelected  es = (cg,(ngiM',egiM'))
  where
    (nids,eids) = editorGetSelected es
    g = editorGetGraph es
    (ngiM, egiM) = editorGetGI es
    cnodes = filter (\n -> nodeId n `elem` nids) $ nodes g
    cedges = filter (\e -> edgeId e `elem` eids) $ edges g
    cg = fromNodesAndEdges cnodes cedges
    ngiM' = M.filterWithKey (\k _ -> NodeId k `elem` nids) ngiM
    egiM' = M.filterWithKey (\k _ -> EdgeId k `elem` eids) egiM

pasteClipBoard :: DiaGraph -> EditorState -> EditorState
pasteClipBoard (cGraph, (cNgiM, cEgiM)) es = editorSetGI (newngiM,newegiM) . editorSetGraph newGraph . editorSetSelected ([], [])$ es
  where
    graph = editorGetGraph es
    (ngiM, egiM) = editorGetGI es
    allPositions = concat [map position (M.elems cNgiM), map (getEdgePosition cGraph (cNgiM, cEgiM)) (edges cGraph)]
    minX = minimum $ map fst allPositions
    minY = minimum $ map snd allPositions
    upd (a,b) = (20+a-minX, 20+b-minY)
    cNgiM' = M.map (\gi -> nodeGiSetPosition (upd $ position gi) gi) cNgiM
    (newGraph, (newngiM,newegiM)) = diagrDisjointUnion (graph,(ngiM,egiM)) (cGraph,(cNgiM', cEgiM))


-- TreeStore Flags -------------------------------------------------------------
-- change window name to indicate if the project was modified
indicateProjChanged :: Gtk.Window -> Bool -> IO ()
indicateProjChanged window True = do
  ttitle <- get window #title
  let title = T.unpack . fromJust $ ttitle
  if title!!0 == '*'
    then return ()
    else set window [#title := T.pack('*':title)]

indicateProjChanged window False = do
  ttitle <- get window #title
  let i:title = T.unpack . fromJust $ ttitle
  if i == '*'
    then set window [#title := T.pack title]
    else return ()

-- write in the treestore that the current graph was modified
indicateGraphChanged :: Gtk.TreeStore -> Gtk.TreeIter -> Bool -> IO ()
indicateGraphChanged store iter True = do
  gtype <- Gtk.treeModelGetValue store iter 3 >>= fromGValue :: IO Int32
  if gtype == 0
    then return ()
    else do
      gvchanged <- toGValue (1::Int32)
      Gtk.treeStoreSetValue store iter 1 gvchanged
      (valid, parentIter) <- Gtk.treeModelIterParent store iter
      if valid
        then Gtk.treeStoreSetValue store parentIter 1 gvchanged
        else return ()

indicateGraphChanged store iter False = do
  gvchanged <- toGValue (0::Int32)
  Gtk.treeStoreSetValue store iter 1 gvchanged
  (valid, parentIter) <- Gtk.treeModelIterParent store iter
  if valid
    then Gtk.treeStoreSetValue store parentIter 1 gvchanged
    else return ()

-- change the flags that inform if the graphs and project were changed and indicate the changes
setChangeFlags :: Gtk.Window -> Gtk.TreeStore -> IORef Bool -> IORef [Bool] -> IORef [Int32] -> IORef Int32 -> Bool -> IO ()
setChangeFlags window store changedProject changedGraph currentPath currentGraph changed = do
  index <- readIORef currentGraph >>= return . fromIntegral
  xs <- readIORef changedGraph
  let xs' = take index xs ++ [changed] ++ drop (index+1) xs
      projChanged = or xs'
  writeIORef changedGraph xs'
  writeIORef changedProject $ projChanged
  indicateProjChanged window $ projChanged
  path <- readIORef currentPath >>= Gtk.treePathNewFromIndices
  (valid,iter) <- Gtk.treeModelGetIter store path
  if valid
    then indicateGraphChanged store iter changed
    else return ()

-- Analise a graph and change the flags that inform if a graph is valid/invalid
setValidFlag :: Gtk.TreeStore -> Gtk.TreeIter -> M.Map Int32 (EditorState, [DiaGraph], [DiaGraph]) -> Graph String String -> IO ()
setValidFlag store iter states tg = do
  index <- Gtk.treeModelGetValue store iter 2 >>= fromGValue :: IO Int32
  mst <- return $ M.lookup index states
  g <- case mst of
    Nothing -> return G.empty
    Just (es, u, r) -> return $ editorGetGraph es
  let valid = isGraphValid g tg
  gvValid <- toGValue valid
  Gtk.treeStoreSetValue store iter 5 gvValid

-- walk in the treeStore, applying setValidFalg for all the hostGraphs and RuleGraphs
-- should be called when occur an update to the typeGraph
setValidFlags :: Gtk.TreeStore -> Graph String String -> M.Map Int32 (EditorState, [DiaGraph], [DiaGraph]) -> IO ()
setValidFlags store tg states = do
  Gtk.treeModelForeach store $ \model path iter -> do
    t <- Gtk.treeModelGetValue store iter 3 >>= fromGValue :: IO Int32
    case t of
      0 -> return ()
      1 -> return ()
      2 -> setValidFlag store iter states tg
      3 -> setValidFlag store iter states tg
    return False

-- Change the valid flag for the graph that is being edited
-- Needed because the graphStates IORef is not updated while the user is editing the graph
setCurrentValidFlag :: Gtk.TreeStore -> IORef EditorState -> IORef (Graph String String) -> IORef [Int32] -> IO ()
setCurrentValidFlag store st typeGraph currentPath = do
      es <- readIORef st
      tg <- readIORef typeGraph
      let valid = isGraphValid (editorGetGraph es) tg
      gvValid <- toGValue valid
      cpath <- readIORef currentPath >>= Gtk.treePathNewFromIndices
      (validIter, iter) <- Gtk.treeModelGetIter store cpath
      if validIter
        then Gtk.treeStoreSetValue store iter 5 gvValid
        else return ()

-- change updatedState
updateSavedState :: IORef (M.Map Int32 DiaGraph) -> IORef (M.Map Int32 (EditorState, [DiaGraph], [DiaGraph])) -> IO ()
updateSavedState sst graphStates = do
  states <- readIORef graphStates
  writeIORef sst $ (M.map (\(es,_,_) -> (editorGetGraph es, editorGetGI es) ) states)
