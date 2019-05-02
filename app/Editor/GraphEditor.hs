{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module Editor.GraphEditor
( startGUI
)where

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
import qualified Control.Exception as E
import qualified Data.Map as M
import Data.Graphs hiding (null, empty)
import qualified Data.Graphs as G
import Editor.GraphicalInfo
import Editor.Render
import Editor.Helper
import Editor.UIBuilders
import Editor.DiaGraph hiding (empty)
import qualified Editor.DiaGraph as DG
import Editor.EditorState

--------------------------------------------------------------------------------
-- MODULE STRUCTURES -----------------------------------------------------------
--------------------------------------------------------------------------------
-- |GraphStore
-- A tuple representing what is showed in each node of the tree in the treeview
-- It contains the informations: name, graph changed (0 - no, 1 - yes, 2 - is new), and a integer that is the identifier of the graph
type GraphStore = (String, Int32, Int32)

storeSetGraphStore :: Gtk.TreeStore -> Gtk.TreeIter -> GraphStore -> IO ()
storeSetGraphStore store iter (n,c,i) = do
  gv0 <- toGValue (Just n)
  gv1 <- toGValue c
  gv2 <- toGValue i
  #set store iter [0,1,2] [gv0,gv1,gv2]


--------------------------------------------------------------------------------
-- MODULE FUNCTIONS ------------------------------------------------------------
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
  let (newm,opn,svn,sva,svg,opg) = fileItems
      (del,udo,rdo,cpy,pst,cut,sla,sln,sle) = editItems
      (zin,zut,z50,zdf,z150,z200,vdf) = viewItems
      (hlp,abt) = helpItems
  -- creates the tree panel
  (treeBox, treeview, treeRenderer, btnNew, btnRmv) <- buildTreePanel
  Gtk.containerAdd treeFrame treeBox
  -- creates the inspector panel and add to the window
  (inspBox, entryName, colorBtn, lineColorBtn, radioShapes, radioStyles, propBoxes) <- buildTypeInspector
  Gtk.containerAdd inspectorFrame inspBox
  let
    inspWidgets = (entryName, colorBtn, lineColorBtn, radioShapes, radioStyles)
    [radioCircle, radioRect, radioQuad] = radioShapes
    [radioNormal, radioPointed, radioSlashed] = radioStyles
  #showAll window

  -- init an model to display in the tree panel --------------------------------
  store <- Gtk.treeStoreNew [gtypeString, gtypeInt, gtypeInt]
  fstIter <- Gtk.treeStoreAppend store Nothing
  Gtk.treeViewSetModel treeview (Just store)
  storeSetGraphStore store fstIter ("new", 0, 0)

  projectCol <- Gtk.treeViewGetColumn treeview 0

  case projectCol of
    Nothing -> return ()
    Just col -> do
      #addAttribute col treeRenderer "text" 0
      Gtk.treeViewColumnSetCellDataFunc col treeRenderer $ Just (\column renderer model iter -> do
        -- render the cellBackground with different colors if there is was a change of some kind
        -- and render the foreground with black to ensure that the user theme will not cause problems
        changed <- Gtk.treeModelGetValue model iter 1 >>= \gv -> (fromGValue gv :: IO Int32)
        renderer' <- castTo Gtk.CellRendererText renderer
        case (renderer', changed) of
          (Just r,1) -> do
            color <- new Gdk.RGBA [#red:=1, #green:=0.5, #blue:=0.1, #alpha:=1]
            set r [ #cellBackgroundRgba := color, #foreground := "black"]
          (Just r,2) -> do
            color <- new Gdk.RGBA [#red:=0, #green:=1, #blue:=0, #alpha:=1]
            set r [ #cellBackgroundRgba := color, #foreground := "black" ]
          (Just r,_) -> do
            -- color <- new Gdk.RGBA [#alpha:=0]
            -- set r [ #cellBackgroundRgba := color ]
            Gtk.clearCellRendererCellBackground r
            Gtk.clearCellRendererTextForeground r
          (Nothing,_) -> return ()
        )


  ------------------------------------------------------------------------------
  -- init the editor variables  ------------------------------------------------
  st              <- newIORef emptyES -- actual state: all the necessary info to draw the graph
  oldPoint        <- newIORef (0.0,0.0) -- last point where a mouse button was pressed
  squareSelection <- newIORef Nothing -- selection box : Maybe (x,y,w,h)
  undoStack       <- newIORef ([] :: [DiaGraph])
  redoStack       <- newIORef ([] :: [DiaGraph])
  movingGI        <- newIORef False -- if the user started moving some object - necessary to add a position to the undoStack
  currentShape    <- newIORef NCircle -- the shape that all new nodes must have
  currentStyle    <- newIORef ENormal -- the style that all new edges must have
  currentC        <- newIORef (1,1,1) -- the color to init new nodes
  currentLC       <- newIORef (0,0,0) -- the color to init new edges and the line and text of new nodes
  clipboard       <- newIORef DG.empty -- clipboard - DiaGraph
  fileName        <- newIORef (Nothing :: Maybe String) -- name of the opened file
  currentGraph    <- newIORef 0 -- current graph being edited
  graphStates     <- newIORef $ M.fromList [(0, (emptyES,[],[]) :: (EditorState,[DiaGraph],[DiaGraph]) )] -- map of states foreach graph in the editor
  changedProject  <- newIORef False -- set this flag as True when the graph is changed somehow
  changedGraph    <- newIORef [False] -- when modify a graph, set the flag in the 'currentGraph' to True
  lastSavedState  <- newIORef (M.empty :: M.Map Int32 DiaGraph)

  ------------------------------------------------------------------------------
  -- EVENT BINDINGS ------------------------------------------------------------
  -- event bindings for the canvas ---------------------------------------------
  -- drawing event
  on canvas #draw $ \context -> do
    es <- liftIO $ readIORef st
    sq <- liftIO $ readIORef squareSelection
    renderWithContext context $ drawGraph es sq canvas
    return False

  -- mouse button pressed on canvas
  on canvas #buttonPressEvent $ \eventButton -> do
    b <- get eventButton #button
    x <- get eventButton #x
    y <- get eventButton #y
    ms <- get eventButton #state
    click <- get eventButton #type
    es <- liftIO $ readIORef st
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
            else liftIO $ Gtk.widgetGrabFocus entryName
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
          updatePropMenu st currentC currentLC inspWidgets propBoxes
        -- right button click: create nodes and insert edges
        (3, False) -> liftIO $ do
          let g = editorGetGraph es
              gi = editorGetGI es
              dstNode = selectNodeInPosition gi (x',y')
          context <- Gtk.widgetGetPangoContext canvas
          stackUndo undoStack redoStack es
          case (Gdk.ModifierTypeControlMask `elem` ms, dstNode) of
            -- no selected node: create node
            (False, Nothing) -> do
              shape <- readIORef currentShape
              c <- readIORef currentC
              lc <- readIORef currentLC
              createNode' st (x',y') shape c lc context
              setChangeFlags window store changedProject changedGraph currentGraph True
              -- one node selected: create edges targeting this node
            (False, Just nid) -> do
              estyle <- readIORef currentStyle
              color <- readIORef currentLC
              modifyIORef st (\es -> createEdges es nid estyle color)
              setChangeFlags window store changedProject changedGraph currentGraph True
            -- ctrl pressed: middle mouse button emulation
            (True,_) -> return ()
          Gtk.widgetQueueDraw canvas
          updatePropMenu st currentC currentLC inspWidgets propBoxes
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
        setChangeFlags window store changedProject changedGraph currentGraph True
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
      (_,_,_,_) -> return ()
    return True

  -- mouse button release on canvas
  on canvas #buttonReleaseEvent $ \eventButton -> do
    b <- get eventButton #button
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
            updatePropMenu st currentC currentLC inspWidgets propBoxes
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
  -- on canvas #keyPressEvent $ \eventKey -> do
  --   k <- get eventKey #keyval >>= return . chr . fromIntegral
  --   ms <- get eventKey #state
  --   case (Gdk.ModifierTypeControlMask `elem` ms, Gdk.ModifierTypeShiftMask `elem` ms, toLower k) of
  --     -- <delete> | <Ctrl> + D : delete selection
  --     (False,False,'\65535') -> do
  --       es <- readIORef st
  --       stackUndo undoStack redoStack es
  --       modifyIORef st (\es -> deleteSelected es)
  --       setChangeFlags window store changedProject changedGraph currentGraph True
  --       Gtk.widgetQueueDraw canvas
  --     (True,False,'d') -> do
  --       es <- readIORef st
  --       stackUndo undoStack redoStack es
  --       modifyIORef st (\es -> deleteSelected es)
  --       setChangeFlags window store changedProject changedGraph currentGraph True
  --       Gtk.widgetQueueDraw canvas
  --     -- CTRL + [SHIFT] + A : [de]select all
  --     (True, True, 'a') -> do
  --       modifyIORef st $ editorSetSelected ([],[])
  --       Gtk.widgetQueueDraw canvas
  --     (True, False, 'a') -> do
  --       modifyIORef st (\es -> let g = editorGetGraph es in editorSetSelected (nodeIds g, edgeIds g) es)
  --       Gtk.widgetQueueDraw canvas
  --     -- F2 - rename selection
  --     (False,False,'\65471') -> Gtk.widgetGrabFocus entryName
  --     -- CTRL + C/V/X : copy/paste/cut
  --     (True, False, 'c') -> Gtk.menuItemActivate cpy
  --     (True, False, 'v') -> Gtk.menuItemActivate pst
  --     (True, False, 'x') -> Gtk.menuItemActivate cut
  --     _       -> return ()
  --   return True

  -- on window #keyPressEvent $ \eventKey -> do
  --   k <- get eventKey #keyval >>= return . chr . fromIntegral
  --   ms <- get eventKey #state
  --   context <- Gtk.widgetGetPangoContext canvas
  --   case (Gdk.ModifierTypeControlMask `elem` ms, Gdk.ModifierTypeShiftMask `elem` ms, toLower k) of
  --     -- CTRL + <+>/<->/<=> : zoom controls
  --     (True,_,'+') -> Gtk.menuItemActivate zin
  --     (True,_,'-') -> Gtk.menuItemActivate zut
  --     (True,_,'=') -> Gtk.menuItemActivate zdf
  --     -- CTRL + <0> : reset pan & zoom
  --     (True,_,'0') -> Gtk.menuItemActivate vdf
  --     -- CTRL + N : create a new graph in the treeView
  --     (True, False, 'n') -> Gtk.buttonClicked btnNew
  --     -- CTRL + W : remove a graph from the treeView
  --     (True, False, 'w') -> Gtk.buttonClicked btnRmv
  --     -- CTRL + SHIFT + N : create a new file
  --     (True, True, 'n') -> Gtk.menuItemActivate newm
  --     -- CTRL + SHIFT + S : save file as
  --     (True, True, 's') -> Gtk.menuItemActivate sva
  --     -- CTRL + S : save file
  --     (True, False, 's') -> Gtk.menuItemActivate svn
  --     -- CTRL + O : open file
  --     (True, False, 'o') -> Gtk.menuItemActivate opn
  --     -- CTRL + Z/R : undo/redo
  --     (True, False, 'z') -> Gtk.menuItemActivate udo
  --     (True, False, 'r') -> Gtk.menuItemActivate rdo
  --     _ -> return ()
  --   return False

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

  -- the next 2 are auxiliar functions to get the structures needed to save the project
  let getTreeStoreValues iter = do
          valN <- Gtk.treeModelGetValue store iter 0 >>= (\n -> fromGValue n :: IO (Maybe String)) >>= return . fromJust
          valI <- Gtk.treeModelGetValue store iter 2 >>= fromGValue
          continue <- Gtk.treeModelIterNext store iter
          if continue
            then do
              newVals <- getTreeStoreValues iter
              return $ (valN, valI) : newVals
            else return $ (valN, valI) : []

  let getStructsToSave = do
          (valid, fstIter) <- Gtk.treeModelGetIterFirst store
          if not valid
            then return []
            else do
              treeNodeList <- getTreeStoreValues fstIter
              states <- readIORef graphStates
              let ids = map snd treeNodeList
                  editors = map (\iD -> let (es,_,_) = fromJust $ M.lookup iD states in es) ids
                  structs = zip (map fst treeNodeList) editors
              return structs

  -- auxiliar function to check if the project was changed
  -- it does the checking and if no, ask the user if them want to save.
  -- returns True if there's no changes, if the user don't wanted to save or if he wanted and the save operation was successfull
  -- returns False if the user wanted to save and the save operation failed or opted to cancel.
  let confirmOperation = do changed <- readIORef changedProject
                            response <- if changed
                              then createConfirmDialog window "The project was changed, want to save?"
                              else return Gtk.ResponseTypeNo
                            case response of
                              Gtk.ResponseTypeCancel -> return False
                              r -> case r of
                                Gtk.ResponseTypeNo -> return True
                                Gtk.ResponseTypeYes -> do
                                  prepToSave
                                  structs <- getStructsToSave
                                  saveFile structs saveProject fileName window True -- returns True if saved the file

  -- new project action activated
  on newm #activate $ do
    continue <- confirmOperation
    if continue
      then do
        writeIORef fileName Nothing
        Gtk.treeStoreClear store
        iter <- Gtk.treeStoreAppend store Nothing
        storeSetGraphStore store iter ("new", 0, 0)
        writeIORef st emptyES
        writeIORef undoStack []
        writeIORef redoStack []
        writeIORef graphStates (M.fromList [(0,(emptyES,[],[]))])
        writeIORef lastSavedState M.empty
        writeIORef changedProject False
        writeIORef changedGraph [False]
        set window [#title := "Graph Editor"]
        Gtk.widgetQueueDraw canvas
      else return ()

  -- open project
  on opn #activate $ do
    continue <- confirmOperation
    if continue
      then do
        mg <- loadFile window loadProject
        case mg of
          Nothing -> return ()
          Just (list,fn) -> do
            if length list > 0
              then do
                Gtk.treeStoreClear store
                let nameList = map (\(n,i) -> (n,0,i)) $ zip (map fst list) [0..]
                    statesList = map (\(es,i) -> (i, (es,[],[]))) $ zip (map snd list) [0..]
                forM nameList $ \element -> do
                  iter <- Gtk.treeStoreAppend store Nothing
                  storeSetGraphStore store iter element
                let (_,es) = list!!0
                writeIORef st es
                writeIORef undoStack []
                writeIORef redoStack []
                writeIORef fileName $ Just fn
                writeIORef currentGraph 0
                writeIORef graphStates $ M.fromList statesList
                writeIORef changedProject False
                writeIORef changedGraph [False]
                set window [#title := T.pack ("Graph Editor - " ++ fn)]
                Gtk.widgetQueueDraw canvas
              else return ()
      else return ()

  -- save project
  on svn #activate $ do
    prepToSave
    structs <- getStructsToSave
    saved <- saveFile structs saveProject fileName window True
    if saved
      then do afterSave
      else return ()

  -- save project as
  sva `on` #activate $ do
    prepToSave
    structs <- getStructsToSave
    saved <- saveFileAs structs saveProject fileName window True
    if saved
      then afterSave
      else return ()

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
        iter <- Gtk.treeStoreAppend store Nothing
        storeSetGraphStore store iter (getName . getLastPart $ path, 2, newKey)
        path <- Gtk.treeModelGetPath store iter
        Gtk.treeViewSetCursor treeview path (Nothing :: Maybe Gtk.TreeViewColumn) False
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

  -- delete item
  on del #activate $ do
    es <- readIORef st
    stackUndo undoStack redoStack es
    modifyIORef st (\es -> deleteSelected es)
    setChangeFlags window store changedProject changedGraph currentGraph True
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
    setChangeFlags window store changedProject changedGraph currentGraph $ not (isDiaGraphEqual (g,gi) x)
    Gtk.widgetQueueDraw canvas

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
    setChangeFlags window store changedProject changedGraph currentGraph $ not (isDiaGraphEqual (g,gi) x)
    Gtk.widgetQueueDraw canvas

  -- copy
  on cpy #activate $ do
    es <- readIORef st
    let copy = copySelected es
    writeIORef clipboard $ copy
    print copy

  -- paste
  on pst #activate $ do
    es <- readIORef st
    clip <- readIORef clipboard
    stackUndo undoStack redoStack es
    setChangeFlags window store changedProject changedGraph currentGraph True
    modifyIORef st (pasteClipBoard clip)
    Gtk.widgetQueueDraw canvas

  -- cut
  on cut #activate $ do
    es <- readIORef st
    writeIORef clipboard $ copySelected es
    modifyIORef st (\es -> deleteSelected es)
    stackUndo undoStack redoStack es
    setChangeFlags window store changedProject changedGraph currentGraph True
    Gtk.widgetQueueDraw canvas

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
  -- pressed a key when editing the entryName
  on entryName #keyPressEvent $ \eventKey -> do
    k <- get eventKey #keyval >>= return . chr . fromIntegral
    let setName = do es <- readIORef st
                     stackUndo undoStack redoStack es
                     setChangeFlags window store changedProject changedGraph currentGraph True
                     name <- Gtk.entryGetText entryName >>= return . T.unpack
                     context <- Gtk.widgetGetPangoContext canvas
                     renameSelected st name context
                     Gtk.widgetQueueDraw canvas
  -- if it's Return or Enter (Numpad), then change the name of the selected elements
    case k of
       '\65293' -> setName
       '\65421' -> setName
       _       -> return ()
    return False

  -- select a fill color or line color
  -- change the selection fill color or line color and
  -- set the current fill or line color as the selected color
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
        setChangeFlags window store changedProject changedGraph currentGraph True
        modifyIORef st (\es -> editorSetGI (newngiM, egiM) es)
        Gtk.widgetQueueDraw canvas

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
        setChangeFlags window store changedProject changedGraph currentGraph True
        modifyIORef st (\es -> editorSetGI (newngiM, newegiM) es)
        Gtk.widgetQueueDraw canvas

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
        setChangeFlags window store changedProject changedGraph currentGraph True
        modifyIORef st (\es -> changeNodeShape es NCircle)
        Gtk.widgetQueueDraw canvas

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
        setChangeFlags window store changedProject changedGraph currentGraph True
        modifyIORef st (\es -> changeNodeShape es NRect)
        Gtk.widgetQueueDraw canvas

  radioQuad `on` #toggled $ do
    writeIORef currentShape NSquare
    es <- readIORef st
    active <- get radioQuad #active
    let nds = fst $ editorGetSelected es
        giM = fst $ editorGetGI es
    if not active || M.null (M.filterWithKey (\k gi -> NodeId k `elem` nds && shape gi /= NSquare) giM)
      then return ()
      else do
        stackUndo undoStack redoStack es
        setChangeFlags window store changedProject changedGraph currentGraph True
        modifyIORef st (\es -> changeNodeShape es NSquare)
        Gtk.widgetQueueDraw canvas

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
        setChangeFlags window store changedProject changedGraph currentGraph True
        modifyIORef st (\es -> changeEdgeStyle es ENormal)
        Gtk.widgetQueueDraw canvas

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
        setChangeFlags window store changedProject changedGraph currentGraph True
        modifyIORef st (\es -> changeEdgeStyle es EPointed)
        Gtk.widgetQueueDraw canvas

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
        setChangeFlags window store changedProject changedGraph currentGraph True
        modifyIORef st (\es -> changeEdgeStyle es ESlashed)
        Gtk.widgetQueueDraw canvas

  -- event bindings for the graphs' tree ---------------------------------------

  -- auxiliar
  let loadFromStore index = do
                      states <- readIORef graphStates
                      let maybeState = M.lookup index states
                      case maybeState of
                        Just (es,u,r) -> do
                          writeIORef st es
                          writeIORef undoStack u
                          writeIORef redoStack r
                          writeIORef currentGraph index
                        Nothing -> return ()


  -- changed the selected graph
  on treeview #cursorChanged $ do
    selection <- Gtk.treeViewGetSelection treeview
    (sel,model,iter) <- Gtk.treeSelectionGetSelected selection
    case sel of
      False -> return ()
      True -> do
        cIndex <- readIORef currentGraph
        val <- Gtk.treeModelGetValue model iter 2
        index <- fromGValue val :: IO Int32
        if cIndex == index
          then return ()
          else do
            -- update the current graph in the tree
            currentES <- readIORef st
            u <- readIORef undoStack
            r <- readIORef redoStack
            modifyIORef graphStates (M.insert cIndex (currentES,u,r))
            -- load the selected graph from the tree
            loadFromStore index
            Gtk.widgetQueueDraw canvas

  -- pressed the 'new' button on the treeview area
  on btnNew #clicked $ do
    states <- readIORef graphStates
    let newKey = maximum (M.keys states) + 1
    iter <- Gtk.treeStoreAppend store Nothing
    storeSetGraphStore store iter ("new",2, newKey)
    modifyIORef graphStates (M.insert newKey (emptyES,[],[]))

  -- pressed the 'remove' button on the treeview area
  on btnRmv #clicked $ do
    selection <- Gtk.treeViewGetSelection treeview
    (sel,model,iter) <- Gtk.treeSelectionGetSelected selection
    case sel of
      False -> return ()
      True -> do
        index <- Gtk.treeModelGetValue store iter 2 >>= fromGValue
        Gtk.treeStoreRemove store iter
        modifyIORef graphStates $ M.delete index

  -- edited a graph name
  on treeRenderer #edited $ \pathStr newName -> do
    path <- Gtk.treePathNewFromString pathStr
    (v,iter) <- Gtk.treeModelGetIter store path
    if v
      then do
        gval <- toGValue (Just newName)
        Gtk.treeStoreSet store iter [0] [gval]
        writeIORef changedProject True
        indicateProjChanged window True
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


--------------------------------------------------------------------------------
-- Callbacks -------------------------------------------------------------------
--------------------------------------------------------------------------------

-- update the inspector --------------------------------------------------------
updatePropMenu :: IORef EditorState -> IORef (Double,Double,Double) -> IORef (Double,Double,Double) -> (Gtk.Entry, Gtk.ColorButton, Gtk.ColorButton, [Gtk.RadioButton], [Gtk.RadioButton]) -> (Gtk.Box, Gtk.Frame, Gtk.Frame)-> IO ()
updatePropMenu st currentC currentLC (entryName, colorBtn, lcolorBtn, radioShapes, radioStyles) (hBoxColor, frameShape, frameStyle) = do
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
      set entryName [#text := ""]
      color <- new Gdk.RGBA [#red := r, #green := g, #blue := b, #alpha:=1.0]
      lcolor <- new Gdk.RGBA [#red := r', #green := g', #blue := b', #alpha:=1.0]
      Gtk.colorChooserSetRgba colorBtn color
      Gtk.colorChooserSetRgba lcolorBtn lcolor
      set hBoxColor [#visible := True]
      set frameShape [#visible := True]
      set frameStyle [#visible := True]
    (n,0) -> do
      let nid = nodeId (ns!!0)
          name = T.pack $ if n == 1 then nodeInfo $ (ns!!0) else unifyNames (map nodeInfo ns)
          gi = getNodeGI (fromEnum nid) ngiM
          (r,g,b) = fillColor gi
          (r',g',b') = lineColor gi
          nodeShape = shape gi
      color <- new Gdk.RGBA [#red := r, #green := g, #blue := b, #alpha := 1.0]
      lcolor <- new Gdk.RGBA [#red := r', #green := g', #blue := b', #alpha := 1.0]
      set entryName [#text := name]
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
          name = T.pack $ if n == 1 then edgeInfo (es!!0) else unifyNames (map edgeInfo es)
          gi = getEdgeGI (fromEnum eid) egiM
          (r,g,b) = color gi
          edgeStyle = style gi
      edgeColor <- new Gdk.RGBA [#red := r, #green := g, #blue := b, #alpha := 1.0]
      set entryName [#text := name]
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
      set entryName [#text := "----" ]
      Gtk.colorChooserSetRgba colorBtn emptyColor
      Gtk.colorChooserSetRgba lcolorBtn emptyColor
      set hBoxColor [#visible := True]
      set frameShape [#visible := True]
      set frameStyle [#visible := True]

-- draw a graph in the canvas --------------------------------------------------
drawGraph :: EditorState -> Maybe (Double,Double,Double,Double) -> Gtk.DrawingArea -> Render ()
drawGraph (g, (nGI,eGI), (sNodes, sEdges), z, (px,py)) sq canvas = do
  scale z z
  translate px py

  -- draw the edges
  forM (edges g) (\e -> do
    let dstN = M.lookup (fromEnum . targetId $ e) nGI
        srcN = M.lookup (fromEnum . sourceId $ e) nGI
        egi  = M.lookup (fromEnum . edgeId   $ e) eGI
        selected = (edgeId e) `elem` sEdges
    case (egi, srcN, dstN) of
      (Just gi, Just src, Just dst) -> renderEdge gi (edgeInfo e) selected src dst
      _ -> return ())

  -- draw the nodes
  forM (nodes g) (\n -> do
    let ngi = M.lookup (fromEnum . nodeId $ n) nGI
        selected = (nodeId n) `elem` (sNodes)
        info = nodeInfo n
    case (ngi) of
      Just gi -> renderNode gi info selected
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

-- general save function -------------------------------------------------------
saveFile :: a -> (a -> String -> IO Bool) -> IORef (Maybe String) -> Gtk.Window -> Bool -> IO Bool
saveFile x saveF fileName window changeFN = do
  fn <- readIORef fileName
  case fn of
    Just path -> do
      tentativa <- saveF x path
      case tentativa of
        True -> return True
        False -> do
          showError window $ T.pack ("Couldn't write to file." ++ path)
          return False
    Nothing -> saveFileAs x saveF fileName window changeFN


saveFileAs :: a -> (a -> String -> IO Bool) -> IORef (Maybe String) -> Gtk.Window -> Bool -> IO Bool
saveFileAs x saveF fileName window changeFN = do
  saveD <- createSaveDialog window
  response <- Gtk.dialogRun saveD
  fn <- case toEnum . fromIntegral $  response of
    Gtk.ResponseTypeAccept -> do
      filename <- Gtk.fileChooserGetFilename saveD
      case filename of
        Nothing -> do
          Gtk.widgetDestroy saveD
          return Nothing
        Just path -> do
          tentativa <- saveF x path
          case tentativa of
            True -> do
              Gtk.widgetDestroy saveD
              return $ Just path
            False -> do
              Gtk.widgetDestroy saveD
              showError window $ T.pack ("Couldn't write to file." ++ path)
              return Nothing
    _  -> do
      Gtk.widgetDestroy saveD
      return Nothing
  case (changeFN, fn) of
    (True, Just path) -> do
      writeIORef fileName (Just path)
      return True
    _ -> return False

-- auxiliar save functions -----------------------------------------------------
-- save project
saveProject :: [(String,EditorState)] -> String -> IO Bool
saveProject esList path = do
  let getWhatMatters = (\(name, es) -> (name, editorGetGraph es, editorGetGI es))
      whatMatters = map getWhatMatters esList
      contents = map (\(name, g, gi) -> ( name
                                        , map (\n -> (nodeId n, nodeInfo n) ) $ nodes g
                                        , map (\e -> (edgeId e, sourceId e, targetId e, edgeInfo e)) $ edges g
                                        , gi )) whatMatters
      writeProject = writeFile path $ show contents
  saveTry <- E.try (writeProject)  :: IO (Either E.IOException ())
  case saveTry of
    Left _ -> return False
    Right _ -> return True
--
-- save graph
saveGraph :: (Graph String String ,GraphicalInfo) -> String -> IO Bool
saveGraph (g,gi) path = do
    let path' = if (tails path)!!(length path-3) == ".gr" then path else path ++ ".gr"
        writeGraph = writeFile path' $ show ( map (\n -> (nodeId n, nodeInfo n) ) $ nodes g
                                           , map (\e -> (edgeId e, sourceId e, targetId e, edgeInfo e)) $ edges g
                                           , gi)

    tentativa <- E.try (writeGraph)  :: IO (Either E.IOException ())
    case tentativa of
      Left _ -> return False
      Right _ -> return True
--
--
-- load function ---------------------------------------------------------------
loadFile :: Gtk.Window -> (String -> Maybe a) -> IO (Maybe (a,String))
loadFile window loadF = do
  loadD <- createLoadDialog window
  response <- Gtk.dialogRun loadD
  case toEnum . fromIntegral $ response of
    Gtk.ResponseTypeAccept -> do
      filename <- Gtk.fileChooserGetFilename loadD
      Gtk.widgetDestroy loadD
      case filename of
        Nothing -> do
          return Nothing
        Just path -> do
          tentativa <- E.try (readFile path) :: IO (Either E.IOException String)
          case tentativa of
            Left _ -> do
              showError window "Couldn't open the file"
              return Nothing
            Right content -> case loadF content of
              Nothing -> do
                showError window "Couldn't read the file"
                return Nothing
              Just x -> return $ Just (x, path)
    _             -> do
      Gtk.widgetDestroy loadD
      return Nothing

-- auxiliar load functions -----------------------------------------------------
-- load project
type NList = [(Int,String)]
type EList = [(Int,Int,Int,String)]
loadProject :: String -> Maybe [(String, EditorState)]
loadProject content = editorList
  where
    genNodes = map (\(nid, info) -> Node (NodeId nid) info)
    genEdges = map (\(eid, src, dst, info) -> Edge (EdgeId eid) (NodeId src) (NodeId dst) info)
    genProj = map (\(name,readNodes,readEdges,gi) ->
                      let nds = genNodes readNodes
                          eds = genEdges readEdges
                          g = fromNodesAndEdges nds eds
                      in (name, editorSetGI gi . editorSetGraph g $ emptyES) )
    editorList = case reads content :: [([(String, NList, EList, GraphicalInfo)], String)] of
      [(contentList,"")] -> Just $ genProj contentList
      _ -> Nothing
-- -- load graph
loadGraph :: String -> Maybe (Graph String String,GraphicalInfo)
loadGraph contents = result
  where
    result = case reads contents :: [( (NList, EList, GraphicalInfo), String)] of
      [((rns,res,gi), "")] -> let ns = map (\(nid, info) -> Node (NodeId nid) info) rns
                                  es = map (\(eid, src, dst, info) -> Edge (EdgeId eid) (NodeId src) (NodeId dst) info) res
                                  g = fromNodesAndEdges ns es
                              in Just (g,gi)
      _ -> Nothing


-- graph interaction
-- create a new node, auto-generating it's name and dimensions
createNode' :: IORef EditorState -> GIPos -> NodeShape -> GIColor -> GIColor -> P.Context ->  IO ()
createNode' st pos nshape color lcolor context = do
  es <- readIORef st
  let nid = head $ newNodes (editorGetGraph es)
      --content = "node " ++ show nid
      content = ""
  dim <- getStringDims content context
  writeIORef st $ createNode es pos dim content nshape color lcolor

-- rename the selected itens
renameSelected:: IORef EditorState -> String -> P.Context -> IO()
renameSelected state name context = do
  es <- readIORef state
  dim <- getStringDims name context
  let graph = editorGetGraph es
      (nids,eids) = editorGetSelected es
      (ngiM,egiM) = editorGetGI es
  let graph' = foldl (\g nid -> updateNodePayload nid g (\_ -> name)) graph nids
      newGraph  = foldl (\g eid -> updateEdgePayload eid g (\_ -> name)) graph' eids
      newNgiM = M.mapWithKey (\k gi -> if NodeId k `elem` nids then nodeGiSetDims dim gi else gi) ngiM
      newEs   = editorSetGI (newNgiM,egiM) . editorSetGraph newGraph $ es
  writeIORef state newEs

-- auxiliar function used by createNode' and renameSelected
-- given a text, compute the size of it's bounding box
-- uses the pango lib
getStringDims :: String -> P.Context -> IO (Double, Double)
getStringDims str context = do
  desc <- P.fontDescriptionFromString "Sans Regular 10"
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
  gvchanged <- toGValue (1::Int32)
  Gtk.treeStoreSetValue store iter 1 gvchanged

indicateGraphChanged store iter False = do
  gvchanged <- toGValue (0::Int32)
  Gtk.treeStoreSetValue store iter 1 gvchanged

-- change the flags that inform if the graphs and project were changed and indicate the changes
setChangeFlags :: Gtk.Window -> Gtk.TreeStore -> IORef Bool -> IORef [Bool] -> IORef Int32 -> Bool -> IO ()
setChangeFlags window store changedProject changedGraph currentGraph changed = do
  index <- readIORef currentGraph >>= return . fromIntegral
  xs <- readIORef changedGraph
  let xs' = take index xs ++ [changed] ++ drop (index+1) xs
      projChanged = or xs'
  writeIORef changedGraph xs'
  writeIORef changedProject $ projChanged
  indicateProjChanged window $ projChanged
  (valid,iter) <- Gtk.treeModelGetIterFromString store (T.pack . show $ index)
  if valid
    then indicateGraphChanged store iter changed
    else return ()


-- change updatedState
updateSavedState :: IORef (M.Map Int32 DiaGraph) -> IORef (M.Map Int32 (EditorState, [DiaGraph], [DiaGraph])) -> IO ()
updateSavedState sst graphStates = do
  states <- readIORef graphStates
  writeIORef sst $ (M.map (\(es,_,_) -> (editorGetGraph es, editorGetGI es) ) states)
