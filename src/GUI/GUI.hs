{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module GUI.GUI(
  startGUI
)where

-- Gtk modules
import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import           Data.GI.Base
import           Data.GI.Base.GValue
import           Data.GI.Base.GType
import           Data.GI.Base.ManagedPtr (unsafeCastTo)    
import           Graphics.Rendering.Cairo
import           Graphics.Rendering.Pango.Layout
import           Graphics.Rendering.Pango

-- haskell data modules
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Zip
import           Data.IORef
import           Data.List
import           Data.Int
import           Data.Char
import           Data.Maybe
import           Data.Either
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Tree as Tree
import           Data.Monoid

-- verigraph modules
import           Abstract.Category
import           Abstract.Rewriting.DPO
import           Data.Graphs hiding (null, empty)
import qualified Data.Graphs as G
import qualified Data.TypedGraph.Morphism as TGM

-- Verigraph-GUI modules
import           GUI.Data.GraphicalInfo
import           GUI.Data.Info hiding (empty)
import qualified GUI.Data.Info as I
import           GUI.Data.DiaGraph hiding (empty)
import           GUI.Dialogs
import           GUI.Data.EditorState
import qualified GUI.Data.DiaGraph as DG
import           GUI.Data.Nac
import           GUI.Editor as Edit
import qualified GUI.Editor.Helper.TreeStore as Edit
import           GUI.Helper.GrammarMaker
import           GUI.Helper.List
import           GUI.Helper.Geometry
import           GUI.Helper.GraphValidation
import           GUI.Render.Render

-- modules for executor
import qualified GUI.Executor as Exec

-- modules needed for analysis
import GUI.Analysis.CriticalPairAnalysis

import GUI.HelpWindow


-- | creates the Graphical User Interface and bind actions to events
startGUI :: IO()
startGUI = do
  -----------------------------------------------------------------------------------------------------------------------------
  --------  IORefs  -----------------------------------------------------------------------------------------------------------
  -----------------------------------------------------------------------------------------------------------------------------

  -- variables to editStore the multiple graphs of the grammar
  -- map of graph ids to graphs, containing the EditorStates of all graphs in the grammar
  statesMap       <- newIORef (M.empty :: M.Map Int32 EditorState)
  currentPath     <- newIORef [0] -- indexes of the path of the current graph in the TreeStore
  currentGraph    <- newIORef (0 :: Int32) -- index of the current graph
  {- number specifying the type of the current graph 
     (see possible values in the module GUI.Helper.TreeStore, lines 52 - 61) -}
  currentGraphType <- newIORef (1 :: Int32)
  writeIORef statesMap $ M.fromList [(a,emptyES) | a <- [0 .. 2]]
  let storeIORefs = (statesMap,currentPath,currentGraph,currentGraphType)
  
  -- variables to keep track of changes
  changedProject  <- newIORef False -- set this flag as True when the graph is changed somehow
  changedGraph    <- newIORef [False] -- when modify a graph, set the flag in the 'currentGraph' to True
  lastSavedState  <- newIORef (M.empty :: M.Map Int32 DiaGraph)
  let changesIORefs = (changedProject, changedGraph, lastSavedState)

  -- file name of the new editor project
  fileName        <- newIORef (Nothing :: Maybe String) -- name of the opened file

  -- the type graph being used as base for the typed graphs
  typeGraph <- newIORef G.empty

  -- variables to specify NACs
  -- Diagraph from the rule - togetter with lhs it make the editor state
  nacInfoMap    <- newIORef (M.empty :: M.Map Int32 (DiaGraph, MergeMapping))
  mergeMapping  <- newIORef (Nothing :: Maybe MergeMapping) -- current merge mapping. important to undo/redo with nacs
  let nacIORefs = (nacInfoMap, mergeMapping)  




  -----------------------------------------------------------------------------------------------------------------------------
  --------  GUI DEFINITION  ---------------------------------------------------------------------------------------------------
  -----------------------------------------------------------------------------------------------------------------------------

  -- init GTK
  Gtk.init Nothing

  -- build main window
  (window, tabs, fileItems, editItems, viewItems, helpItems) <- buildMainWindow
  #showAll window

  -- build auxiliar windows
  helpWindow <- buildHelpWindow

  -- set the menubar
  let [newm,opn,svn,sva,eggx,evgg] = fileItems
      [del,udo,rdo,cpy,pst,cut,sla,sln,sle,mrg,spt] = editItems
      [zin,zut,z50,zdf,z150,z200,vdf] = viewItems
      [hlp,abt] = helpItems

  -- init an model to display in the editor tree panel --------------------------------
  editStore <- Gtk.treeStoreNew [gtypeString, gtypeInt, gtypeInt, gtypeInt, gtypeBoolean, gtypeBoolean]
  Edit.initStore editStore
  -- start editor module
  (editorPane,currentES) <- Edit.startEditor window editStore
                                                fileName typeGraph
                                                storeIORefs changesIORefs nacIORefs
                                                fileItems editItems viewItems

  -- start executor module
  execStore <- Gtk.treeStoreNew [gtypeString, gtypeInt, gtypeInt, gtypeInt]
  Exec.updateTreeStore execStore ("Rule0", 2, 3, 0)
  (execPane, execState, execStarted) <- Exec.buildExecutor execStore statesMap typeGraph nacInfoMap

  -- start analysis module
  cpaBox <- buildCpaBox window editStore statesMap nacInfoMap

  -- set the tabs
  editorTabLabel <- new Gtk.Label [#label := "Editor"]
  Gtk.notebookAppendPage tabs editorPane (Just editorTabLabel)
  
  execTabLabel <- new Gtk.Label [#label := "Execute"]
  Gtk.notebookAppendPage tabs execPane (Just execTabLabel)

  analysisTabLabel <- new Gtk.Label [#label := "Analysis"]
  Gtk.notebookAppendPage tabs cpaBox (Just analysisTabLabel)    
                    


  ----------------------------------------------------------------------------------------------------------------------------
  --------  EVENT BINDINGS  --------------------------------------------------------------------------------------------------
  ----------------------------------------------------------------------------------------------------------------------------
  
  -- update execStore based on editStore
  after editStore #rowChanged $ \path iter -> do
    n  <- Gtk.treeModelGetValue editStore iter 0 >>= (\n -> fromGValue n :: IO (Maybe String)) >>= return . fromJust
    id <- Gtk.treeModelGetValue editStore iter 2 >>= fromGValue :: IO Int32
    t  <- Gtk.treeModelGetValue editStore iter 3 >>= fromGValue :: IO Int32
    a  <- Gtk.treeModelGetValue editStore iter 4 >>= fromGValue :: IO Bool
    v  <- Gtk.treeModelGetValue editStore iter 5 >>= fromGValue :: IO Bool
    case (v,t) of
      (True,3) -> Exec.updateTreeStore execStore (n,id,t,0)
      (True,4) -> do
        (valid,parent) <- Gtk.treeModelIterParent editStore iter
        if valid
          then do
            p <- Gtk.treeModelGetValue editStore parent 2 >>= fromGValue :: IO Int32
            Exec.updateTreeStore execStore (n,id,t,p)
          else return ()
      (False,3) -> Exec.removeFromTreeStore execStore id
      (False,4) -> Exec.removeFromTreeStore execStore id
      _ -> return ()

  -- when a rule or nac is deleted from editStore, remove the correspondent from execStore
  after editStore #rowDeleted $ \path -> do
    statesM <- readIORef statesMap
    Exec.removeTrashFromTreeStore execStore statesM

  on tabs #switchPage $ \page pageNum -> do
    -- update statesMap with the information of currentES
    Edit.storeCurrentES window currentES storeIORefs nacInfoMap
    case pageNum of
      1 -> do
          started <- readIORef execStarted
          if started
            then return ()
            else do
              initState <- readIORef statesMap >>= return . fromMaybe emptyES . M.lookup 1
              writeIORef execState initState
      _ -> return ()      

  -- Help Menu 
  -- help
  on hlp #activate $ do
    #showAll helpWindow

  -- about
  on abt #activate $ buildAboutDialog

  -- event bindings for the main window --------------------------------------------------------------------------------------
  -- when click in the close button, the application must close
  on window #deleteEvent $ return $ do
    continue <- Edit.confirmOperation window editStore changedProject currentES nacInfoMap fileName storeIORefs
    if continue
      then do
        Gtk.mainQuit
        return False
      else return True

  -- start the Gtk main loop -------------------------------------------------------------------------------------------------
  Gtk.main








------------------------------------------------------------------------------------------------------------------------------
-- GUI Definition ------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------

buildMainWindow :: IO ( Gtk.Window, Gtk.Notebook, [Gtk.MenuItem], [Gtk.MenuItem], [Gtk.MenuItem], [Gtk.MenuItem])
buildMainWindow = do
  builder <- new Gtk.Builder []
  Gtk.builderAddFromFile builder "./Resources/window.glade"
  window  <- Gtk.builderGetObject builder "window" >>= unsafeCastTo Gtk.Window . fromJust
  tabs    <- Gtk.builderGetObject builder "tabs" >>= unsafeCastTo Gtk.Notebook . fromJust
  
  -- menubar
  menubar  <- Gtk.builderGetObject builder "menubar" >>= unsafeCastTo Gtk.MenuBar . fromJust

  newItem <- Gtk.builderGetObject builder "new_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  openItem <- Gtk.builderGetObject builder "open_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  saveItem <- Gtk.builderGetObject builder "save_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  saveAsItem <- Gtk.builderGetObject builder "save_as_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  exportGGXItem <- Gtk.builderGetObject builder "export_ggx_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  exportVGGItem <- Gtk.builderGetObject builder "export_vgg_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  let fileItems = [newItem,openItem,saveItem,saveAsItem,exportGGXItem,exportVGGItem]

  delItem <- Gtk.builderGetObject builder  "delete_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  undoItem <- Gtk.builderGetObject builder  "undo_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  redoItem <- Gtk.builderGetObject builder  "redo_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  copyItem <- Gtk.builderGetObject builder  "copy_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  pasteItem <- Gtk.builderGetObject builder  "paste_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  cutItem <- Gtk.builderGetObject builder  "cut_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  sallItem <- Gtk.builderGetObject builder  "sall_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  snodesItem <- Gtk.builderGetObject builder  "snodes_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  sedgesItem <- Gtk.builderGetObject builder  "sedges_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  mergeItem <- Gtk.builderGetObject builder  "merge_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  splitItem <- Gtk.builderGetObject builder  "split_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  let editItems = [delItem, undoItem,redoItem,copyItem,pasteItem,cutItem,sallItem,snodesItem,sedgesItem,mergeItem,splitItem]

  zoomInItem <- Gtk.builderGetObject builder  "zoomin_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  zoomOutItem <- Gtk.builderGetObject builder  "zoomout_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  zoom50Item <- Gtk.builderGetObject builder  "zoom50_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  zoom100Item <- Gtk.builderGetObject builder  "zoom100_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  zoom150Item <- Gtk.builderGetObject builder  "zoom150_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  zoom200Item <- Gtk.builderGetObject builder  "zoom200_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  resetViewItem <- Gtk.builderGetObject builder  "resetview_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  let viewItems = [zoomInItem,zoomOutItem,zoom50Item,zoom100Item,zoom150Item,zoom200Item,resetViewItem]

  helpItem <- Gtk.builderGetObject builder  "help_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  aboutItem <- Gtk.builderGetObject builder  "about_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  let helpItems = [helpItem, aboutItem]

  return ( window, tabs, fileItems, editItems, viewItems, helpItems)