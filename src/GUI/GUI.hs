{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module GUI.GUI(
  startGUI
)where

-- Gtk modules
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

-- editor modules
import GUI.Data.GraphicalInfo
import GUI.Data.Info hiding (empty)
import qualified GUI.Data.Info as I
import GUI.Data.DiaGraph hiding (empty)
import GUI.Data.EditorState
import qualified GUI.Data.DiaGraph as DG
import GUI.Render.Render
import GUI.Editor
import GUI.Editor.Helper.Clipboard
import GUI.Editor.Helper.GrammarMaker
import GUI.Editor.Helper.GraphicalInfo
import GUI.Editor.Helper.Nac
import GUI.Editor.Helper.SaveLoad
import GUI.Editor.Helper.TreeStore
import GUI.Editor.Helper.TypeInfer
import GUI.Editor.Helper.UndoRedo
import GUI.Editor.Render.GraphDraw
import GUI.Editor.UI.UIBuilders
import GUI.Editor.UI.RuleViewer
import GUI.Editor.UI.UpdateInspector
import GUI.Data.Nac
import GUI.Helper.List
import GUI.Helper.Geometry
import GUI.Helper.GraphValidation

-- modules needed for analysis
import qualified Exec.GlobalOptions        as EGO
import qualified Exec.CriticalPairAnalysis as CPA
import Category.TypedGraphRule (RuleMorphism)
import Rewriting.DPO.TypedGraph (emptyGraphRule)
import GUI.Analysis.CriticalPairAnalysis



-- | creates the Graphical User Interface and bind actions to events
startGUI :: IO()
startGUI = do
  -----------------------------------------------------------------------------------------------------------------------------
  --------  IORefs  -----------------------------------------------------------------------------------------------------------
  -----------------------------------------------------------------------------------------------------------------------------

  -- variables used for edition/visualization
  st              <- newIORef emptyES -- current state: the necessary info to draw the graph
  oldPoint        <- newIORef (0.0,0.0) -- last point where a mouse button was pressed
  squareSelection <- newIORef Nothing -- selection box : Maybe (x1,y1,x2,y2)
  movingGI        <- newIORef False -- if the user started moving some object - necessary to add a position to the undoStack
  let basicEditIORefs = (st, oldPoint, squareSelection, movingGI)

  
  -- variables to store the multiple graphs of the grammar
  -- map of states foreach graph of the grammar, containing undo and redo stacks foreach graph
  graphStates     <- newIORef (M.empty :: M.Map Int32 (EditorState, ChangeStack, ChangeStack) )
  currentPath     <- newIORef [0] -- indexes of the path of the current graph in the TreeStore
  currentGraph    <- newIORef (0 :: Int32) -- index of the current graph
  {- number specifying the type of the current graph 
     (see possible values in the module GUI.Editor.Helper.TreeStore, lines 52 - 61) 
  -}
  currentGraphType <- newIORef (1 :: Int32)
  writeIORef graphStates $ M.fromList [(0, (emptyES,[],[])), (1, (emptyES, [], [])), (2, (emptyES, [], []))]
  let storeIORefs = (graphStates,currentPath,currentGraph,currentGraphType)

  -- variables for undo/redo
  undoStack       <- newIORef ([] :: ChangeStack )
  redoStack       <- newIORef ([] :: ChangeStack )
  let undoRedoIORefs = (undoStack, redoStack)
  
  -- variables to keep track of changes
  changedProject  <- newIORef False -- set this flag as True when the graph is changed somehow
  changedGraph    <- newIORef [False] -- when modify a graph, set the flag in the 'currentGraph' to True
  lastSavedState  <- newIORef (M.empty :: M.Map Int32 DiaGraph)
  let changesIORefs = (changedProject, changedGraph, lastSavedState)

  -- file name of the new editor project
  fileName        <- newIORef (Nothing :: Maybe String) -- name of the opened file

  -- the type graph being used as base for the typed graphs
  activeTypeGraph     <- newIORef G.empty  

  

  -- variables to specify NACs
  -- Diagraph from the rule - togetter with lhs it make the editor state
  nacInfoMapIORef <- newIORef (M.empty :: M.Map Int32 (DiaGraph, MergeMapping))
  mergeMappingIORef <- newIORef (Nothing :: Maybe MergeMapping) -- current merge mapping. important to undo/redo with nacs
  let nacIORefs = (nacInfoMapIORef, mergeMappingIORef)  

  -----------------------------------------------------------------------------------------------------------------------------
  --------  GUI DEFINITION  ---------------------------------------------------------------------------------------------------
  -----------------------------------------------------------------------------------------------------------------------------

  -- init GTK
  Gtk.init Nothing

  -- build main window
  (window, mainBox, fileItems, editItems, viewItems, analysisItems, helpItems) <- buildMainWindow

  -- build auxiliar windows
  helpWindow <- buildHelpWindow
  (rvWindow, rvNameLabel, rvlCanvas, rvrCanvas, rvlesIOR, rvresIOR, rvtgIOR, rvkIOR) <- createRuleViewerWindow
  (cpaWindow, cpaEssentialCheckBtn, cpaConfCheckBtn, cpaDependCheckBtn, cpaRunBtn, cpaResultBuffer) <- buildCpaWindow window

  -- set the menubar
  let [newm,opn,svn,sva,eggx,evgg,svg,opg] = fileItems
      [del,udo,rdo,cpy,pst,cut,sla,sln,sle,mrg,spt] = editItems
      [zin,zut,z50,zdf,z150,z200,vdf] = viewItems
      [cpa] = analysisItems
      [hlp,abt] = helpItems
  

  -- init an model to display in the tree panel --------------------------------
  store <- Gtk.treeStoreNew [gtypeString, gtypeInt, gtypeInt, gtypeInt, gtypeBoolean, gtypeBoolean]
  initStore store
  
  -- start editor module
  editorPane <- startEditor  window fileItems editItems viewItems store
                              basicEditIORefs storeIORefs undoRedoIORefs changesIORefs
                              fileName activeTypeGraph nacIORefs

  Gtk.boxPackStart mainBox editorPane True True 0
  -- show window
  #showAll window
                    

  ----------------------------------------------------------------------------------------------------------------------------
  --------  EVENT BINDINGS  --------------------------------------------------------------------------------------------------
  ----------------------------------------------------------------------------------------------------------------------------
  
  -- event bindings for the menu toolbar -------------------------------------------------------------------------------------

  -- Analysis Menu -----------------------------------------------------------------------------------------------------------
  cpa `on` #activate $ do
    #showAll cpaWindow
        
  on cpaRunBtn #pressed $ do
    efstOrderGG <- prepToExport store graphStates nacInfoMapIORef
    sts <- readIORef graphStates
    let (tes,_,_) = fromJust $ M.lookup 0 sts
        tg = editorGetGraph tes
    case efstOrderGG of
      Left msg -> showError window (T.pack msg)
      Right gg -> do
        essential <- Gtk.toggleButtonGetActive cpaEssentialCheckBtn
        confFlag <- Gtk.toggleButtonGetActive cpaConfCheckBtn
        dependFlag <- Gtk.toggleButtonGetActive cpaDependCheckBtn
        let analysisT = case (confFlag, dependFlag) of
                          (True,False) -> CPA.Conflicts
                          (False,True) -> CPA.Dependencies
                          _-> CPA.Both
        let emptySndOrderGG = grammar (emptyGraphRule (makeTypeGraph tg)) [] [] :: Grammar (RuleMorphism Info Info)
            opts = CPA.Options Nothing False essential analysisT
            globalOpts = EGO.GOpts EGO.MonoMatches False "" False
            dpoConf = EGO.morphismsConf globalOpts
            resStr = CPA.execute' globalOpts opts dpoConf gg emptySndOrderGG
        set cpaResultBuffer [#text := ""]
        Gtk.textBufferInsertAtCursor cpaResultBuffer (T.pack resStr) (-1)
        

  -- Help Menu ---------------------------------------------------------------------------------------------------------------
  -- help
  hlp `on` #activate $ do
    #showAll helpWindow

  -- about
  abt `on` #activate $ buildAboutDialog

  -- event bindings for the main window --------------------------------------------------------------------------------------
  -- when click in the close button, the application must close
  on window #deleteEvent $ return $ do
    continue <- confirmOperation window store changedProject st nacInfoMapIORef fileName undoRedoIORefs storeIORefs
    if continue
      then do
        Gtk.mainQuit
        return False
      else return True

  -- run the program ---------------------------------------------------------------------------------------------------------
  Gtk.main