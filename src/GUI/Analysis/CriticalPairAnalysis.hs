{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module GUI.Analysis.CriticalPairAnalysis (
  buildCpaBox
)
where

import qualified GI.Gtk                    as Gtk
import qualified GI.Gdk                    as Gdk

import           Control.Monad.IO.Class
import           Data.IORef
import           Data.Int
import           Data.Maybe
import           Data.GI.Base
import           Data.GI.Base.ManagedPtr   (unsafeCastTo)
import           Data.Matrix               (Matrix)
import qualified Data.Text                 as T
import qualified Data.Map                  as M

import           Abstract.Rewriting.DPO
import           Analysis.CriticalSequence (CriticalSequence)
import           Analysis.CriticalPairs    (CriticalPair)
import           Category.TypedGraphRule   (RuleMorphism)
import           Category.TypedGraph       (TypedGraphMorphism)
import           Rewriting.DPO.TypedGraph  (emptyGraphRule)
import qualified Data.TypedGraph.Morphism  as TGM
import qualified Data.Graphs               as G

import qualified Exec.GlobalOptions        as EGO
import qualified Exec.CriticalPairAnalysis as CPA

import           GUI.Data.DiaGraph
import           GUI.Data.GraphState
import           GUI.Data.Info
import           GUI.Data.Nac
import           GUI.Dialogs
import           GUI.Helper.FilePath
import           GUI.Helper.GrammarMaker

import qualified GUI.Editor.Helper.TreeStore as Edit

import qualified System.FilePath as FilePath


buildCpaBox :: Gtk.Window
            -> Gtk.TreeStore
            -> IORef (M.Map Int32 GraphState)
            -> IORef (M.Map Int32 MergeMapping)
            -> IO (Gtk.Box)
buildCpaBox window editStore statesMap nacsMergeMappings = do
  builder <- new Gtk.Builder []
  resourcesFolder <- getResourcesFolder
  Gtk.builderAddFromFile builder $ T.pack (resourcesFolder ++ "cpa.glade")

  cpaBox            <- Gtk.builderGetObject builder "cpaBox" >>= unsafeCastTo Gtk.Box . fromJust
  essentialCheckBtn <- Gtk.builderGetObject builder "essentialCheckBtn" >>= unsafeCastTo Gtk.CheckButton . fromJust
  confCheckBtn      <- Gtk.builderGetObject builder "confCheckBtn" >>= unsafeCastTo Gtk.CheckButton . fromJust
  dependCheckBtn    <- Gtk.builderGetObject builder "dependCheckBtn" >>= unsafeCastTo Gtk.CheckButton . fromJust
  execBtn           <- Gtk.builderGetObject builder "execBtn" >>= unsafeCastTo Gtk.Button . fromJust
  textView          <- Gtk.builderGetObject builder "textView" >>= unsafeCastTo Gtk.TextView . fromJust
  filePathEntry     <- Gtk.builderGetObject builder "filePathEntry" >>= unsafeCastTo Gtk.Entry . fromJust
  filePathBtn       <- Gtk.builderGetObject builder "filePathBtn" >>= unsafeCastTo Gtk.Button . fromJust
  saveResultsBtn    <- Gtk.builderGetObject builder "saveResultsBtn" >>= unsafeCastTo Gtk.Button . fromJust


  resultBuffer <- new Gtk.TextBuffer []
  set textView [#buffer := resultBuffer]

  Gtk.textBufferInsertAtCursor resultBuffer "Critical Pair Analysis\n" (-1)

  conflicts    <- newIORef (Nothing :: Maybe (Matrix (String, String, [CriticalPair (TypedGraphMorphism Info Info)])) )
  dependencies <- newIORef (Nothing :: Maybe (Matrix (String, String, [CriticalSequence (TypedGraphMorphism Info Info)])) )
  executedGG   <- newIORef (Nothing :: Maybe (Grammar (TGM.TypedGraphMorphism Info Info)) )

  on execBtn #pressed $ do
    efstOrderGG <- prepToExport editStore statesMap nacsMergeMappings
    sts <- readIORef statesMap
    let tes = fromJust $ M.lookup 0 sts
        tg = stateGetGraph tes
    case efstOrderGG of
      Left msg -> showError window (T.pack msg)
      Right gg -> do
        essential <- Gtk.toggleButtonGetActive essentialCheckBtn
        confFlag <- Gtk.toggleButtonGetActive confCheckBtn
        dependFlag <- Gtk.toggleButtonGetActive dependCheckBtn
        let analysisT = case (confFlag, dependFlag) of
                          (True,False) -> CPA.Conflicts
                          (False,True) -> CPA.Dependencies
                          _-> CPA.Both
        let opts = CPA.Options Nothing False essential analysisT
            globalOpts = EGO.GOpts EGO.MonoMatches False "" False
            dpoConf = EGO.morphismsConf globalOpts
            (resStr,conflicts',dependencies') = CPA.executeInGUI globalOpts opts dpoConf gg
        set resultBuffer [#text := ""]
        Gtk.textBufferInsertAtCursor resultBuffer (T.pack resStr) (-1)
        writeIORef conflicts conflicts'
        writeIORef dependencies dependencies'
        writeIORef executedGG $ Just gg

  on saveResultsBtn #pressed $ do
    maybeGrammar    <- readIORef executedGG
    conflicts'      <- readIORef conflicts
    dependencies'   <- readIORef dependencies
    filePath <- Gtk.entryGetText filePathEntry >>= return . T.unpack
    case (maybeGrammar, filePath) of
      (Nothing, _) -> showError window "The analysis must be run before saving the results"
      (_,"")       -> showError window "File path empty"
      (Just gg, path) -> do
        sts <- readIORef statesMap
        let tes = fromJust $ M.lookup 0 sts
            tg = stateGetGraph tes
            nods = G.nodes tg
            edgs = G.edges tg
            nodeNames = map (\n -> ('N' : (show . fromEnum . G.nodeId $ n), (infoLabelStr . G.nodeInfo $ n) ++ "%:[NODE]:" )) nods
            edgeNames = map (\e -> ('E' : (show . fromEnum . G.edgeId $ e), (infoLabelStr . G.edgeInfo $ e) ++ "%:[EDGE]:" )) edgs
            names = nodeNames ++ edgeNames
            emptySndOrderGG = grammar (emptyGraphRule (makeTypeGraph tg)) [] [] :: Grammar (RuleMorphism Info Info)
            ggName = FilePath.takeBaseName path
        CPA.saveCPXResults gg emptySndOrderGG conflicts' dependencies' ggName names path
        Gtk.textBufferInsertAtCursor resultBuffer (T.pack $ "\n\n result of analysis saved in " ++ path) (-1)

  on filePathBtn #pressed $ do
    saveD <- createSaveDialog window
    response <- Gtk.dialogRun saveD
    case toEnum . fromIntegral $  response of
      Gtk.ResponseTypeAccept -> do
        filename <- Gtk.fileChooserGetFilename saveD
        case filename of
          Nothing -> return ()
          Just path -> do
            let path' = T.pack $ case FilePath.takeExtension path of
                      ".cpx" -> path
                      _ -> FilePath.replaceExtension path ".cpx"
            Gtk.entrySetText filePathEntry path'
      _  -> return ()
    Gtk.widgetDestroy saveD

  return cpaBox



prepToExport :: Gtk.TreeStore
             -> IORef (M.Map Int32 GraphState)
             -> IORef (M.Map Int32 MergeMapping)
             -> IO (Either String (Grammar (TGM.TypedGraphMorphism Info Info)))
prepToExport store graphStates nacsMergeMappings = do
  sts <- readIORef graphStates

  let tg = stateGetGraph . fromJust $ M.lookup 0 sts
      hg = stateGetGraph . fromJust $ M.lookup 1 sts

  rules <- Edit.getRules store graphStates nacsMergeMappings
  let rulesNames = map (\(_,_,name) -> name) rules
      rulesNnacs = map (\(r,ns,_) -> (r,ns)) rules

  let efstOrderGG = makeGrammar tg hg rulesNnacs rulesNames
  return efstOrderGG
