{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module GUI.Editor.Helper.SaveLoad
( SaveInfo(..)
, saveFile
, saveFileAs
, saveVGGX
, exportGGX
, saveProject
, loadFile
, loadProject
, loadVGGX
)where

import qualified GI.Gtk as Gtk

import qualified System.FilePath as FilePath

import qualified Data.Tree as Tree
import qualified Data.Text as T
import qualified Data.Map as M
import Data.List
import Data.Int
import Data.IORef
import qualified Control.Exception as E

import Data.Graphs hiding (null, empty)
import Data.Graphs.Morphism
import qualified Data.Graphs as G
import qualified Data.TypedGraph.Morphism as TGM
import qualified Data.TypedGraph as TG
import Abstract.Constraint
import Abstract.Rewriting.DPO
import Rewriting.DPO.TypedGraph
import Category.TypedGraphRule (RuleMorphism)
import XML.GGXWriter
import XML.VGGXWritter
import XML.VGGXReader

import GUI.Data.GraphicalInfo
import GUI.Data.GraphState
import GUI.Data.Info
import GUI.Data.SaveInfo
import GUI.Dialogs
import GUI.Helper.GrammarMaker


--------------------------------------------------------------------------------
-- functions -------------------------------------------------------------------

saveFile :: a -> (a -> String -> IO Bool) -> IORef (Maybe String) -> Gtk.Window -> IO Bool
saveFile x saveF fileName window = do
  fn <- readIORef fileName
  case fn of
    Just path -> do
      tentativa <- saveF x path
      case tentativa of
        True -> return True
        False -> do
          showError window $ T.pack ("Couldn't write to file." ++ path)
          return False
    Nothing -> saveFileAs x saveF fileName (Just ".vgg") window True
    

saveFileAs :: a -> (a -> String -> IO Bool) -> IORef (Maybe String) -> Maybe String -> Gtk.Window -> Bool -> IO Bool
saveFileAs x saveF fileName extension window changeFN = do
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
          path' <- case extension of
            Just ext -> return $ FilePath.replaceExtension path ext
            Nothing -> return path
          attempt <- saveF x path'
          case attempt of
            True -> do
              Gtk.widgetDestroy saveD
              return $ Just path'
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


saveProject :: Tree.Forest SaveInfo -> String -> IO Bool
saveProject contents path = do
  let nodeContents g = map (\(Node nid info) -> (fromEnum nid, info)) (nodes g)
      edgeContents g = map (\(Edge eid srcid tgtid info) -> (fromEnum eid, fromEnum srcid, fromEnum tgtid, info)) (edges g)
      nodeContents' es = nodeContents (stateGetGraph es)
      edgeContents' es = edgeContents (stateGetGraph es)
      toIntPairs m = map (\(a,b) -> (fromEnum a, fromEnum b)) $ M.toList m
      writeProject = writeFile path $ show contents
  saveTry <- E.try (writeProject)  :: IO (Either E.IOException ())
  case saveTry of
    Left _ -> return False
    Right _ -> return True

saveVGGX :: Tree.Forest SaveInfo -> String -> IO Bool
saveVGGX contents path = do 
  writeVGGX contents path
  return True

exportGGX :: (Grammar (TGM.TypedGraphMorphism Info Info), Graph Info Info) -> String -> IO Bool
exportGGX (fstOrderGG, tg)  path = do
  let nods = nodes tg
      edgs = edges tg
      nodeNames = map (\n -> ('N' : (show . fromEnum . nodeId $ n), (infoLabelStr . nodeInfo $ n) ++ "%:[NODE]:" )) nods
      edgeNames = map (\e -> ('E' : (show . fromEnum . edgeId $ e), (infoLabelStr . edgeInfo $ e) ++ "%:[EDGE]:" )) edgs
      names = nodeNames ++ edgeNames

  let emptySndOrderGG = grammar (emptyGraphRule (makeTypeGraph tg)) [] [] :: Grammar (RuleMorphism Info Info)
  let ggName = reverse . takeWhile (/= '/') . drop 4 . reverse $ path

  writeGrammarFile (fstOrderGG,emptySndOrderGG) ggName names path
  return True

loadFile :: Gtk.Window -> (String -> Maybe a) -> (T.Text, T.Text) -> IO (Maybe (a,String))
loadFile window loadF (filterName, extension) = do
  loadD <- createLoadDialog window

  filter <- Gtk.fileFilterNew
  Gtk.fileFilterAddPattern filter extension
  Gtk.fileFilterSetName filter (Just filterName)
  Gtk.fileChooserAddFilter loadD filter
  filters <- Gtk.fileChooserListFilters loadD

  allFilter <- Gtk.fileFilterNew
  Gtk.fileFilterAddPattern allFilter "*"
  Gtk.fileFilterSetName allFilter (Just "all file formats")
  Gtk.fileChooserAddFilter loadD allFilter


  response <- Gtk.dialogRun loadD
  case toEnum . fromIntegral $ response of
    Gtk.ResponseTypeAccept -> do
      filename <- Gtk.fileChooserGetFilename loadD
      Gtk.widgetDestroy loadD
      case filename of
        Nothing -> do
          return Nothing
        Just path -> do
          result <- E.try (readFile path) :: IO (Either E.IOException String)
          case result of
            Left _ -> do
              showError window (T.pack "Couldn't open the file")
              return Nothing
            Right content -> case loadF content of
              Nothing -> do
                showError window (T.pack "Couldn't read the file")
                return Nothing
              Just x -> return $ Just (x, path)
    _             -> do
      Gtk.widgetDestroy loadD
      return Nothing


loadProject :: String -> Maybe (Tree.Forest SaveInfo)
loadProject content = loadedTree
  where
    loadedTree = case reads content :: [(Tree.Forest SaveInfo, String)] of
      [(tree,"")] -> Just tree
      _ -> Nothing

loadVGGX :: Gtk.Window -> IO (Maybe (Tree.Forest SaveInfo,String))
loadVGGX window = do
  loadD <- createLoadDialog window

  xmlFilter <- Gtk.fileFilterNew
  Gtk.fileFilterAddPattern xmlFilter "*.vggx"
  Gtk.fileFilterSetName xmlFilter (Just "verigraph-gui grammar XML file")
  Gtk.fileChooserAddFilter loadD xmlFilter

  response <- Gtk.dialogRun loadD
  case toEnum . fromIntegral $ response of
    Gtk.ResponseTypeAccept -> do
      filename <- Gtk.fileChooserGetFilename loadD
      Gtk.widgetDestroy loadD
      case filename of
        Nothing -> do
          return Nothing
        Just path -> do
          result <- E.try (readVGGX path) :: IO (Either E.IOException (Maybe (Tree.Forest SaveInfo)))
          case result of
            Left _ -> do
              showError window (T.pack "Couldn't open the file")
              return Nothing
            Right content -> return $ Just (\x -> (x,path)) <*> content
    _             -> do
      Gtk.widgetDestroy loadD
      return Nothing