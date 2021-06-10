{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module GUI.Editor.Helper.SaveLoad
( SaveInfo(..)
, saveFile
, saveFileAs
, exportAs
, exportGGX
, loadFile
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
import GUI.XML.GGXWriter
import GUI.XML.GGXReader
import GUI.XML.VGGXWritter
import GUI.XML.VGGXReader

import GUI.Data.GraphicalInfo
import GUI.Data.GraphState
import GUI.Data.Info
import GUI.Data.SaveInfo
import GUI.Dialogs
import GUI.Helper.GrammarMaker

-- save a forest of SaveInfo
saveFile :: Tree.Forest SaveInfo -> IORef (Maybe String) -> Gtk.Window -> IO Bool
saveFile saveInfo fileName window = do
  fn <- readIORef fileName
  case fn of
    Just path -> do
      let extension = FilePath.takeExtension path
      attempt <- case extension of
                    ".vgg" -> saveVGG saveInfo path
                    _ -> saveVGGX saveInfo path
      case attempt of
        True -> return True
        False -> do
          showError window $ T.pack ("Couldn't write to file. " ++ path)
          return False
    Nothing -> saveFileAs saveInfo fileName window

saveFileAs :: Tree.Forest SaveInfo-> IORef (Maybe String) -> Gtk.Window -> IO Bool
saveFileAs saveInfo fileName window = do
  saveD <- createSaveDialog window

  vggxFilter <- Gtk.fileFilterNew
  Gtk.fileFilterAddPattern vggxFilter "*.vggx"
  Gtk.fileFilterSetName vggxFilter (Just "Verigraph-GUI Grammar XML (.vggx)")
  Gtk.fileChooserAddFilter saveD vggxFilter

  vggFilter <- Gtk.fileFilterNew
  Gtk.fileFilterAddPattern vggFilter "*.vgg"
  Gtk.fileFilterSetName vggFilter (Just "Verigraph-GUI Grammar (.vgg) [Deprecated]")
  Gtk.fileChooserAddFilter saveD vggFilter

  response <- Gtk.dialogRun saveD

  fn <- case toEnum . fromIntegral $  response of
    Gtk.ResponseTypeAccept -> do
      filename <- Gtk.fileChooserGetFilename saveD
      case filename of
        Nothing -> do
          Gtk.widgetDestroy saveD
          return Nothing
        Just path -> do
          fileFilter <- Gtk.fileChooserGetFilter saveD
          filterName <- case fileFilter of
                          Just ff -> Gtk.fileFilterGetName ff
                          Nothing -> return Nothing
          let extension = case FilePath.takeExtension path of
                        ".vgg" -> ".vgg"
                        ".vggx" -> ".vggx"
                        _ -> case filterName of
                                Just "Verigraph-GUI Grammar XML (.vggx)" -> ".vggx"
                                Just "Verigraph-GUI Grammar (.vgg) [Deprecated]" -> ".vgg"
                                Nothing -> ".vggx"
          let path' = FilePath.replaceExtension path extension
          attempt <- case extension of
              ".vgg" -> saveVGG saveInfo path'
              ".vggx" -> saveVGGX saveInfo path'
          Gtk.widgetDestroy saveD
          case attempt of
            True -> return $ Just path'
            False -> do
              showError window $ T.pack ("Couldn't write to file." ++ path')
              return Nothing
    _  -> do
      Gtk.widgetDestroy saveD
      return Nothing

  -- change fileName
  case fn of
    Just path -> do
      writeIORef fileName (Just path)
      return True
    Nothing -> return False

saveVGG :: Tree.Forest SaveInfo -> String -> IO Bool
saveVGG contents path = do
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

-- save a structure that isn't SaveInfo
exportAs :: a -> (a -> String -> IO Bool) -> Gtk.Window -> IO Bool
exportAs x saveF window = do
  saveD <- createSaveDialog window
  response <- Gtk.dialogRun saveD
  case toEnum . fromIntegral $  response of
    Gtk.ResponseTypeAccept -> do
      filename <- Gtk.fileChooserGetFilename saveD
      case filename of
        Nothing -> do
          Gtk.widgetDestroy saveD
          return False
        Just path -> do
          attempt <- saveF x path
          Gtk.widgetDestroy saveD
          if not attempt
            then showError window $ T.pack ("Couldn't write to file." ++ path)
            else return ()
          return attempt
    _  -> do
      Gtk.widgetDestroy saveD
      return False

exportGGX :: Tree.Forest SaveInfo -> String -> IO Bool
exportGGX saveInfo path = do
  writeGGX saveInfo path
  return True

loadFile :: Gtk.Window -> IO (Maybe (Tree.Forest SaveInfo,String))
loadFile window = do
  loadD <- createLoadDialog window

  allFilter <- Gtk.fileFilterNew
  Gtk.fileFilterAddPattern allFilter "*"
  Gtk.fileFilterSetName allFilter (Just "all files")
  Gtk.fileChooserAddFilter loadD allFilter

  vggxFilter <- Gtk.fileFilterNew
  Gtk.fileFilterAddPattern vggxFilter "*.vggx"
  Gtk.fileFilterSetName vggxFilter (Just "Verigraph-GUI Grammar XML (.vggx)")
  Gtk.fileChooserAddFilter loadD vggxFilter

  vggFilter <- Gtk.fileFilterNew
  Gtk.fileFilterAddPattern vggFilter "*.vgg"
  Gtk.fileFilterSetName vggFilter (Just "Verigraph-GUI Grammar (.vgg) [Deprecated]")
  Gtk.fileChooserAddFilter loadD vggFilter

  ggxFilter <- Gtk.fileFilterNew
  Gtk.fileFilterAddPattern ggxFilter "*.ggx"
  Gtk.fileFilterSetName ggxFilter (Just "AGG XML [Not fully supported]")
  Gtk.fileChooserAddFilter loadD ggxFilter

  response <- Gtk.dialogRun loadD
  case toEnum . fromIntegral $ response of
    Gtk.ResponseTypeAccept -> do
      filename <- Gtk.fileChooserGetFilename loadD
      Gtk.widgetDestroy loadD
      case filename of
        Nothing -> do
          return Nothing
        Just path -> do
          case FilePath.takeExtension path of
            ".vgg" -> loadVGG window path
            ".vggx" -> loadVGGX window path
            ".ggx" -> importGGX window path
            _ -> loadVGGX window path
    _             -> do
      Gtk.widgetDestroy loadD
      return Nothing


loadVGG :: Gtk.Window -> String -> IO (Maybe (Tree.Forest SaveInfo, String))
loadVGG window path = do
  result <- E.try (readFile path) :: IO (Either E.IOException String)
  case result of
    Left _ -> do
      showError window (T.pack "Couldn't open the file.")
      return Nothing
    Right content -> case reads content :: [(Tree.Forest SaveInfo, String)] of
      [(tree,"")] -> return $ Just (tree, path)
      _ -> do
        showError window (T.pack "Couldn't read the file.")
        return Nothing

loadVGGX :: Gtk.Window -> String -> IO (Maybe (Tree.Forest SaveInfo,String))
loadVGGX window path = do
  result <- E.try (readVGGX path) :: IO (Either E.SomeException (Maybe (Tree.Forest SaveInfo)))
  case result of
    Left e -> do
      showError window (T.pack (show e))
      return Nothing
    Right content -> return $ Just (\x -> (x,path)) <*> content


importGGX :: Gtk.Window -> String -> IO (Maybe (Tree.Forest SaveInfo, String))
importGGX window path = do
  result <- E.try (readGGX path) :: IO (Either E.SomeException (Maybe (Tree.Forest SaveInfo)))
  case result of
    Left e -> do
      showError window (T.pack (show e))
      return Nothing
    Right content -> return $ Just (\x -> (x,path)) <*> content
