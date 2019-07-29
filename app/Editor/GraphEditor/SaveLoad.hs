module Editor.GraphEditor.SaveLoad
( SaveInfo(..)
, saveFile
, saveFileAs
, saveGraph
, exportGGX
, saveProject
, loadFile
, loadGraph
, loadProject
)where

import Editor.GraphEditor.UIBuilders
import Editor.Data.GraphicalInfo
import Editor.Data.EditorState
import Data.Graphs hiding (null, empty)
import qualified Data.Graphs as G
import qualified Data.Tree as Tree
import qualified GI.Gtk as Gtk
import qualified Control.Exception as E
import qualified Data.Text as T
import Data.IORef
import Data.List
import Abstract.Rewriting.DPO
import qualified Data.TypedGraph.Morphism as TGM
import XML.GGXWriter
import Editor.GraphEditor.GrammarMaker
import Category.TypedGraphRule (RuleMorphism)
import Rewriting.DPO.TypedGraph
import Editor.Data.Info
--------------------------------------------------------------------------------
-- structs ---------------------------------------------------------------------

type NList = [(Int,String)]
type EList = [(Int,Int,Int,String)]
data SaveInfo = Topic String | TypeGraph String EditorState | HostGraph String EditorState | RuleGraph String EditorState Bool deriving (Show)
data UncompressedSaveInfo = T String
                          | TG String NList EList GraphicalInfo
                          | HG String NList EList GraphicalInfo
                          | RG String NList EList GraphicalInfo Bool
                          deriving (Show, Read)
--------------------------------------------------------------------------------
-- functions -------------------------------------------------------------------

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


saveProject :: Tree.Forest SaveInfo -> String -> IO Bool
saveProject saveInfo path = do
  let nodeContents es = map (\(Node nid info) -> (fromEnum nid, info)) (nodes $ editorGetGraph es)
      edgeContents es = map (\(Edge eid srcid tgtid info) -> (fromEnum eid, fromEnum srcid, fromEnum tgtid, info)) (edges $ editorGetGraph es)
      contents =  map
                  (fmap (\node -> case node of
                                      Topic name -> T name
                                      TypeGraph name es -> TG name (nodeContents es) (edgeContents es) (editorGetGI es)
                                      HostGraph name es -> HG name (nodeContents es) (edgeContents es) (editorGetGI es)
                                      RuleGraph name es a -> RG name (nodeContents es) (edgeContents es) (editorGetGI es) a))
                  saveInfo
      writeProject = writeFile path $ show contents
  saveTry <- E.try (writeProject)  :: IO (Either E.IOException ())
  case saveTry of
    Left _ -> return False
    Right _ -> return True


exportGGX :: (Grammar (TGM.TypedGraphMorphism String String), Graph String String) -> String -> IO Bool
exportGGX (fstOrderGG, tg)  path = do
  let path' = if (tails path)!!(length path-4) == ".ggx" then path else path ++ ".ggx"
  let nods = nodes tg
      edgs = edges tg
      nodeNames = map (\n -> ('N' : (show . nodeId $ n), (infoLabel . nodeInfo $ n) ++ "%:[NODE]:" )) nods
      edgeNames = map (\e -> ('E' : (show . edgeId $ e), (infoLabel . edgeInfo $ e) ++ "%:[EDGE]:" )) edgs
      names = nodeNames ++ edgeNames

  let emptySndOrderGG = grammar (emptyGraphRule (makeTypeGraph tg)) [] [] :: Grammar (RuleMorphism String String)
  let ggName = reverse . takeWhile (/= '/') . drop 4 . reverse $ path'

  writeGrammarFile (fstOrderGG,emptySndOrderGG) ggName names path'
  return True


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


loadGraph :: String -> Maybe (Graph String String,GraphicalInfo)
loadGraph contents = result
  where
    result = case reads contents :: [( (NList, EList, GraphicalInfo), String)] of
      [((rns,res,gi), "")] -> let ns = map (\(nid, info) -> Node (NodeId nid) info) rns
                                  es = map (\(eid, src, dst, info) -> Edge (EdgeId eid) (NodeId src) (NodeId dst) info) res
                                  g = fromNodesAndEdges ns es
                              in Just (g,gi)
      _ -> Nothing


loadProject :: String -> Maybe (Tree.Forest SaveInfo)
loadProject content = loadedTree
  where
    loadedTree = case reads content :: [(Tree.Forest UncompressedSaveInfo, String)] of
      [(tree,"")] -> Just $ compress tree
      _ -> Nothing
    genNodes = map (\(nid, info) -> Node (NodeId nid) info)
    genEdges = map (\(eid, src, dst, info) -> Edge (EdgeId eid) (NodeId src) (NodeId dst) info)
    compress = map
               (fmap
                  (\node -> case node of
                              T name -> Topic name
                              TG name nlist elist gi -> let g = fromNodesAndEdges (genNodes nlist) (genEdges elist)
                                                            es = editorSetGI gi . editorSetGraph g $ emptyES
                                                        in TypeGraph name es
                              HG name nlist elist gi -> let g = fromNodesAndEdges (genNodes nlist) (genEdges elist)
                                                            es = editorSetGI gi . editorSetGraph g $ emptyES
                                                        in HostGraph name es
                              RG name nlist elist gi a -> let g = fromNodesAndEdges (genNodes nlist) (genEdges elist)
                                                              es = editorSetGI gi . editorSetGraph g $ emptyES
                                                          in RuleGraph name es a
                  ) )
