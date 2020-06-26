module GUI.Editor.Helper.SaveLoad
( SaveInfo(..)
, saveFile
, saveFileAs
, saveGraph
, exportGGX
, exportVGG
, saveProject
, loadFile
, loadGraph
, loadProject
)where

import qualified GI.Gtk as Gtk

import qualified System.FilePath as FilePath

import qualified Data.Tree as Tree
import qualified Data.Text as T
import qualified Data.Map as M
import Data.List
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

import GUI.Data.GraphicalInfo
import GUI.Data.EditorState
import GUI.Data.Info
import GUI.Dialogs
import GUI.Editor.Helper.GrammarMaker


--------------------------------------------------------------------------------
-- structs ---------------------------------------------------------------------

type NList = [(Int,Info)]
type EList = [(Int,Int,Int,Info)]
type NACInfo = ((Graph Info Info,GraphicalInfo), (M.Map NodeId NodeId, M.Map EdgeId EdgeId))
data SaveInfo = Topic String | TypeGraph String EditorState | HostGraph String EditorState | RuleGraph String EditorState Bool | NacGraph String NACInfo deriving (Show)
data UncompressedSaveInfo = T String
                          | TG String NList EList GraphicalInfo
                          | HG String NList EList GraphicalInfo
                          | RG String NList EList GraphicalInfo Bool
                          | NG String NList EList [(Int,Int)] [(Int,Int)] GraphicalInfo
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


saveGraph :: (Graph Info Info ,GraphicalInfo) -> String -> IO Bool
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
  let nodeContents g = map (\(Node nid info) -> (fromEnum nid, info)) (nodes g)
      edgeContents g = map (\(Edge eid srcid tgtid info) -> (fromEnum eid, fromEnum srcid, fromEnum tgtid, info)) (edges g)
      nodeContents' es = nodeContents (editorGetGraph es)
      edgeContents' es = edgeContents (editorGetGraph es)
      toIntPairs m = map (\(a,b) -> (fromEnum a, fromEnum b)) $ M.toList m
      contents =  map
                  (fmap (\node -> case node of
                                      Topic name -> T name
                                      TypeGraph name es -> TG name (nodeContents' es) (edgeContents' es) (editorGetGI es)
                                      HostGraph name es -> HG name (nodeContents' es) (edgeContents' es) (editorGetGI es)
                                      RuleGraph name es a -> RG name (nodeContents' es) (edgeContents' es) (editorGetGI es) a
                                      NacGraph name ((g,gi),(nm,em)) -> NG name (nodeContents g) (edgeContents g) (toIntPairs nm) (toIntPairs em) gi))
                  saveInfo
      writeProject = writeFile path $ show contents
  saveTry <- E.try (writeProject)  :: IO (Either E.IOException ())
  case saveTry of
    Left _ -> return False
    Right _ -> return True


exportGGX :: (Grammar (TGM.TypedGraphMorphism Info Info), Graph Info Info) -> String -> IO Bool
exportGGX (fstOrderGG, tg)  path = do
  let path' = if FilePath.takeExtension path == ".ggx" then path else FilePath.replaceExtension path ".ggx"
  let nods = nodes tg
      edgs = edges tg
      nodeNames = map (\n -> ('N' : (show . fromEnum . nodeId $ n), (infoLabelStr . nodeInfo $ n) ++ "%:[NODE]:" )) nods
      edgeNames = map (\e -> ('E' : (show . fromEnum . edgeId $ e), (infoLabelStr . edgeInfo $ e) ++ "%:[EDGE]:" )) edgs
      names = nodeNames ++ edgeNames

  let emptySndOrderGG = grammar (emptyGraphRule (makeTypeGraph tg)) [] [] :: Grammar (RuleMorphism Info Info)
  let ggName = reverse . takeWhile (/= '/') . drop 4 . reverse $ path'

  writeGrammarFile (fstOrderGG,emptySndOrderGG) ggName names path'
  return True

exportVGG :: Grammar (TGM.TypedGraphMorphism Info Info) -> String -> IO Bool
exportVGG fstOrderGG path = do
  let path' = if FilePath.takeExtension path == ".vgg" then path else FilePath.replaceExtension path ".vgg"

  -- functions of conversion
  -- Grammar (TGM.TypedGraphMorphism Info Info) -> Grammar (TGM.TypedGraphMorphism String String)
  let changeNodes = foldr (\(id,n) l -> (id, n {nodeInfo = Just show <*> (nodeInfo n) }):l) []
      changeEdges = foldr (\(id,e) l -> (id, e {edgeInfo = Just show <*> (edgeInfo e) }):l) []
      -- f :: Graph Info Info -> Graph String String
      f g = g { nodeMap = changeNodes (nodeMap g)
              , edgeMap = changeEdges (edgeMap g)
              }
      -- ff :: TypedGraph Info Info -> TypedGraph String String
      -- ff :: GraphMorphism (Maybe Info) (Maybe Info) -> GraphMorphism (Maybe String) (Maybe String)
      ff tg = tg { domainGraph = f (domainGraph tg)
                 , codomainGraph = f (codomainGraph tg)}
      -- fff :: TypedGraphMorphism Info Info -> TypedGraphMorphism String String
      fff tgm = tgm { TGM.domainGraph = ff (TGM.domainGraph tgm)
                    , TGM.codomainGraph = ff (TGM.codomainGraph tgm)
                    , TGM.mapping = ff (TGM.mapping tgm)}

  let startGraph = ff (start fstOrderGG)

  let rules = map (\(n, rule) -> (n, rule { leftMorphism = fff (leftMorphism rule)
                                           , rightMorphism = fff (rightMorphism rule)
                                           , nacs = map fff (nacs rule) }
                                  )
                  ) 
                  (productions fstOrderGG)

  let cs = [] :: [Constraint (TGM.TypedGraphMorphism String String)]

  let writeVGG = writeFile path' $ show (startGraph, cs, rules) 
  saveTry <- E.try (writeVGG) :: IO (Either E.IOException ())
  case saveTry of
    Left _ -> return False
    Right _ -> return True

  



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


loadGraph :: String -> Maybe (Graph Info Info,GraphicalInfo)
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
    genNodeMap = M.fromList . map (\(n1,n2) -> (NodeId n1, NodeId n2))
    genEdgeMap = M.fromList . map (\(e1,e2) -> (EdgeId e1, EdgeId e2))
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
                              NG name nlist elist nmap emap gi -> let g = fromNodesAndEdges (genNodes nlist) (genEdges elist)
                                                                      nm' = genNodeMap nmap
                                                                      em' = genEdgeMap emap
                                                                  in NacGraph name ((g,gi),(nm',em'))

                  ) )
