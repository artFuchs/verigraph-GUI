{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module GUI.Analysis.ModelChecker (
  buildStateSpaceBox
) where

-- GTK modules
import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import           Data.GI.Base

-- modules needed for threads
import           Control.Concurrent
import           Control.Concurrent.MVar
import qualified GI.GLib as GLib

-- Haskell structures
import           Data.IORef
import           Data.Int
import           Data.Maybe
import qualified Data.IntMap                        as IntMap
import qualified Data.Text                          as T
import qualified Data.Map                           as M
import qualified Data.Set                           as Set

-- verigraph structures
import qualified  Abstract.Category                 as Cat
import qualified  Abstract.Rewriting.DPO            as DPO
import qualified  Abstract.Rewriting.DPO.StateSpace as SS
import qualified  Data.Graphs                       as G
import qualified  Data.TypedGraph.Morphism          as TGM
import qualified  Data.TypedGraph                   as TG
import            Rewriting.DPO.TypedGraph

-- GUI modules
import            GUI.Data.Info hiding (empty)
import qualified  GUI.Data.Info as Info
import            GUI.Data.Nac
import            GUI.Data.DiaGraph
import            GUI.Data.GraphState
import            GUI.Data.GraphicalInfo
import            GUI.Helper.BasicCanvasCallbacks
import            GUI.Helper.GraphicalInfo
import            GUI.Helper.GrammarMaker
import            GUI.Helper.FilePath
import            GUI.Dialogs
import            GUI.Render.Render
import            GUI.Render.GraphDraw

import qualified  GUI.Editor as Edit

-- User Interface -----------------------------------------------------------------------------------------
buildStateSpaceBox :: Gtk.Window
                   -> Gtk.TreeStore
                   -> Gtk.MenuItem
                   -> IORef (Maybe Gtk.DrawingArea)
                   -> IORef (Maybe (IORef GraphState))
                   -> IORef (M.Map Int32 GraphState)
                   -> IORef (M.Map Int32 (DiaGraph, MergeMapping))
                   -> IO (Gtk.Box)
buildStateSpaceBox window store genStateSpaceItem focusedCanvas focusedStateIORef graphStatesIORef nacsInfoIORef = do
  -- build -- box
  builder <- new Gtk.Builder []
  resourcesFolder <- getResourcesFolder
  Gtk.builderAddFromFile builder $ T.pack (resourcesFolder ++ "stateSpace.glade")

  mainBox <- Gtk.builderGetObject builder "main_box" >>= unsafeCastTo Gtk.Box . fromJust
  generateBtn <- Gtk.builderGetObject builder "generate_button" >>= unsafeCastTo Gtk.Button . fromJust
  stopBtn <- Gtk.builderGetObject builder "stop_button" >>= unsafeCastTo Gtk.Button . fromJust
  depthSpinBtn <- Gtk.builderGetObject builder "depth_spin_button" >>= unsafeCastTo Gtk.SpinButton . fromJust
  statusSpinner <- Gtk.builderGetObject builder "status_spinner" >>= unsafeCastTo Gtk.Spinner . fromJust
  statusLabel <- Gtk.builderGetObject builder "status_label" >>= unsafeCastTo Gtk.Label . fromJust
  canvas <- Gtk.builderGetObject builder "canvas" >>= unsafeCastTo Gtk.DrawingArea . fromJust
  Gtk.widgetSetEvents canvas [toEnum $ fromEnum Gdk.EventMaskAllEventsMask - fromEnum Gdk.EventMaskSmoothScrollMask]

  Gtk.spinnerStop statusSpinner
  Gtk.labelSetText statusLabel ""

  -- IORefs
  ssGraphState <- newIORef emptyState
  execThread <- newIORef Nothing
  fstStateIORef <- newIORef []

  -- bindings ------------------------------------------------------------------------
  -- controls
  on genStateSpaceItem #activate $ do
    eGG <- Edit.prepToExport store graphStatesIORef nacsInfoIORef
    case eGG of
      Left msg -> showError window (T.pack msg)
      Right grammar -> do
        stMVar <- newEmptyMVar
        depth <- Gtk.spinButtonGetValueAsInt depthSpinBtn >>= return . fromIntegral
        context <- Gtk.widgetGetPangoContext canvas
        execT <- forkFinally  (do
                                Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
                                    Gtk.spinnerStart statusSpinner
                                    Gtk.labelSetText statusLabel "generating state space"
                                    return False

                                let initialGraph = DPO.start grammar
                                    mconf = (DPO.MorphismsConfig Cat.monic) :: DPO.MorphismsConfig (TGM.TypedGraphMorphism Info Info)
                                    (initialStates, stateSpace) = exploreStateSpace mconf depth grammar [("initialGraph",initialGraph)]
                                    st = generateGraphState initialStates stateSpace
                                    (ngi,egi) = stateGetGI st
                                    g = stateGetGraph st

                                ngi' <- updateNodesGiDims ngi g context
                                st' <- return $ stateSetGI (ngi',egi) st
                                putMVar stMVar st'
                              )
                              (\_ -> do
                                Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
                                  interruptedComputation <- isEmptyMVar stMVar
                                  if interruptedComputation
                                    then return ()
                                    else do
                                      st <- takeMVar stMVar
                                      writeIORef ssGraphState st
                                  writeIORef execThread Nothing
                                  Gtk.spinnerStop statusSpinner
                                  Gtk.labelSetText statusLabel ""
                                  Gtk.widgetQueueDraw canvas
                                  return False
                                return ()
                              )
        writeIORef execThread (Just execT)

  on generateBtn #pressed $ Gtk.menuItemActivate genStateSpaceItem

  on stopBtn #pressed $ do
    execT <- readIORef execThread
    case execT of
      Nothing -> return ()
      Just t -> do
        killThread t
        writeIORef execThread Nothing

  -- canvas - to draw the state space graph
  oldPoint        <- newIORef (0.0,0.0) -- last point where a mouse button was pressed
  squareSelection <- newIORef Nothing   -- selection box : Maybe (x1,y1,x2,y2)
  on canvas #draw $ \context -> do
    ss <- readIORef ssGraphState
    sq <- readIORef squareSelection
    renderWithContext context   $ drawTypeGraph ss sq
    return False
  on canvas #buttonPressEvent   $ basicCanvasButtonPressedCallback ssGraphState oldPoint squareSelection canvas
  on canvas #motionNotifyEvent  $ basicCanvasMotionCallBack ssGraphState oldPoint squareSelection canvas
  on canvas #buttonReleaseEvent $ basicCanvasButtonReleasedCallback ssGraphState squareSelection canvas
  on canvas #scrollEvent        $ basicCanvasScrollCallback ssGraphState canvas
  on canvas #focusInEvent       $ \event -> do
      writeIORef focusedCanvas     $ Just canvas
      writeIORef focusedStateIORef $ Just ssGraphState
      return False

  return mainBox

generateGraphState :: [Int] -> Space Info Info -> GraphState
generateGraphState initialStates stateSpace = st'
  where
    addLevel levels (a,b) = case M.lookup a levels of
                              Nothing -> M.insert b 1 $ M.insert a 0 levels
                              Just l -> M.alter
                                        (\mx -> case mx of
                                                  Nothing -> Just (l+1)
                                                  Just x -> Just (min x (l+1)))
                                        b levels
    nidsWithLevels = Set.foldl addLevel (M.singleton 0 0) (SS.transitions stateSpace) :: M.Map Int Int
    organizeLevels a l levels = case M.lookup l levels of
                              Nothing -> M.insert l [a] levels
                              Just ls -> M.insert l (ls++[a]) levels
    levels = M.foldrWithKey organizeLevels (M.empty) nidsWithLevels :: M.Map Int [Int]
    getStatePredicates nid = fromMaybe [] $ snd <$> (IntMap.lookup nid (SS.states stateSpace))
    nodeWithPos nid posX posY =
      let
        predicates = getStatePredicates nid
        label = init $ unlines $ ("state " ++ show nid ++ "\n"):predicates
      in
        (G.Node (G.NodeId nid) (infoSetLabel Info.empty label), (fromIntegral posX, fromIntegral posY))
    addNodeInLevel l nids nds = foldr (\(nid,posX) ls -> (nodeWithPos nid posX (l*50)):ls ) nds (zip nids [0,50..])
    nds = M.foldrWithKey addNodeInLevel [] levels
    ndsGIs = M.fromList $ map (\(n,pos) -> (fromEnum $ G.nodeId n, newNodeGI {position = pos, shape = NRect})) nds
    g = G.fromNodesAndEdges (map fst nds) []
    st = stateSetGI (ndsGIs,M.empty) $ stateSetGraph g $ emptyState
    st' = Set.foldr (\(a,b) st -> createEdge st (G.NodeId a) (G.NodeId b) (infoSetLabel Info.empty (show (a,b))) False ENormal (0,0,0)) st (SS.transitions stateSpace)


-- definitions taken from Verigraph CLI/ModelChecker.hs -------------------------------------------------------------
exploreStateSpace :: DPO.MorphismsConfig (TGM.TypedGraphMorphism a b) -> Int -> DPO.Grammar (TGM.TypedGraphMorphism a b) -> [(String, TG.TypedGraph a b)] -> ([Int], Space a b)
exploreStateSpace conf maxDepth grammar graphs =
  let
    (productions, predicates) =
      splitPredicates (DPO.productions grammar)

    searchFrom (_, graph) =
      do
        (idx, _) <- SS.putState graph
        SS.depthSearch maxDepth graph
        return idx

    search =
      mapM searchFrom graphs

    initialSpace =
      SS.empty conf (map snd productions) predicates
  in
    SS.runStateSpaceBuilder search initialSpace

type NamedPredicate a b = (String, TypedGraphRule a b)
type NamedProduction a b = (String, TypedGraphRule a b)
type Space a b = SS.StateSpace (TGM.TypedGraphMorphism a b)

-- | Separates the rules that change nothing (which are considered predicates)
-- from those that have some effect (which are considered productions).
splitPredicates :: [(String, TypedGraphRule a b)] -> ([NamedProduction a b], [NamedPredicate a b])
splitPredicates [] =
  ([], [])

splitPredicates ((name, rule) : rest) =
  let
    (productions, predicates) =
      splitPredicates rest
  in
    if Cat.isIsomorphism (leftMorphism rule) && Cat.isIsomorphism (rightMorphism rule) then
      (productions, (name, rule):predicates)
    else
      ((name, rule):productions, predicates)
