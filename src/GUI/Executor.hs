{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module GUI.Executor (
  buildExecutor
, ExecGraphEntry
, updateTreeStore
, removeFromTreeStore
, removeTrashFromTreeStore
) where

-- GTK related modules
import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import Graphics.Rendering.Cairo (Render)

-- basic modules
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.IORef
import           Data.Maybe
import qualified Data.Map as M
import           Data.GI.Base
import           Data.GI.Base.ManagedPtr (unsafeCastTo)
import           Data.Int
import qualified Data.Text as T

-- Verigraph modules
import qualified Abstract.Category as Cat
import qualified Abstract.Rewriting.DPO as DPO
import qualified Data.TypedGraph.Morphism as TGM
import qualified Data.Graphs.Morphism as GM
import qualified Data.Graphs as G
import qualified Data.Relation as R

-- Verigraph-GUI modules
import           GUI.Data.DiaGraph hiding (empty)
import qualified GUI.Data.DiaGraph as DG
import           GUI.Data.GraphState
import           GUI.Data.Info
import           GUI.Data.Nac
import           GUI.Render.Render
import           GUI.Render.GraphDraw
import qualified GUI.Helper.GrammarMaker as GMker

-- shouldn't use functions from editor module. Must refactore later
import qualified GUI.Editor as Editor
import qualified GUI.Editor.Helper.Nac as Nac

buildExecutor :: Gtk.TreeStore 
              -> IORef (M.Map Int32 GraphState) 
              -> IORef (G.Graph Info Info) 
              -> IORef (M.Map Int32 NacInfo)
              -> IORef (Maybe Gtk.DrawingArea) -> IORef (Maybe (IORef GraphState))
              -> IO (Gtk.Paned, Gtk.DrawingArea, Gtk.ComboBoxText, IORef GraphState, IORef Bool, IORef (M.Map Int32 [(String, Int32)]))
buildExecutor store statesMap typeGraph nacInfoMap focusedCanvas focusedStateIORef = do
    builder <- new Gtk.Builder []
    Gtk.builderAddFromFile builder "./Resources/executor.glade"

    executorPane <- Gtk.builderGetObject builder "executorPane" >>= unsafeCastTo Gtk.Paned . fromJust

    execPane  <- Gtk.builderGetObject builder "execPane" >>= unsafeCastTo Gtk.Paned . fromJust 
    hideRVBtn <- Gtk.builderGetObject builder "hideRVBtn" >>= unsafeCastTo Gtk.Button . fromJust

    mainCanvas <- Gtk.builderGetObject builder "mainCanvas" >>= unsafeCastTo Gtk.DrawingArea . fromJust
    nacCanvas  <- Gtk.builderGetObject builder "nacCanvas" >>= unsafeCastTo Gtk.DrawingArea . fromJust
    lCanvas    <- Gtk.builderGetObject builder "lCanvas" >>= unsafeCastTo Gtk.DrawingArea . fromJust
    rCanvas    <- Gtk.builderGetObject builder "rCanvas" >>= unsafeCastTo Gtk.DrawingArea . fromJust
    ruleCanvas    <- Gtk.builderGetObject builder "ruleCanvas" >>= unsafeCastTo Gtk.DrawingArea . fromJust
    Gtk.widgetSetEvents mainCanvas [toEnum $ fromEnum Gdk.EventMaskAllEventsMask - fromEnum Gdk.EventMaskSmoothScrollMask]
    Gtk.widgetSetEvents nacCanvas [toEnum $ fromEnum Gdk.EventMaskAllEventsMask - fromEnum Gdk.EventMaskSmoothScrollMask]
    Gtk.widgetSetEvents lCanvas [toEnum $ fromEnum Gdk.EventMaskAllEventsMask - fromEnum Gdk.EventMaskSmoothScrollMask]
    Gtk.widgetSetEvents rCanvas [toEnum $ fromEnum Gdk.EventMaskAllEventsMask - fromEnum Gdk.EventMaskSmoothScrollMask]
    Gtk.widgetSetEvents ruleCanvas [toEnum $ fromEnum Gdk.EventMaskAllEventsMask - fromEnum Gdk.EventMaskSmoothScrollMask]

    nacCBox <- Gtk.builderGetObject builder "nacCBox" >>= unsafeCastTo Gtk.ComboBoxText . fromJust

    stopBtn <- Gtk.builderGetObject builder "stopBtn" >>= unsafeCastTo Gtk.Button . fromJust
    stepBtn <- Gtk.builderGetObject builder "stepBtn" >>= unsafeCastTo Gtk.Button . fromJust

    treeView <- Gtk.builderGetObject builder "treeView" >>= unsafeCastTo Gtk.TreeView . fromJust
    Gtk.treeViewSetModel treeView (Just store)

    firstRulePath <- Gtk.treePathNewFromIndices [0]
    Gtk.treeViewSetCursor treeView firstRulePath Gtk.noTreeViewColumn False

    -- IORefs -------------------------------------------------------------------------------------------------------------------
    hostState   <- newIORef emptyState -- state refering to the init graph with rules applied
    ruleState   <- newIORef emptyState -- state refering to the graph of the selected rule
    lState      <- newIORef emptyState -- state refering to the graph of the left side of selected rule
    rState      <- newIORef emptyState -- state refering to the graph of the right side of selected rule
    kGraph      <- newIORef G.empty -- k graph needed for displaying ids in the left and right sides of rules

    lStateOrig  <- newIORef emptyState -- similar to lState, but not moved
    nacState    <- newIORef emptyState -- state refering to the graph of nac
    
    mergeMap    <- newIORef (Nothing :: Maybe MergeMapping)
    
    currentNACIndex    <- newIORef (-1 :: Int32) -- index of current selected NAC
    currentRuleIndex   <- newIORef (-1 :: Int32) -- index of currentRule

    execStarted  <- newIORef False   -- if execution has already started

    nacListMap <- newIORef (M.empty :: M.Map Int32 [(String,Int32)])

    productionMap <- newIORef (M.empty :: M.Map Int32 (DPO.Production (TGM.TypedGraphMorphism Info Info)))
    matchesMap <- newIORef (M.empty :: M.Map Int32 (M.Map Int32 (TGM.TypedGraphMorphism Info Info)))

    -- callbacks ----------------------------------------------------------------------------------------------------------------
    -- hide rule viewer panel when colse button is pressed
    on hideRVBtn #pressed $ do
        closePos <- get execPane #maxPosition
        Gtk.panedSetPosition execPane closePos

    -- canvas
    setCanvasCallBacks mainCanvas hostState typeGraph (Just drawHostGraph) focusedCanvas focusedStateIORef
    setCanvasCallBacks ruleCanvas ruleState typeGraph (Just drawRuleGraph) focusedCanvas focusedStateIORef
    setCanvasCallBacks lCanvas lState kGraph (Just drawRuleSideGraph) focusedCanvas focusedStateIORef
    setCanvasCallBacks rCanvas rState kGraph (Just drawRuleSideGraph) focusedCanvas focusedStateIORef
    setCanvasCallBacks rCanvas rState kGraph (Just drawRuleSideGraph) focusedCanvas focusedStateIORef
    (_,nacSqrSel)<- setCanvasCallBacks nacCanvas nacState typeGraph Nothing focusedCanvas focusedStateIORef
    on nacCanvas #draw $ \context -> do
        es <- readIORef nacState
        tg <- readIORef typeGraph
        sq <- readIORef nacSqrSel
        index <- readIORef currentNACIndex
        mm <- readIORef mergeMap >>= return . fromMaybe (M.empty,M.empty)
        renderWithContext context $ drawNACGraph es sq tg mm
        return False
    
    -- when select a rule, change their states
    on treeView #cursorChanged $ do
        selection <- Gtk.treeViewGetSelection treeView
        (sel,model,iter) <- Gtk.treeSelectionGetSelected selection
        if sel
            then do
                t <- Gtk.treeModelGetValue model iter 2 >>= fromGValue :: IO Int32
                rIndex <- case t of
                    1 -> Gtk.treeModelGetValue model iter 1 >>= fromGValue :: IO Int32
                    2 -> Gtk.treeModelGetValue model iter 3 >>= fromGValue :: IO Int32
                statesM <- readIORef statesMap

                -- TODO: select which match is to be applied.            
                
                --load rule
                let es = fromMaybe emptyState $ M.lookup rIndex statesM
                    g = stateGetGraph es
                    (l,k,r) = GMker.graphToRuleGraphs g
                    (ngiM,egiM) = stateGetGI es
                currRIndex <- readIORef currentRuleIndex
                if (currRIndex == rIndex)
                    then return ()
                    else do
                        let lngiM = M.filterWithKey (\k a -> (G.NodeId k) `elem` (G.nodeIds l)) ngiM
                            legiM = M.filterWithKey (\k a -> (G.EdgeId k) `elem` (G.edgeIds l)) egiM
                            rngiM = M.filterWithKey (\k a -> (G.NodeId k) `elem` (G.nodeIds r)) ngiM
                            regiM = M.filterWithKey (\k a -> (G.EdgeId k) `elem` (G.edgeIds r)) egiM
                            (_,lgi) = adjustDiagrPosition (l,(lngiM,legiM))
                            (_,rgi) = adjustDiagrPosition (r,(rngiM,regiM))
                            (_,gi') = adjustDiagrPosition (g,(ngiM,egiM))
                        writeIORef ruleState $ stateSetGI gi' es
                        writeIORef lState $ stateSetGraph l . stateSetGI lgi $ es
                        writeIORef rState $ stateSetGraph r . stateSetGI rgi $ es
                        writeIORef lStateOrig $ stateSetGraph l . stateSetGI (lngiM,legiM) $ es
                        writeIORef kGraph k                    
                        writeIORef currentRuleIndex rIndex

                nacListM <- readIORef nacListMap
                let nacList = M.lookup rIndex nacListM
                
                Gtk.comboBoxTextRemoveAll nacCBox
                forM_ (fromMaybe [] nacList) $ \(str,index) -> Gtk.comboBoxTextAppendText nacCBox (T.pack str)
                Gtk.comboBoxSetActive nacCBox 0

                Gtk.widgetQueueDraw lCanvas
                Gtk.widgetQueueDraw rCanvas
                Gtk.widgetQueueDraw ruleCanvas
                Gtk.widgetQueueDraw nacCanvas
            else return ()

    on nacCBox #changed $ do
        index <- Gtk.comboBoxGetActive nacCBox
        if index == (-1)
            then writeIORef nacState $ emptyState
            else do
                nacName <- Gtk.comboBoxTextGetActiveText nacCBox >>= return . T.unpack
                ruleIndex <- readIORef currentRuleIndex
                nacListM <- readIORef nacListMap
                case M.lookup ruleIndex nacListM of
                    Nothing -> writeIORef nacState $ emptyState
                    Just nacList -> case lookup nacName nacList of
                        Nothing -> writeIORef nacState $ emptyState
                        Just nacIndex -> do
                            les <- readIORef lStateOrig
                            let l = stateGetGraph les
                                lgi = stateGetGI les
                            nacInfoM <- readIORef nacInfoMap
                            (n,ngi) <- case M.lookup nacIndex nacInfoM of
                                Nothing -> return (l,lgi)
                                Just nacInfo -> do
                                    context <- Gtk.widgetGetPangoContext nacCanvas
                                    tg <- readIORef typeGraph
                                    (nacdg',mergeM') <- Nac.applyLhsChangesToNac l nacInfo (Just context)
                                    writeIORef mergeMap $ Just mergeM'
                                    return $ Nac.mountNACGraph (l,lgi) tg (nacdg',mergeM')
                            let (_,ngi') = adjustDiagrPosition (n,ngi)
                                nes = stateSetGI ngi' . stateSetGraph n $ emptyState
                            writeIORef nacState nes
                            writeIORef currentNACIndex nacIndex

    -- execution controls
    on stopBtn #pressed $ do
        statesM <- readIORef statesMap
        let initState = fromMaybe emptyState $ M.lookup 1 statesM
        writeIORef hostState initState
        writeIORef execStarted False
        findMatches store statesMap hostState typeGraph nacInfoMap nacListMap matchesMap productionMap
        
    on stepBtn #pressed $ do
        -- test: take first match
        matchesM <- readIORef matchesMap
        prodMap  <- readIORef productionMap
        -- Map ruleId (Map matchId match)
        -- [(ruleId,[(matchId,match)])]
        let ( (rid,(_,m):_):_ ) = M.toList $ M.map M.toList matchesM
            p = fromJust $ M.lookup rid prodMap
        
        -- apply match to get mapping between graphs
        let (k,n,f,g) = DPO.calculateDPO m p        
            fMapping = TGM.mapping f
            gMapping = TGM.mapping g
            nMapping = TGM.mapping n
            
            fNodeRelation = R.inverseRelation $ GM.nodeRelation fMapping -- G --fn'--> D
            fEdgeRelation = R.inverseRelation $ GM.edgeRelation fMapping -- G --fe'--> D
            gNodeRelation = GM.nodeRelation gMapping                     -- D --gn--> H
            gEdgeRelation = GM.edgeRelation gMapping                     -- D --ge--> H
            nNodeRelation = R.inverseRelation $ GM.nodeRelation nMapping -- H --nn'--> R
            nEdgeRelation = R.inverseRelation $ GM.edgeRelation nMapping -- H --ne'--> R

        let apply rel k def = case R.apply rel k of [] -> def; id:_ -> id
            replaceInfo (k,i) m = case lookup k m of Nothing -> i; Just i' -> i'


        -- make sure the mapped elements preserve the information between transformations
        let gGraph = GM.domainGraph fMapping
            dGraph = GM.codomainGraph fMapping
            hGraph = GM.codomainGraph gMapping

            gNodesInfo = map (\(k,n) -> (apply fNodeRelation k (G.NodeId (-1)), G.nodeInfo n)) (G.nodeMap gGraph)
            gEdgesInfo = map (\(k,e) -> (apply fEdgeRelation k (G.EdgeId (-1)), G.edgeInfo e)) (G.edgeMap gGraph)
            
            dNodesInfo = map (\(k,n) -> (k, G.nodeInfo n)) (G.nodeMap dGraph)
            dEdgesInfo = map (\(k,e) -> (k, G.edgeInfo e)) (G.edgeMap dGraph)
            dNodesInfo' = map (\(k,ni) -> (apply gNodeRelation k (G.NodeId (-1)), replaceInfo (k,ni) gNodesInfo) ) dNodesInfo
            dEdgesInfo' = map (\(k,ei) -> (apply gEdgeRelation k (G.EdgeId (-1)), replaceInfo (k,ei) gEdgesInfo) ) dEdgesInfo

            hNodesInfo = map (\(k,n) -> (k, replaceInfo (k, G.nodeInfo n) dNodesInfo')) (G.nodeMap hGraph)
            hEdgesInfo = map (\(k,e) -> (k, replaceInfo (k, G.edgeInfo e) dEdgesInfo')) (G.edgeMap hGraph)
            
            hGraph' = foldr (\(k,i) g -> G.updateNodePayload k g (\ni -> i)) hGraph hNodesInfo
            hGraph'' = foldr (\(k,i) g -> G.updateEdgePayload k g (\ei -> i)) hGraph' hEdgesInfo
            hNodeMap' = map (\(k,n) -> (k, GMker.nodeFromJust n)) (G.nodeMap hGraph'')
            hEdgeMap' = map (\(k,e) -> (k, GMker.edgeFromJust e)) (G.edgeMap hGraph'')
            finalNodeMap = map (\(k,n) -> (k, n {G.nodeInfo = infoSetOperation (G.nodeInfo n) Preserve})) hNodeMap'
            finalEdgeMap = map (\(k,e) -> (k, e {G.edgeInfo = infoSetOperation (G.edgeInfo e) Preserve})) hEdgeMap'
            finalGraph = G.Graph finalNodeMap finalEdgeMap

        -- modify graphical information to match the modifications
        hostSt <- readIORef hostState
        statesM <- readIORef statesMap
        let (sgiN,sgiE) = stateGetGI hostSt
            -- delete layouts of elements that are not in the f morphism (G --f--> D)
            sgiN' = M.mapKeys G.NodeId $ M.filterWithKey (\k _ -> G.NodeId k `elem` (R.domain fNodeRelation)) sgiN
            sgiE' = M.mapKeys G.EdgeId $ M.filterWithKey (\k _ -> G.EdgeId k `elem` (R.domain fEdgeRelation)) sgiE
            -- modify the ids of the elements that are in the f morphism (G --f--> D)
            dgiN  = M.filterWithKey (\k _ -> fromEnum k > 0) $ M.mapKeys (\k -> apply fNodeRelation k (G.NodeId (-1))) sgiN'
            dgiE  = M.filterWithKey (\k _ -> fromEnum k > 0) $ M.mapKeys (\k -> apply fEdgeRelation k (G.EdgeId (-1))) sgiE'
            -- modify the ids of the elements that are in the g morphism (D --g--> H)
            dgiN' = M.filterWithKey (\k _ -> fromEnum k > 0) $ M.mapKeys (\k -> apply gNodeRelation k (G.NodeId (-1))) dgiN
            dgiE' = M.filterWithKey (\k _ -> fromEnum k > 0) $ M.mapKeys (\k -> apply gEdgeRelation k (G.EdgeId (-1))) dgiE
            -- add layouts to the elements of codomain of g morphism that have no relation to elements of it's domain
            
            rSt = fromJust $ M.lookup rid statesM
            (rgiN,rgiE) = stateGetGI rSt
            addedNodeIds = filter (\id -> id `notElem` (M.keys dgiN')) (R.domain nNodeRelation)
            addedEdgeIds = filter (\id -> id `notElem` (M.keys dgiE')) (R.domain nEdgeRelation)
            addedNodeIds' = filter (\(_,kr) -> fromEnum kr > 0) $ map (\k -> (k, apply nNodeRelation k (-1))) addedNodeIds
            addedEdgeIds' = filter (\(_,kr) -> fromEnum kr > 0) $ map (\k -> (k, apply nEdgeRelation k (-1))) addedEdgeIds
            addedNodeGIs = map (\(k,kr) -> (k,fromJust $ M.lookup (fromEnum kr) rgiN)) addedNodeIds'
            addedEdgeGIs = map (\(k,kr) -> (k,fromJust $ M.lookup (fromEnum kr) rgiE)) addedEdgeIds'

            {- TODO: set position of graphical informations avoiding overlapping
                Nodes should have a position relative to another node
                Edges positions are relative to to quantity of edges that are already there
            -} 

            hgiN = M.mapKeys fromEnum $ foldr (\(k,gi) m -> M.insert k gi m) dgiN' addedNodeGIs
            hgiE = M.mapKeys fromEnum $ foldr (\(k,gi) m -> M.insert k gi m) dgiE' addedEdgeGIs
            hState = stateSetGraph finalGraph . stateSetGI (hgiN,hgiE) $ hostSt

        writeIORef hostState hState
        Gtk.widgetQueueDraw mainCanvas
        findMatches store statesMap hostState typeGraph nacInfoMap nacListMap matchesMap productionMap

            
        

    
    #show executorPane
    return (executorPane, mainCanvas, nacCBox, hostState, execStarted, nacListMap)

type SquareSelection = Maybe (Double,Double,Double,Double)
setCanvasCallBacks :: Gtk.DrawingArea 
                   -> IORef GraphState 
                   -> IORef (G.Graph Info Info )
                   -> Maybe (GraphState -> SquareSelection -> G.Graph Info Info -> Render ())
                   -> IORef (Maybe Gtk.DrawingArea) -> IORef (Maybe (IORef GraphState))
                   -> IO (IORef (Double,Double), IORef SquareSelection)
setCanvasCallBacks canvas state refGraph drawMethod focusedCanvas focusedStateIORef = do
    oldPoint        <- newIORef (0.0,0.0) -- last point where a mouse button was pressed
    squareSelection <- newIORef Nothing   -- selection box : Maybe (x1,y1,x2,y2)
    case drawMethod of
        Just draw -> do 
            on canvas #draw $ \context -> do
                es <- readIORef state
                rg <- readIORef refGraph
                sq <- readIORef squareSelection
                renderWithContext context $ draw es sq rg
                return False
            return ()
        Nothing -> return ()
    on canvas #buttonPressEvent $ Editor.basicCanvasButtonPressedCallback state oldPoint squareSelection canvas
    on canvas #motionNotifyEvent $ Editor.basicCanvasMotionCallBack state oldPoint squareSelection canvas
    on canvas #buttonReleaseEvent $ Editor.basicCanvasButtonReleasedCallback state squareSelection canvas
    on canvas #scrollEvent $ Editor.basicCanvasScrollCallback state canvas
    on canvas #focusInEvent $ \event -> do
        writeIORef focusedCanvas $ Just canvas
        writeIORef focusedStateIORef $ Just state
        return False
    return (oldPoint,squareSelection) 


findMatches :: Gtk.TreeStore
            -> IORef (M.Map Int32 GraphState)
            -> IORef GraphState 
            -> IORef (G.Graph Info Info) 
            -> IORef (M.Map Int32 NacInfo)
            -> IORef (M.Map Int32 [(String,Int32)])
            -> IORef (M.Map Int32 (M.Map Int32 (TGM.TypedGraphMorphism Info Info)))
            -> IORef (M.Map Int32 (DPO.Production (TGM.TypedGraphMorphism Info Info)))
            -> IO ()
findMatches store statesMap hostState typeGraph nacInfoMap nacListMap matchesMap productionMap = do
        -- get matches of each rule in the initial graph
        -- prepare initial graph
        hState <- readIORef hostState
        typeG <- readIORef typeGraph
        let g = stateGetGraph hState
            tg = GMker.makeTypeGraph typeG
            obj = GMker.makeTypedGraph g tg
            
        -- get rules from treeStore
        nacListM <- readIORef nacListMap
        rulesStates <- treeStoreGetRules store statesMap
        productions <- forM rulesStates $ \(id,st) -> do
                            let nacList = fromMaybe [] $ M.lookup id nacListM
                            nacInfoM <- readIORef nacInfoMap
                            let getNac index = Just (\(dg,mm) -> (fst dg, mm)) <*> (M.lookup index nacInfoM)
                                mnacs = filter (not . null) $ map (\(_,index) -> getNac index) nacList
                                nacs = map fromJust mnacs
                            let rg = stateGetGraph st
                            return $ (id,GMker.graphToRule rg nacs tg)
        writeIORef productionMap (M.fromList productions)

        -- get dpo matches
        let morphismClass = Cat.monic :: Cat.MorphismClass (TGM.TypedGraphMorphism Info Info)
            conf = DPO.MorphismsConfig morphismClass
            matches = map (\(id,prod) -> (id,DPO.findApplicableMatches conf prod obj)) productions

        let matchesM = foldr (\(rid,l) m -> M.insert rid (M.fromList $ zip ([1,2..] :: [Int32]) l) m) M.empty matches
        writeIORef matchesMap matchesM

        -- foreach match, generate a entry
        let matchesEntries = concat $
                                map (\(rid,mM) -> 
                                    map (\(mid,m) -> ("match " ++ (show mid), mid, 2 :: Int32, rid) ) $ M.toList mM) $
                                M.toList matchesM
        
        forM_ matchesEntries $ \entry -> updateTreeStore store entry


{-| ExecGraphEntry
    A tuple representing what is showed in each node of the tree in the treeview
    It contains the informations:
    * name,
    * id,
    * type (1 - rule, 2 - rule match),
    * parent id (used if type is NAC).
    * active
-}
type ExecGraphEntry = (String, Int32, Int32, Int32)

-- | set the ExecGraphStore in a position given by an iter in the TreeStore
storeSetGraphEntry :: Gtk.TreeStore -> Gtk.TreeIter -> ExecGraphEntry -> IO ()
storeSetGraphEntry store iter (n,i,t,p) = do
    gvn <- toGValue (Just n)
    gvi <- toGValue i
    gvt <- toGValue t
    gvp <- toGValue p
    #set store iter [0,1,2,3] [gvn,gvi,gvt,gvp]

-- search the treeStore for an entry. If the entry is found then update it, else create the entry.
updateTreeStore :: Gtk.TreeStore -> ExecGraphEntry -> IO ()
updateTreeStore store entry = do
    (valid,iter) <- Gtk.treeModelGetIterFirst store
    if valid
        then updateTreeStore' store iter entry
        else do
            iter <- Gtk.treeStoreAppend store Nothing
            storeSetGraphEntry store iter entry

updateTreeStore' :: Gtk.TreeStore -> Gtk.TreeIter -> ExecGraphEntry -> IO ()
updateTreeStore' store iter entry@(n,i,t,p) = do
    cid <- Gtk.treeModelGetValue store iter 1 >>= fromGValue :: IO Int32
    ct  <- Gtk.treeModelGetValue store iter 2 >>= fromGValue :: IO Int32
    if ct == t && cid == i
        then storeSetGraphEntry store iter entry
        else case t of
            1 -> updateInList Nothing
            2 -> case (ct==1,cid==p) of
                (True,True) -> do
                    (valid,childIter) <- Gtk.treeModelIterChildren store (Just iter)
                    if valid
                        then updateTreeStore' store childIter entry
                        else do
                            newIter <- Gtk.treeStoreAppend store (Just iter)
                            storeSetGraphEntry store newIter entry
                (True,False) -> do
                    valid <- Gtk.treeModelIterNext store iter
                    if valid
                        then updateTreeStore' store iter entry
                        else return ()
                _ -> do
                    (valid,parentIter) <- Gtk.treeModelIterParent store iter
                    if valid
                        then updateInList (Just parentIter)
                        else return ()
            _ -> return ()

    where
        updateInList parentIter = do
            valid <- Gtk.treeModelIterNext store iter
            if valid
                then updateTreeStore' store iter entry
                else do
                    newIter <- Gtk.treeStoreAppend store parentIter
                    storeSetGraphEntry store newIter entry
        


removeFromTreeStore :: Gtk.TreeStore -> Int32 -> IO ()
removeFromTreeStore store index = do
    (valid, iter) <- Gtk.treeModelGetIterFirst store
    if valid
        then removeFromTreeStore' store iter index
        else return ()

removeFromTreeStore' :: Gtk.TreeStore -> Gtk.TreeIter -> Int32 -> IO ()
removeFromTreeStore' store iter index = do
    cindex <- Gtk.treeModelGetValue store iter 1 >>= fromGValue :: IO Int32
    if cindex == index
        then do 
            Gtk.treeStoreRemove store iter
            return ()
        else do 
            continue <- Gtk.treeModelIterNext store iter
            if continue 
                then removeFromTreeStore' store iter index
                else return ()



-- remove entries that contains invalid keys for the Map that it refers to
removeTrashFromTreeStore :: Gtk.TreeStore -> M.Map Int32 GraphState -> IO ()
removeTrashFromTreeStore store statesMap = do
    (valid, iter) <- Gtk.treeModelGetIterFirst store
    if valid
        then removeTrashFromTreeStore' store iter statesMap
        else return ()

removeTrashFromTreeStore' :: Gtk.TreeStore -> Gtk.TreeIter -> M.Map Int32 GraphState -> IO ()
removeTrashFromTreeStore' store iter statesMap = do
    index <- Gtk.treeModelGetValue store iter 1 >>= fromGValue :: IO Int32
    state <- return $ M.lookup index statesMap
    continue <- case state of
        Just st -> Gtk.treeModelIterNext store iter                    
        Nothing -> Gtk.treeStoreRemove store iter
    if continue
        then removeTrashFromTreeStore' store iter statesMap
        else return ()

-- get the GraphStates that are referenciated by the treeStore    
treeStoreGetRules :: Gtk.TreeStore -> IORef (M.Map Int32 GraphState) -> IO [(Int32,GraphState)]
treeStoreGetRules store statesMap = do
    statesM <- readIORef statesMap
    (valid,iter) <- Gtk.treeModelGetIterFirst store
    if valid
        then treeStoreGetRules' store iter statesM
        else return []

treeStoreGetRules' :: Gtk.TreeStore -> Gtk.TreeIter -> M.Map Int32 GraphState -> IO [(Int32,GraphState)]
treeStoreGetRules' store iter statesMap = do
    index <- Gtk.treeModelGetValue store iter 1 >>= fromGValue :: IO Int32
    let state = M.lookup index statesMap
    continue <- Gtk.treeModelIterNext store iter
    states <- case continue of
        True -> treeStoreGetRules' store iter statesMap
        False -> return []
    case state of
        Nothing -> return states
        Just st -> return $ (index,st) : states
    