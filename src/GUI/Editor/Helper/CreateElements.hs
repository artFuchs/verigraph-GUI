{-| This module contains auxiliar functions to canvasButtonPressedCallback
    that are related to the creation of nodes and edges
-}

{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module GUI.Editor.Helper.CreateElements where

import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import qualified GI.Pango as P
import           Data.GI.Base

import           Control.Monad
import           Data.IORef
import           Data.Int
import qualified Data.Maybe as Maybe
import qualified Data.Map as M
import           Data.Graphs hiding (empty)
import qualified Data.Graphs as G

import           GUI.Data.Diagram
import           GUI.Data.GraphState
import           GUI.Data.GraphicalInfo
import           GUI.Data.Info hiding (empty)
import qualified GUI.Data.Info as I
import           GUI.Data.Nac
import           GUI.Helper.GraphicalInfo

import           GUI.Editor.Helper.TypeInfer




-- create a node or n edges and return if something was created or not
createNodesOrEdgesCallback :: Gtk.DrawingArea -> Gtk.Entry -> Gtk.CheckButton -> Gtk.CheckButton
                           -> IORef GraphState -> IORef Int32 -> IORef Int32 -> IORef (Graph Info Info) -> IORef (Maybe MergeMapping)
                           -> IORef (Maybe String) -> IORef (M.Map String (NodeGI,Int32)) -> IORef (Maybe String) -> IORef (M.Map String (M.Map (String, String) EdgeGI, Int32))
                           -> IORef NodeShape -> IORef EdgeStyle -> IORef GIColor -> IORef GIColor
                           -> Gdk.EventButton
                           -> IO Bool
createNodesOrEdgesCallback canvas nameEntry autoLabelNCheckBtn autoLabelECheckBtn
                           currentState currentGraph currentGraphType typeGraph mergeMapping
                           currentNodeType possibleNodeTypes currentEdgeType possibleEdgeTypes
                           currentShape currentStyle currentC currentLC
                           eventButton  =
  do
    x <- get eventButton #x
    y <- get eventButton #y
    es <- readIORef currentState
    let z = stateGetZoom es
        (px,py) = stateGetPan es
        (x',y') = (x/z - px, y/z - py)
    -- add the current state to the undo stack
    mergeM <- readIORef mergeMapping
    let g = stateGetGraph es
        gi = stateGetGI es
        dstNode = selectNodeInPosition gi (x',y')
    context <- Gtk.widgetGetPangoContext canvas
    case (dstNode) of
      -- no selected node: create node
      Nothing -> do
        auto <- Gtk.toggleButtonGetActive autoLabelNCheckBtn
        createNodeCallback currentGraphType currentState currentNodeType possibleNodeTypes currentShape currentC currentLC (x',y') auto context
      -- one node selected: create edges targeting this node
      Just nid ->
        if length (fst $ stateGetSelected es) > 0 then do
          auto <- Gtk.toggleButtonGetActive autoLabelECheckBtn
          createEdgeCallback typeGraph currentGraphType currentState currentEdgeType possibleEdgeTypes currentStyle currentLC auto nid
        else do
          modifyIORef currentState $ stateSetSelected ([nid],[])
          return False


createNodeCallback :: IORef Int32
                   -> IORef GraphState
                   -> IORef (Maybe String)
                   -> IORef (M.Map String (NodeGI, Int32))
                   -> IORef NodeShape
                   -> IORef GIColor
                   -> IORef GIColor
                   -> GIPos
                   -> Bool
                   -> P.Context
                   -> IO Bool
createNodeCallback currentGraphType currentState currentNodeType possibleNodeTypes currentShape currentC currentLC (x,y) auto context =
  do
    gType <- readIORef currentGraphType
    case gType of
      0 -> return False
      1 -> do
        cShape <- readIORef currentShape
        cColor <- readIORef currentC
        cLColor <- readIORef currentLC
        createNode' currentState I.empty True (x,y) cShape cColor cLColor context
        return True
      _ -> do
        (t, shape, c, lc) <- getNodeLayoutPrimitives currentNodeType possibleNodeTypes currentShape currentC currentLC
        createNode' currentState (infoSetType I.empty t) auto (x,y) shape c lc context
        return True

-- create a new node, auto-generating it's name and dimensions
createNode' :: IORef GraphState -> Info -> Bool -> GIPos -> NodeShape -> GIColor -> GIColor -> P.Context ->  IO NodeId
createNode' currentState info autoNaming pos nshape color lcolor context = do
  es <- readIORef currentState
  let nid = head $ newNodes (stateGetGraph es)
      info' = if infoLabelStr info == "" && autoNaming then infoSetLabel info (show $ fromEnum nid) else info
  dim <- getStringDims (infoVisible info') context Nothing
  writeIORef currentState $ createNode es pos dim info' nshape color lcolor

  return nid

createEdgeCallback :: IORef (Graph Info Info) -> IORef Int32 -> IORef GraphState
                   -> IORef (Maybe String) -> IORef (M.Map String (M.Map (String, String) EdgeGI, Int32))
                   -> IORef EdgeStyle -> IORef GIColor
                   -> Bool
                   -> NodeId
                   -> IO Bool
createEdgeCallback typeGraph currentGraphType currentState currentEdgeType possibleEdgeTypes currentStyle currentLC auto nid =
  do
    gType <- readIORef currentGraphType
    case gType of
      0 -> return False
      1 -> do
        estyle <- readIORef currentStyle
        color <- readIORef currentLC
        modifyIORef currentState (\es -> createEdges es nid I.empty True estyle color)
        return True
      _ -> do
        tg <- readIORef typeGraph
        es <- readIORef currentState
        metype <- readIORef currentEdgeType

        -- create edges infering their types
        let sNids = fst $ stateGetSelected es
            g = stateGetGraph es
            srcNodes = Maybe.catMaybes $ map (\nid -> G.lookupNode nid g) sNids
            maybeTgtNode = G.lookupNode nid g

        case maybeTgtNode of
          Nothing ->
            return ()
          Just tgtNode ->
            do
              let tgtType = infoType $ nodeInfo tgtNode
                  edgesTs = map (\src -> (src, infereEdgeType tg src tgtNode metype)) srcNodes

              (es', createdEdges) <- foldM
                    (\(es,eids) (src, mt) ->
                        do
                          (t,estyle,color) <- getEdgeLayoutPrimitives mt (infoType $ nodeInfo src) tgtType possibleEdgeTypes currentStyle currentLC
                          let srcId = nodeId src
                              es' = createEdge es Nothing srcId nid (infoSetType I.empty t) auto estyle color
                              eids' = (snd $ stateGetSelected es') ++ eids
                          return (es',eids'))
                    (es,[])
                    edgesTs

              writeIORef currentState $ stateSetSelected ([],createdEdges) es'
        return True



-- auxiliar function that checks if the currentNodeType exist in possibleNodeTypes and then
-- returns the correspondent triple (type, style, color)
getNodeLayoutPrimitives :: IORef (Maybe String)
                        -> IORef (M.Map String (NodeGI, Int32))
                        -> IORef NodeShape
                        -> IORef GIColor
                        -> IORef GIColor
                        -> IO (String, NodeShape, GIColor, GIColor)
getNodeLayoutPrimitives currentNodeType possibleNodeTypes currentShape currentC currentLC =
      do
        cShape <- readIORef currentShape
        cColor <- readIORef currentC
        cLColor <- readIORef currentLC
        mntype <- readIORef currentNodeType
        case mntype of
          Nothing -> return ("", cShape, cColor, cLColor)
          Just t -> do
            possibleNT <- readIORef possibleNodeTypes
            let possibleNT' = M.map (\(gi,i) -> gi) possibleNT
                mngi = M.lookup t possibleNT'
            case mngi of
              Nothing -> return ("", cShape, cColor, cLColor)
              Just gi -> return (t, shape gi, fillColor gi, lineColor gi)


-- auxiliar function that checks if the type mt, exist in possibleEdgeTypes.
-- returns the correspondent triple (type, style, color)
getEdgeLayoutPrimitives :: Maybe String
                        -> String
                        -> String
                        -> IORef (M.Map String (M.Map (String, String) EdgeGI, Int32))
                        -> IORef EdgeStyle
                        -> IORef GIColor
                        -> IO (String,EdgeStyle,GIColor)
getEdgeLayoutPrimitives mt srcType tgtType possibleEdgeTypes currentStyle currentLC = do
        pet <- readIORef possibleEdgeTypes
        pet' <- return $ M.map fst pet
        cEstyle <- readIORef currentStyle
        cColor <- readIORef currentLC
        return $ case mt of
            Nothing -> ("", cEstyle, cColor)
            Just t -> let megi = M.lookup t pet'
                      in case megi of
                            Nothing -> ("", cEstyle, cColor)
                            Just sm -> case M.lookup (srcType,tgtType) sm of
                                          Nothing -> ("", cEstyle, cColor)
                                          Just gi -> (t, style gi, color gi)
