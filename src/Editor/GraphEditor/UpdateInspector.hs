{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module Editor.GraphEditor.UpdateInspector(
  updateTypeInspector
, updateHostInspector
, updateRuleInspector
, updateNacInspector
)where

import Data.GI.Base
import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import qualified GI.Pango as P

import Data.IORef
import Data.Int
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T


import Data.Graphs

import Editor.Data.EditorState
import Editor.Data.GraphicalInfo
import Editor.Data.Info1
import Editor.Data.Nac

-- update the inspector --------------------------------------------------------
updateTypeInspector :: IORef EditorState -> IORef (Double,Double,Double) -> IORef (Double,Double,Double) ->
                      (Gtk.Entry, Gtk.ColorButton, Gtk.ColorButton, [Gtk.RadioButton], [Gtk.RadioButton]) ->
                      (Gtk.Box, Gtk.Frame, Gtk.Frame)-> IO ()
updateTypeInspector st currentC currentLC (nameEntry, colorBtn, lcolorBtn, radioShapes, radioStyles) (hBoxColor, frameShape, frameStyle) = do
  emptyColor <- new Gdk.RGBA [#red := 0.5, #blue := 0.5, #green := 0.5, #alpha := 1.0]
  est <- readIORef st
  let g = editorGetGraph est
      ns = filter (\n -> elem (nodeId n) $ fst $ editorGetSelected est) $ nodes g
      es = filter (\e -> elem (edgeId e) $ snd $ editorGetSelected est) $ edges g
      (ngiM,egiM) = editorGetGI est
      unifyNames (x:xs) = if all (==x) xs then x else "----"
  case (length ns, length es) of
    (0,0) -> do
      (r, g, b)    <- readIORef currentC
      (r', g', b') <- readIORef currentLC
      set nameEntry [#text := ""]
      color <- new Gdk.RGBA [#red := r, #green := g, #blue := b, #alpha:=1.0]
      lcolor <- new Gdk.RGBA [#red := r', #green := g', #blue := b', #alpha:=1.0]
      Gtk.colorChooserSetRgba colorBtn color
      Gtk.colorChooserSetRgba lcolorBtn lcolor
      set hBoxColor [#visible := True]
      set frameShape [#visible := True]
      set frameStyle [#visible := True]
    (n,0) -> do
      let nid = nodeId (ns!!0)
          info = T.pack . unifyNames $ map (infoLabelStr . nodeInfo) ns
          gi = getNodeGI (fromEnum nid) ngiM
          (r,g,b) = fillColor gi
          (r',g',b') = lineColor gi
          nodeShape = shape gi
      color <- new Gdk.RGBA [#red := r, #green := g, #blue := b, #alpha := 1.0]
      lcolor <- new Gdk.RGBA [#red := r', #green := g', #blue := b', #alpha := 1.0]
      set nameEntry [#text := info]
      Gtk.colorChooserSetRgba colorBtn $ if n==1 then color else emptyColor
      Gtk.colorChooserSetRgba lcolorBtn $ if n==1 then lcolor else emptyColor
      case (n,nodeShape) of
        (1,NCircle) -> Gtk.toggleButtonSetActive (radioShapes!!0) True
        (1,NRect) -> Gtk.toggleButtonSetActive (radioShapes!!1) True
        (1,NSquare) -> Gtk.toggleButtonSetActive (radioShapes!!2) True
        _ -> return ()

      set hBoxColor [#visible := True]
      set frameShape [#visible := True]
      set frameStyle [#visible := False]
    (0,n) -> do
      let eid = edgeId (es!!0)
          info = T.pack . unifyNames $ map (infoLabelStr . edgeInfo) es
          gi = getEdgeGI (fromEnum eid) egiM
          (r,g,b) = color gi
          edgeStyle = style gi
      edgeColor <- new Gdk.RGBA [#red := r, #green := g, #blue := b, #alpha := 1.0]
      set nameEntry [#text := info]
      Gtk.colorChooserSetRgba lcolorBtn $ if n == 1 then edgeColor else emptyColor
      case (n,edgeStyle) of
        (1,ENormal) -> Gtk.toggleButtonSetActive (radioStyles!!0) True
        (1,EPointed) -> Gtk.toggleButtonSetActive (radioStyles!!1) True
        (1,ESlashed) -> Gtk.toggleButtonSetActive (radioStyles!!2) True
        _ -> return ()

      set hBoxColor [#visible := False]
      set frameShape [#visible := False]
      set frameStyle [#visible := True]
    _ -> do
      let info = T.pack . unifyNames $ concat [(map (infoLabelStr . edgeInfo) es), (map (infoLabelStr . nodeInfo) ns)]
      set nameEntry [#text := info ]
      Gtk.colorChooserSetRgba colorBtn emptyColor
      Gtk.colorChooserSetRgba lcolorBtn emptyColor
      set hBoxColor [#visible := True]
      set frameShape [#visible := True]
      set frameStyle [#visible := True]

updateHostInspector :: IORef EditorState -> IORef (M.Map String (NodeGI, Int32)) -> IORef (M.Map String (EdgeGI, Int32)) ->
                       IORef (Maybe String) -> IORef (Maybe String) -> (Gtk.Entry, Gtk.ComboBoxText, Gtk.ComboBoxText) ->
                       (Gtk.Box, Gtk.Box) -> IO()
updateHostInspector st possibleNT possibleET currentNodeType currentEdgeType (entry, nodeTCBox, edgeTCBox) (nodeTBox, edgeTBox) = do
  est <- readIORef st
  pNT <- readIORef possibleNT
  pET <- readIORef possibleET
  cNT <- readIORef currentNodeType >>= \x -> return $ fromMaybe "" x
  cET <- readIORef currentEdgeType >>= \x -> return $ fromMaybe "" x
  let g = editorGetGraph est
      ns = filter (\n -> elem (nodeId n) $ fst $ editorGetSelected est) $ nodes g
      es = filter (\e -> elem (edgeId e) $ snd $ editorGetSelected est) $ edges g
      (ngiM,egiM) = editorGetGI est
      unifyNames (x:xs) = if all (==x) xs then x else ""
  case (length ns, length es) of
    (0,0) -> do
      Gtk.comboBoxSetActive nodeTCBox $ fromMaybe (-1) (Just snd <*> (M.lookup cNT pNT))
      Gtk.comboBoxSetActive edgeTCBox $ fromMaybe (-1) (Just snd <*> (M.lookup cET pET))
      set nodeTBox [#visible := True]
      set edgeTBox [#visible := True]
    (n,0) -> do
      let typeL = unifyNames $ map (infoType . nodeInfo) ns
          typeI = case M.lookup typeL pNT of
                  Nothing -> -1
                  Just (gi,i) -> i
      Gtk.comboBoxSetActive nodeTCBox typeI
      set nodeTBox [#visible := True]
      set edgeTBox [#visible := False]
    (0,e) -> do
      let typeL = unifyNames $ map (infoType . edgeInfo) es
          typeI = case M.lookup typeL pET of
                  Nothing -> -1
                  Just (gi,i) -> i

      Gtk.comboBoxSetActive edgeTCBox typeI
      set edgeTBox [#visible := True]
      set nodeTBox [#visible := False]
    (n,e) -> do
      let typeNL = unifyNames $ map (infoType . nodeInfo) ns
          typeNI = case M.lookup typeNL pNT of
                  Nothing -> -1
                  Just (gi,i) -> i
          typeEL = unifyNames $ map (infoType . edgeInfo) es
          typeEI = case M.lookup typeEL pET of
                  Nothing -> -1
                  Just (gi,i) -> i
      Gtk.comboBoxSetActive nodeTCBox typeNI
      Gtk.comboBoxSetActive edgeTCBox typeEI
      set edgeTBox [#visible := True]
      set nodeTBox [#visible := True]

updateRuleInspector :: IORef EditorState -> IORef (M.Map String (NodeGI, Int32)) -> IORef (M.Map String (EdgeGI, Int32)) ->
                       IORef (Maybe String) -> IORef (Maybe String) -> (Gtk.Entry, Gtk.ComboBoxText, Gtk.ComboBoxText, Gtk.ComboBoxText) ->
                       (Gtk.Box, Gtk.Box) -> IO()
updateRuleInspector st possibleNT possibleET currentNodeType currentEdgeType (entry, nodeTCBox, edgeTCBox, operationCBox) (nodeTBox, edgeTBox) = do
  est <- readIORef st
  pNT <- readIORef possibleNT
  pET <- readIORef possibleET
  cNT <- readIORef currentNodeType >>= \x -> return $ fromMaybe "" x
  cET <- readIORef currentEdgeType >>= \x -> return $ fromMaybe "" x
  let g = editorGetGraph est
      ns = filter (\n -> elem (nodeId n) $ fst $ editorGetSelected est) $ nodes g
      es = filter (\e -> elem (edgeId e) $ snd $ editorGetSelected est) $ edges g
      (ngiM,egiM) = editorGetGI est
      unifyNames [] = ""
      unifyNames (x:xs) = if all (==x) xs then x else "------"
      typeNL = unifyNames $ map (infoType . nodeInfo) ns
      typeEL = unifyNames $ map (infoType . edgeInfo) es
      operation = unifyNames $ concat [map (infoOperationStr . edgeInfo) es, map (infoOperationStr . nodeInfo) ns]
      typeNI = case M.lookup typeNL pNT of
              Nothing -> -1
              Just (gi,i) -> i
      typeEI = case M.lookup typeEL pET of
              Nothing -> -1
              Just (gi,i) -> i
      opI = case operation of
        "" -> 0
        "new:" -> 1
        "del:" -> 2
        _ -> -1
  case (length ns, length es) of
    (0,0) -> do
      Gtk.comboBoxSetActive nodeTCBox $ fromMaybe (-1) (Just snd <*> (M.lookup cNT pNT))
      Gtk.comboBoxSetActive edgeTCBox $ fromMaybe (-1) (Just snd <*> (M.lookup cET pET))
      Gtk.comboBoxSetActive operationCBox opI
      set nodeTBox [#visible := True]
      set edgeTBox [#visible := True]
    (n,0) -> do
      Gtk.comboBoxSetActive nodeTCBox typeNI
      Gtk.comboBoxSetActive operationCBox opI
      set nodeTBox [#visible := True]
      set edgeTBox [#visible := False]
    (0,e) -> do
      Gtk.comboBoxSetActive edgeTCBox typeEI
      Gtk.comboBoxSetActive operationCBox opI
      set edgeTBox [#visible := True]
      set nodeTBox [#visible := False]
    (n,e) -> do
      Gtk.comboBoxSetActive nodeTCBox typeNI
      Gtk.comboBoxSetActive edgeTCBox typeEI
      Gtk.comboBoxSetActive operationCBox opI
      set edgeTBox [#visible := True]
      set nodeTBox [#visible := True]

updateNacInspector :: IORef EditorState -> IORef (M.Map String (NodeGI, Int32)) -> IORef (M.Map String (EdgeGI, Int32)) 
                    -> IORef (Maybe String) -> IORef (Maybe String) -> IORef (Maybe MergeMapping)
                    -> (Gtk.Entry, Gtk.ComboBoxText, Gtk.ComboBoxText, Gtk.Button, Gtk.Button) 
                    -> (Gtk.Box, Gtk.Box) -> IO()
updateNacInspector st possibleNT possibleET currentNodeType currentEdgeType mergeMappingIORef (entry, nodeTCBox, edgeTCBox, joinBtn, splitBtn) (nodeTBox, edgeTBox) = do
  updateHostInspector st possibleNT possibleET currentNodeType currentEdgeType (entry, nodeTCBox, edgeTCBox) (nodeTBox, edgeTBox)
  est <- readIORef st
  mm <- readIORef mergeMappingIORef 
  let (nM,eM) = case mm of 
                      Nothing -> (M.empty, M.empty) 
                      Just m -> m
  let g = editorGetGraph est
      (snodes,sedges) = editorGetSelected est
      nodesFromLHS = filter (\n -> nodeId n `elem` snodes && (infoLocked $ nodeInfo n)) $ nodes g
      edgesFromLHS = filter (\e -> edgeId e `elem` sedges && (infoLocked $ edgeInfo e)) $ edges g
      mergeableNodes = filter (\n -> (infoType $ nodeInfo n) == (infoType $ nodeInfo $ head nodesFromLHS)) nodesFromLHS
      mergeableEdges = filter (\e -> (infoType $ edgeInfo e) == (infoType $ edgeInfo $ head edgesFromLHS)) edgesFromLHS
      splittableNids = M.filterWithKey (\k n -> k /= n) nM
      splittableEids = M.filterWithKey (\k n -> k /= n) eM
  case (length mergeableNodes < 2, length mergeableEdges < 2) of
    (True,True) -> Gtk.widgetSetSensitive joinBtn False
    (_,_) -> Gtk.widgetSetSensitive joinBtn True
  case (M.null splittableNids, M.null splittableEids) of
    (True, True) -> Gtk.widgetSetSensitive splitBtn False
    (_,_) -> Gtk.widgetSetSensitive splitBtn True

