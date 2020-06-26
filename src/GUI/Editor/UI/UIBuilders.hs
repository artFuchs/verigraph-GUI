{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

-- | This module contains the UI definition
module GUI.Editor.UI.UIBuilders(
  buildEditor
, buildTreePanel
) where

import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk

import Control.Monad.IO.Class
import Data.Maybe
import Data.GI.Base
import qualified Data.Text as T
import Data.GI.Base.ManagedPtr (unsafeCastTo)       
                     
buildEditor :: IO (Gtk.Paned, Gtk.Frame, Gtk.DrawingArea, Gtk.Entry, Gtk.Label
                  , ( Gtk.Box, Gtk.Box, Gtk.ColorButton, Gtk.Box, Gtk.ColorButton
                    , Gtk.Frame, [Gtk.RadioButton], Gtk.Frame, [Gtk.RadioButton])
                  , ( Gtk.Box, Gtk.CheckButton, Gtk.CheckButton
                      , Gtk.Box, Gtk.ComboBoxText, Gtk.Box, Gtk.ComboBoxText
                      , Gtk.Box, Gtk.ComboBoxText
                      , Gtk.Button, Gtk.Button))
buildEditor = do
  builder <- new Gtk.Builder []
  Gtk.builderAddFromFile builder "./Resources/editor.glade"

  mainPane <- Gtk.builderGetObject builder "mainPane" >>= unsafeCastTo Gtk.Paned . fromJust
  
  -- frame to put the treeView
  treeFrame <- Gtk.builderGetObject builder "treeFrame" >>= unsafeCastTo Gtk.Frame . fromJust

  -- canvas
  canvas <- Gtk.builderGetObject builder "canvas" >>= unsafeCastTo Gtk.DrawingArea . fromJust
  Gtk.widgetSetEvents canvas [toEnum $ fromEnum Gdk.EventMaskAllEventsMask - fromEnum Gdk.EventMaskSmoothScrollMask]
  
  -- inspector
  entry <- Gtk.builderGetObject builder "entry" >>= unsafeCastTo Gtk.Entry . fromJust
  entryLabel <- Gtk.builderGetObject builder "entryLabel" >>= unsafeCastTo Gtk.Label . fromJust

  layoutBox <- Gtk.builderGetObject builder "layoutBox" >>= unsafeCastTo Gtk.Box . fromJust
  fillColorBox <- Gtk.builderGetObject builder "fillColorBox" >>= unsafeCastTo Gtk.Box . fromJust
  fillColorBtn <- Gtk.builderGetObject builder "fillColorBtn" >>= unsafeCastTo Gtk.ColorButton . fromJust
  lineColorBox <- Gtk.builderGetObject builder "lineColorBox" >>= unsafeCastTo Gtk.Box . fromJust
  lineColorBtn <- Gtk.builderGetObject builder "lineColorBtn" >>= unsafeCastTo Gtk.ColorButton . fromJust
  nodeShapeFrame <- Gtk.builderGetObject builder "nodeShapeFrame" >>= unsafeCastTo Gtk.Frame . fromJust
  circleRadioBtn <- Gtk.builderGetObject builder "circleRadioBtn" >>= unsafeCastTo Gtk.RadioButton . fromJust
  rectRadioBtn <- Gtk.builderGetObject builder "rectRadioBtn" >>= unsafeCastTo Gtk.RadioButton . fromJust
  squareRadioBtn <- Gtk.builderGetObject builder "squareRadioBtn" >>= unsafeCastTo Gtk.RadioButton . fromJust
  edgeStyleFrame <- Gtk.builderGetObject builder "edgeStyleFrame" >>= unsafeCastTo Gtk.Frame . fromJust
  normalRadioBtn <- Gtk.builderGetObject builder "normalRadioBtn" >>= unsafeCastTo Gtk.RadioButton . fromJust
  slashedRadioBtn <- Gtk.builderGetObject builder "slashedRadioBtn" >>= unsafeCastTo Gtk.RadioButton . fromJust
  pointedRadioBtn <- Gtk.builderGetObject builder "pointedRadioBtn" >>= unsafeCastTo Gtk.RadioButton . fromJust
  let layoutWidgets = ( layoutBox
                      , fillColorBox, fillColorBtn
                      , lineColorBox, lineColorBtn
                      , nodeShapeFrame, [circleRadioBtn, rectRadioBtn, squareRadioBtn]
                      , edgeStyleFrame, [normalRadioBtn, slashedRadioBtn, pointedRadioBtn]
                      )

  typeSelectionBox <- Gtk.builderGetObject builder "typeSelectionBox" >>= unsafeCastTo Gtk.Box . fromJust
  nodeLabCheckBtn <- Gtk.builderGetObject builder "nodeLabCheckBtn" >>= unsafeCastTo Gtk.CheckButton . fromJust
  edgeLabCheckBtn <- Gtk.builderGetObject builder "edgeLabCheckBtn" >>= unsafeCastTo Gtk.CheckButton . fromJust
  nodeTypeBox <- Gtk.builderGetObject builder "nodeTypeBox" >>= unsafeCastTo Gtk.Box . fromJust
  nodeTypeCBox <- Gtk.builderGetObject builder "nodeTypeCBox" >>= unsafeCastTo Gtk.ComboBoxText . fromJust
  edgeTypeBox <- Gtk.builderGetObject builder "edgeTypeBox" >>= unsafeCastTo Gtk.Box . fromJust
  edgeTypeCBox <- Gtk.builderGetObject builder "edgeTypeCBox" >>= unsafeCastTo Gtk.ComboBoxText . fromJust
  operationBox <- Gtk.builderGetObject builder "operationBox" >>= unsafeCastTo Gtk.Box . fromJust
  operationCBox <- Gtk.builderGetObject builder "operationCBox" >>= unsafeCastTo Gtk.ComboBoxText . fromJust
  mergeBtn <- Gtk.builderGetObject builder "mergeBtn" >>= unsafeCastTo Gtk.Button . fromJust
  splitBtn <- Gtk.builderGetObject builder "splitBtn" >>= unsafeCastTo Gtk.Button . fromJust
  let typeSelectionWidgets = ( typeSelectionBox
                             , nodeLabCheckBtn, edgeLabCheckBtn
                             , nodeTypeBox, nodeTypeCBox
                             , edgeTypeBox, edgeTypeCBox
                             , operationBox, operationCBox
                             , mergeBtn, splitBtn 
                             )
  return (mainPane, treeFrame, canvas, entry, entryLabel, layoutWidgets, typeSelectionWidgets)

   

-- creates the treePanel
buildTreePanel = do
  return () :: IO ()
  mainBox <- new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 0]

  scrolledwin <- new Gtk.ScrolledWindow []
  Gtk.boxPackStart mainBox scrolledwin True True 0
  treeview <- new Gtk.TreeView [ #headersVisible := True
                               , #levelIndentation := 4 ]
  Gtk.containerAdd scrolledwin treeview

  colChanges <- new Gtk.TreeViewColumn []
  Gtk.treeViewAppendColumn treeview colChanges
  rendererChanges <- new Gtk.CellRendererText [#editable := False]
  Gtk.cellLayoutPackStart colChanges rendererChanges False

  colName <- new Gtk.TreeViewColumn [#title := "name"]
  Gtk.treeViewAppendColumn treeview colName
  rendererProj <- new Gtk.CellRendererText [#editable := True]
  Gtk.cellLayoutPackStart colName rendererProj False

  colActive <- new Gtk.TreeViewColumn [#title := "actived"]
  Gtk.treeViewAppendColumn treeview colActive
  rendererActive <- new Gtk.CellRendererToggle [#activatable := True, #radio := False]
  Gtk.cellLayoutPackStart colActive rendererActive False


  btnNewR <- new Gtk.Button [#label := "New Rule"]
  Gtk.boxPackStart mainBox btnNewR False False 0

  btnRmv <- new Gtk.Button [#label := "Remove Rule"]
  Gtk.boxPackStart mainBox btnRmv False False 0

  btnNewN <- new Gtk.Button [#label := "New NAC"]
  Gtk.boxPackStart mainBox btnNewN False False 0

  return (mainBox, treeview, rendererChanges, rendererProj, rendererActive, btnNewR, btnRmv, btnNewN)