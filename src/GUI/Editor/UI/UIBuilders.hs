{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

-- | This module contains the UI definition
module GUI.Editor.UI.UIBuilders(
  buildEditor
, buildTreePanel
) where

import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import Data.GI.Base
import Data.GI.Base.GValue
import Data.Int

import Control.Monad.IO.Class
import Data.Maybe
import Data.GI.Base
import qualified Data.Text as T
import Data.GI.Base.ManagedPtr (unsafeCastTo)

import GUI.Helper.FilePath

buildEditor :: IO (Gtk.Paned, Gtk.Frame, Gtk.DrawingArea, Gtk.Entry, Gtk.Label
                  , ( Gtk.Box, Gtk.Box, Gtk.ColorButton, Gtk.Box, Gtk.ColorButton
                    , Gtk.Frame, [Gtk.RadioButton], Gtk.Frame, [Gtk.RadioButton])
                  , ( Gtk.Box, Gtk.CheckButton, Gtk.CheckButton
                      , Gtk.Box, Gtk.ComboBoxText, Gtk.Box, Gtk.ComboBoxText
                      , Gtk.Box, Gtk.ComboBoxText
                      , Gtk.Button, Gtk.Button))
buildEditor = do
  builder <- new Gtk.Builder []
  resourcesFolder <- getResourcesFolder
  Gtk.builderAddFromFile builder $ T.pack (resourcesFolder ++ "editor.glade")

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
buildTreePanel :: IO ( Gtk.Box, Gtk.TreeView, Gtk.CellRendererText, Gtk.CellRendererToggle, Gtk.Button, Gtk.Button, Gtk.Button )
buildTreePanel = do
  mainBox <- new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 0]

  scrolledwin <- new Gtk.ScrolledWindow []
  Gtk.boxPackStart mainBox scrolledwin True True 0
  treeview <- new Gtk.TreeView [ #headersVisible := True
                               , #levelIndentation := 4 ]
  Gtk.containerAdd scrolledwin treeview

  changesCol <- new Gtk.TreeViewColumn []
  Gtk.treeViewAppendColumn treeview changesCol
  changesRenderer <- new Gtk.CellRendererText [#editable := False]
  Gtk.cellLayoutPackStart changesCol changesRenderer False

  nameCol <- new Gtk.TreeViewColumn [#title := "name"]
  Gtk.treeViewAppendColumn treeview nameCol
  nameRenderer <- new Gtk.CellRendererText [#editable := True]
  Gtk.cellLayoutPackStart nameCol nameRenderer False

  activeCol <- new Gtk.TreeViewColumn [#title := "actived"]
  Gtk.treeViewAppendColumn treeview activeCol
  activeRenderer <- new Gtk.CellRendererToggle [#activatable := True, #radio := False]
  Gtk.cellLayoutPackStart activeCol activeRenderer False


  btnNewR <- new Gtk.Button [#label := "New Rule"]
  Gtk.boxPackStart mainBox btnNewR False False 0

  btnNewN <- new Gtk.Button [#label := "New NAC"]
  Gtk.boxPackStart mainBox btnNewN False False 0

  btnRmv <- new Gtk.Button [#label := "Remove Rule"]
  Gtk.boxPackStart mainBox btnRmv False False 0

  -- set the information renderered by each column of the treeView
  Gtk.treeViewColumnSetCellDataFunc changesCol changesRenderer $ Just $ \column renderer model iter ->
    do
      changed <- Gtk.treeModelGetValue model iter 1 >>= fromGValue:: IO Bool
      valid <- Gtk.treeModelGetValue model iter 5 >>= fromGValue :: IO Bool
      renderer' <- castTo Gtk.CellRendererText renderer
      case (renderer', changed, valid) of
        (Just r, False, True)  -> set r [#text := ""  ]
        (Just r, True, True)  -> set r [#text := "*" ]
        (Just r, False, False) -> set r [#text := "!" ]
        (Just r, True, False) -> set r [#text := "!*"]
        _ -> return ()

  #addAttribute nameCol nameRenderer "text" 0

  Gtk.treeViewColumnSetCellDataFunc activeCol activeRenderer $ Just $ \column renderer model iter ->
    do
      gType <- Gtk.treeModelGetValue model iter 3 >>= \gv -> (fromGValue gv :: IO Int32)
      active <- Gtk.treeModelGetValue model iter 4 >>= \gv -> (fromGValue gv :: IO Bool)
      renderer' <- castTo Gtk.CellRendererToggle renderer
      case (renderer', gType) of
        (Just r, 3) -> set r [#visible := True, #radio := False, #active := active, #activatable:=True]
        (Just r, 4) -> set r [#visible := True, #radio := False, #active := active, #activatable:=True]
        (Just r, _) -> set r [#visible := False]
        _ -> return ()

  return (mainBox, treeview, nameRenderer, activeRenderer, btnNewR, btnRmv, btnNewN)
