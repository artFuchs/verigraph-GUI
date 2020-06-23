{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

-- | This module contains the UI definition
module GUI.Editor.UI.UIBuilders(
  buildMainWindow
, buildEditor
, buildTreePanel
, buildAboutDialog
, buildHelpWindow
, showError
, createSaveDialog
, createLoadDialog
, createConfirmDialog
) where

import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk

import Control.Monad.IO.Class
import Data.Maybe
import Data.GI.Base
import qualified Data.Text as T
import Data.GI.Base.ManagedPtr (unsafeCastTo)



-- load the main window containing the box and the window
buildMainWindow :: IO ( Gtk.Window, Gtk.Box, [Gtk.MenuItem], [Gtk.MenuItem], [Gtk.MenuItem], [Gtk.MenuItem], [Gtk.MenuItem])
buildMainWindow = do
  builder <- new Gtk.Builder []
  Gtk.builderAddFromFile builder "./Resources/window.glade"
  window  <- Gtk.builderGetObject builder "window" >>= unsafeCastTo Gtk.Window . fromJust
  mainBox <- Gtk.builderGetObject builder "mainBox" >>= unsafeCastTo Gtk.Box . fromJust
  
  -- menubar
  menubar  <- Gtk.builderGetObject builder "menubar" >>= unsafeCastTo Gtk.MenuBar . fromJust

  newItem <- Gtk.builderGetObject builder "new_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  openItem <- Gtk.builderGetObject builder "open_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  saveItem <- Gtk.builderGetObject builder "save_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  saveAsItem <- Gtk.builderGetObject builder "save_as_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  exportGGXItem <- Gtk.builderGetObject builder "export_ggx_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  exportVGGItem <- Gtk.builderGetObject builder "export_vgg_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  saveGraphItem <- Gtk.builderGetObject builder "save_graph_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  openGraphItem <- Gtk.builderGetObject builder "open_graph_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  let fileItems = [newItem,openItem,saveItem,saveAsItem,exportGGXItem,exportVGGItem,saveGraphItem,openGraphItem]

  delItem <- Gtk.builderGetObject builder  "delete_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  undoItem <- Gtk.builderGetObject builder  "undo_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  redoItem <- Gtk.builderGetObject builder  "redo_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  copyItem <- Gtk.builderGetObject builder  "copy_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  pasteItem <- Gtk.builderGetObject builder  "paste_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  cutItem <- Gtk.builderGetObject builder  "cut_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  sallItem <- Gtk.builderGetObject builder  "sall_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  snodesItem <- Gtk.builderGetObject builder  "snodes_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  sedgesItem <- Gtk.builderGetObject builder  "sedges_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  mergeItem <- Gtk.builderGetObject builder  "merge_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  splitItem <- Gtk.builderGetObject builder  "split_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  let editItems = [delItem, undoItem,redoItem,copyItem,pasteItem,cutItem,sallItem,snodesItem,sedgesItem,mergeItem,splitItem]

  zoomInItem <- Gtk.builderGetObject builder  "zoomin_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  zoomOutItem <- Gtk.builderGetObject builder  "zoomout_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  zoom50Item <- Gtk.builderGetObject builder  "zoom50_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  zoom100Item <- Gtk.builderGetObject builder  "zoom100_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  zoom150Item <- Gtk.builderGetObject builder  "zoom150_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  zoom200Item <- Gtk.builderGetObject builder  "zoom200_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  resetViewItem <- Gtk.builderGetObject builder  "resetview_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  let viewItems = [zoomInItem,zoomOutItem,zoom50Item,zoom100Item,zoom150Item,zoom200Item,resetViewItem]

  cpaItem <- Gtk.builderGetObject builder "cpa_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  let analysisItems = [cpaItem]

  helpItem <- Gtk.builderGetObject builder  "help_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  aboutItem <- Gtk.builderGetObject builder  "about_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  let helpItems = [helpItem, aboutItem]

  return ( window, mainBox, fileItems, editItems, viewItems, analysisItems, helpItems)

-- load and build the editor widgets


                      
                     
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

buildAboutDialog :: IO ()
buildAboutDialog = do
  builder <- new Gtk.Builder []
  Gtk.builderAddFromFile builder "./Resources/aboutWindow.glade"
  dialog  <- Gtk.builderGetObject builder "aboutWin" >>= unsafeCastTo Gtk.AboutDialog. fromJust
  #showAll dialog
  Gtk.dialogRun dialog
  Gtk.widgetDestroy dialog
  return ()


buildHelpWindow :: IO Gtk.Window
buildHelpWindow = do
  helpWindow <- new Gtk.Window [ #title := "Verigraph-GUI Help"
                               , #defaultWidth := 600
                               , #defaultHeight := 400]

  helpTagTable <- new Gtk.TextTagTable []
  titleTag <- new Gtk.TextTag [ #font := "Liberation Sans Italic 14"
                              , #pixelsBelowLines := 10]
  Gtk.textTagTableAdd helpTagTable titleTag

  helpBuffer <- new Gtk.TextBuffer [ #tagTable := helpTagTable]


  Gtk.textBufferInsertAtCursor helpBuffer "Basic Editing \n" (-1)
  cpe1 <- get helpBuffer #cursorPosition
  Gtk.textBufferInsertAtCursor helpBuffer "Click with the right mouse button in a blank space to create a new node. \n\n" (-1)
  Gtk.textBufferInsertAtCursor helpBuffer "Click with the left mouse button in a node/edge to select it. \n" (-1)
  Gtk.textBufferInsertAtCursor helpBuffer "Use <shift> to select multiple items. \n" (-1)
  Gtk.textBufferInsertAtCursor helpBuffer "Use <ctrl> + <shift> to remove an element from selection. \n \n" (-1)
  Gtk.textBufferInsertAtCursor helpBuffer "Click with the right button in a node while there's other nodes selected to create edges from the selected nodes to it. \n" (-1)
  Gtk.textBufferInsertAtCursor helpBuffer "You can also create loops by selecting a node and then clicking in the same node. \n" (-1)
  cps2 <- get helpBuffer #cursorPosition
  Gtk.textBufferInsertAtCursor helpBuffer "\nChanging elements properties \n" (-1)
  cpe2 <- get helpBuffer #cursorPosition
  Gtk.textBufferInsertAtCursor helpBuffer "To change the properties of a node/edge, select it and use the inspector panel on the right. \n" (-1)
  Gtk.textBufferInsertAtCursor helpBuffer "Double-clicking a element, or pressing F2 while it is selected will focus on the entry box in the inspector panel. \n" (-1)
  cps3 <- get helpBuffer #cursorPosition
  Gtk.textBufferInsertAtCursor helpBuffer "\nZoom and navigation \n" (-1)
  cpe3 <- get helpBuffer #cursorPosition
  Gtk.textBufferInsertAtCursor helpBuffer "Use <ctrl> + mouse wheel or <ctrl> + <+/-> to change the zoom level. \n" (-1)
  Gtk.textBufferInsertAtCursor helpBuffer "Use <ctrl> + <=> to change the zoom level to the original. \n" (-1)
  Gtk.textBufferInsertAtCursor helpBuffer "Hold the middle mouse button, or <ctrl> + right mouse button, to navigate throught the canvas. \n" (-1)
  Gtk.textBufferInsertAtCursor helpBuffer "Press <ctrl> + <0> to return to reset the original zoom level and canvas position. \n" (-1)

  s1 <- Gtk.textBufferGetStartIter helpBuffer
  e1 <- Gtk.textBufferGetIterAtOffset helpBuffer cpe1
  Gtk.textBufferApplyTag helpBuffer titleTag s1 e1

  s2 <- Gtk.textBufferGetIterAtOffset helpBuffer cps2
  e2 <- Gtk.textBufferGetIterAtOffset helpBuffer cpe2
  Gtk.textBufferApplyTag helpBuffer titleTag s2 e2

  s3 <- Gtk.textBufferGetIterAtOffset helpBuffer cps3
  e3 <- Gtk.textBufferGetIterAtOffset helpBuffer cpe3
  Gtk.textBufferApplyTag helpBuffer titleTag s3 e3



  helpView <- new Gtk.TextView [ #buffer := helpBuffer
                               , #wrapMode := Gtk.WrapModeWord
                               , #editable := False
                               , #cursorVisible := False]
  scrollWin <- new Gtk.ScrolledWindow []
  Gtk.containerAdd scrollWin helpView
  Gtk.containerAdd helpWindow scrollWin

  on helpWindow #deleteEvent $ return $ do
      #hide helpWindow
      return True

  return helpWindow


showError :: Gtk.Window -> T.Text -> IO ()
showError window msg = do
  --dlgE <- messageDialogNew window [DialogDestroyWithParent] MessageError ButtonsOk msg
  msgDialog <- new Gtk.MessageDialog [ #text := msg
                                , #messageType := Gtk.MessageTypeError
                                , #buttons := Gtk.ButtonsTypeOk
                                , #transientFor := window
                                , #destroyWithParent := True
                                ]
  Gtk.widgetShowAll msgDialog
  Gtk.dialogRun msgDialog
  Gtk.widgetDestroy msgDialog
  return ()

createSaveDialog :: Gtk.Window -> IO Gtk.FileChooserDialog
createSaveDialog window = do
  saveD <- new Gtk.FileChooserDialog [ #action := Gtk.FileChooserActionSave
                                     , #createFolders := True
                                     , #doOverwriteConfirmation := True
                                     , #transientFor := window
                                     , #destroyWithParent := True
                                     ]
  Gtk.dialogAddButton saveD "Save" (fromIntegral . fromEnum $ Gtk.ResponseTypeAccept)
  Gtk.dialogAddButton saveD "Cancel" (fromIntegral . fromEnum $ Gtk.ResponseTypeReject)
  return saveD

createLoadDialog :: Gtk.Window -> IO Gtk.FileChooserDialog
createLoadDialog window = do
  loadD <- new Gtk.FileChooserDialog [ #action := Gtk.FileChooserActionOpen
                                     , #createFolders := False
                                     , #doOverwriteConfirmation := False
                                     , #transientFor := window
                                     , #destroyWithParent := True
                                     ]
  Gtk.dialogAddButton loadD "Open" (fromIntegral . fromEnum $ Gtk.ResponseTypeAccept)
  Gtk.dialogAddButton loadD "Cancel" (fromIntegral . fromEnum $ Gtk.ResponseTypeReject)
  return loadD

createConfirmDialog :: Gtk.Window -> T.Text -> IO Gtk.ResponseType
createConfirmDialog window msg = do
  closeD <- new Gtk.MessageDialog
            [ #text := msg
            , #messageType := Gtk.MessageTypeWarning
            , #buttons := Gtk.ButtonsTypeNone
            , #transientFor := window
            , #destroyWithParent := True
            ]
  Gtk.dialogAddButton closeD "Save" (fromIntegral . fromEnum $ Gtk.ResponseTypeYes)
  Gtk.dialogAddButton closeD "Don't save" (fromIntegral . fromEnum $ Gtk.ResponseTypeNo)
  Gtk.dialogAddButton closeD "Cancel" (fromIntegral . fromEnum $ Gtk.ResponseTypeCancel)
  response <- Gtk.dialogRun closeD
  Gtk.widgetDestroy closeD
  return $ toEnum . fromIntegral $ response
