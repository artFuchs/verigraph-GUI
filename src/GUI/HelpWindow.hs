{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module GUI.HelpWindow (
  buildHelpWindow
, buildAboutDialog
)where

import qualified GI.Gtk as Gtk
import           Data.GI.Base
import           Data.Maybe
import           Data.GI.Base.ManagedPtr (unsafeCastTo)

buildHelpWindow :: IO Gtk.Window
buildHelpWindow = do
  helpWindow <- new Gtk.Window [ #title := "Verigraph-GUI Help"
                               , #defaultWidth := 600
                               , #defaultHeight := 400]

  helpTagTable <- new Gtk.TextTagTable []
  titleTag <- new Gtk.TextTag [ #font := "Liberation Sans Bold 18"
                              , #pixelsBelowLines := 10]
  subtitleTag <- new Gtk.TextTag [ #font := "Liberation Sans Italic 14"
                              , #pixelsBelowLines := 10]
  Gtk.textTagTableAdd helpTagTable titleTag
  Gtk.textTagTableAdd helpTagTable subtitleTag

  helpBuffer <- new Gtk.TextBuffer [ #tagTable := helpTagTable]


  Gtk.textBufferInsertAtCursor helpBuffer "Basic Editing \n" (-1)
  cpte1 <- get helpBuffer #cursorPosition
  Gtk.textBufferInsertAtCursor helpBuffer "Click with the right mouse button in a blank space to create a new node. \n\n" (-1)
  Gtk.textBufferInsertAtCursor helpBuffer "Click with the left mouse button in a node/edge to select it. \n" (-1)
  Gtk.textBufferInsertAtCursor helpBuffer "Use <shift> to select multiple items. \n" (-1)
  Gtk.textBufferInsertAtCursor helpBuffer "Use <ctrl> + <shift> to remove an element from selection. \n \n" (-1)
  Gtk.textBufferInsertAtCursor helpBuffer "Click with the right button in a node while there's other nodes selected to create edges from the selected nodes to it. \n" (-1)
  Gtk.textBufferInsertAtCursor helpBuffer "You can also create loops by selecting a node and then clicking in the same node. \n" (-1)
  cpts2 <- get helpBuffer #cursorPosition
  Gtk.textBufferInsertAtCursor helpBuffer "\nChanging elements properties \n" (-1)
  cpte2 <- get helpBuffer #cursorPosition
  Gtk.textBufferInsertAtCursor helpBuffer "To change the properties of a node/edge, select it and use the inspector panel on the right. \n" (-1)
  Gtk.textBufferInsertAtCursor helpBuffer "Double-clicking a element, or pressing F2 while it is selected will focus on the entry box in the inspector panel." (-1)
  Gtk.textBufferInsertAtCursor helpBuffer "This allows you to edit the label of the element. \n" (-1)
  Gtk.textBufferInsertAtCursor helpBuffer "To change the properties of a node/edge, select it and use the inspector panel on the right. \n" (-1)
  cpts3 <- get helpBuffer #cursorPosition
  Gtk.textBufferInsertAtCursor helpBuffer "\nZoom and navigation \n" (-1)
  cpte3 <- get helpBuffer #cursorPosition
  Gtk.textBufferInsertAtCursor helpBuffer "Use <ctrl> + mouse wheel or <ctrl> + <+/-> to change the zoom level. \n" (-1)
  Gtk.textBufferInsertAtCursor helpBuffer "Use <ctrl> + <=> to change the zoom level to the original. \n" (-1)
  Gtk.textBufferInsertAtCursor helpBuffer "Hold the middle mouse button, or <ctrl> + right mouse button, to navigate throught the canvas. \n" (-1)
  Gtk.textBufferInsertAtCursor helpBuffer "Press <ctrl> + <0> to return to reset the original zoom level and canvas position. \n" (-1)
  
  s1 <- Gtk.textBufferGetStartIter helpBuffer
  e1 <- Gtk.textBufferGetIterAtOffset helpBuffer cpte1
  Gtk.textBufferApplyTag helpBuffer titleTag s1 e1

  s2 <- Gtk.textBufferGetIterAtOffset helpBuffer cpts2
  e2 <- Gtk.textBufferGetIterAtOffset helpBuffer cpte2
  Gtk.textBufferApplyTag helpBuffer titleTag s2 e2

  s3 <- Gtk.textBufferGetIterAtOffset helpBuffer cpts3
  e3 <- Gtk.textBufferGetIterAtOffset helpBuffer cpte3
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

buildAboutDialog :: IO ()
buildAboutDialog = do
  builder <- new Gtk.Builder []
  Gtk.builderAddFromFile builder "./Resources/aboutWindow.glade"
  dialog  <- Gtk.builderGetObject builder "aboutWin" >>= unsafeCastTo Gtk.AboutDialog. fromJust
  #showAll dialog
  Gtk.dialogRun dialog
  Gtk.widgetDestroy dialog
  return ()