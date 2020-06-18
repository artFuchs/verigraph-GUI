{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module GUI.Analysis.CriticalPairAnalysis (
  buildCpaWindow
)
where

import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk

import Control.Monad.IO.Class
import Data.Maybe
import Data.GI.Base
import Data.GI.Base.ManagedPtr (unsafeCastTo)

buildCpaWindow :: Gtk.Window -> IO (Gtk.Window, Gtk.CheckButton, Gtk.CheckButton, Gtk.CheckButton, Gtk.Button, Gtk.TextBuffer)
buildCpaWindow window = do
  builder <- new Gtk.Builder []
  Gtk.builderAddFromFile builder "./Resources/cpaWindow.glade"
  win <- Gtk.builderGetObject builder "cpaWindow" >>= unsafeCastTo Gtk.Window . fromJust
  essentialCheckBtn <- Gtk.builderGetObject builder "essentialCheckBtn" >>= unsafeCastTo Gtk.CheckButton . fromJust
  confCheckBtn <- Gtk.builderGetObject builder "confCheckBtn" >>= unsafeCastTo Gtk.CheckButton . fromJust
  dependCheckBtn <- Gtk.builderGetObject builder "dependCheckBtn" >>= unsafeCastTo Gtk.CheckButton . fromJust
  execBtn <- Gtk.builderGetObject builder "execBtn" >>= unsafeCastTo Gtk.Button . fromJust
  textView <- Gtk.builderGetObject builder "textView" >>= unsafeCastTo Gtk.TextView . fromJust
  
  set win [ #transientFor := window
          , #destroyWithParent := True ]
  on win #deleteEvent $ return $ Gtk.widgetHideOnDelete win

  resultBuffer <- new Gtk.TextBuffer []
  set textView [#buffer := resultBuffer]

  Gtk.textBufferInsertAtCursor resultBuffer "⊂_ヽ\n" (-1)
  Gtk.textBufferInsertAtCursor resultBuffer "　 ＼＼ ＿\n" (-1)
  Gtk.textBufferInsertAtCursor resultBuffer "　　 ＼(　•_•) F\n" (-1)
  Gtk.textBufferInsertAtCursor resultBuffer "　　　<　⌒ヽ A\n" (-1)
  Gtk.textBufferInsertAtCursor resultBuffer "　　 /   　 へ＼ N\n" (-1) 
  Gtk.textBufferInsertAtCursor resultBuffer "　  (  　　/  　＼＼T\n" (-1)  
  Gtk.textBufferInsertAtCursor resultBuffer "　　 ﾚ 　ノ　　   ヽ_つA\n" (-1)  
  Gtk.textBufferInsertAtCursor resultBuffer "　　/　/ S\n" (-1) 
  Gtk.textBufferInsertAtCursor resultBuffer "　 /　/ T\n" (-1) 
  Gtk.textBufferInsertAtCursor resultBuffer "　(　(ヽI\n" (-1) 
  Gtk.textBufferInsertAtCursor resultBuffer "　|　|、＼C.\n" (-1) 
  Gtk.textBufferInsertAtCursor resultBuffer "　| 丿 ＼ ⌒)\n" (-1) 
  Gtk.textBufferInsertAtCursor resultBuffer "　| |　　) /\n" (-1) 
  Gtk.textBufferInsertAtCursor resultBuffer "ノ )　　Lﾉ__\n" (-1) 
  Gtk.textBufferInsertAtCursor resultBuffer "(／___\n" (-1) 



  return (win, essentialCheckBtn, confCheckBtn, dependCheckBtn, execBtn, resultBuffer)