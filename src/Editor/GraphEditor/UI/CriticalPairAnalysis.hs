{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module Editor.GraphEditor.UI.CriticalPairAnalysis (
  buildCpaWindow

)
where

import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk

import Control.Monad.IO.Class
import Data.Maybe
import Data.GI.Base
import Data.GI.Base.ManagedPtr (unsafeCastTo)

buildCpaWindow :: Gtk.Window -> IO (Gtk.Window, Gtk.CheckButton, Gtk.CheckButton, Gtk.CheckButton, Gtk.Button)
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

  return (win, essentialCheckBtn, confCheckBtn, dependCheckBtn, execBtn)