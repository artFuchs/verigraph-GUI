{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module GUI.Analysis.CriticalPairAnalysis (
  buildCpaBox
)
where

import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk

import Control.Monad.IO.Class
import Data.Maybe
import Data.GI.Base
import Data.GI.Base.ManagedPtr (unsafeCastTo)

buildCpaBox :: IO (Gtk.Box, Gtk.CheckButton, Gtk.CheckButton, Gtk.CheckButton, Gtk.Button, Gtk.TextBuffer)
buildCpaBox = do
  builder <- new Gtk.Builder []
  Gtk.builderAddFromFile builder "./Resources/cpaBox.glade"

  cpaBox <- Gtk.builderGetObject builder "cpaBox" >>= unsafeCastTo Gtk.Box . fromJust
  essentialCheckBtn <- Gtk.builderGetObject builder "essentialCheckBtn" >>= unsafeCastTo Gtk.CheckButton . fromJust
  confCheckBtn <- Gtk.builderGetObject builder "confCheckBtn" >>= unsafeCastTo Gtk.CheckButton . fromJust
  dependCheckBtn <- Gtk.builderGetObject builder "dependCheckBtn" >>= unsafeCastTo Gtk.CheckButton . fromJust
  execBtn <- Gtk.builderGetObject builder "execBtn" >>= unsafeCastTo Gtk.Button . fromJust
  textView <- Gtk.builderGetObject builder "textView" >>= unsafeCastTo Gtk.TextView . fromJust

  resultBuffer <- new Gtk.TextBuffer []
  set textView [#buffer := resultBuffer]

  Gtk.textBufferInsertAtCursor resultBuffer "Critical Pair Analysis\n" (-1)
  
  return (cpaBox, essentialCheckBtn, confCheckBtn, dependCheckBtn, execBtn, resultBuffer)