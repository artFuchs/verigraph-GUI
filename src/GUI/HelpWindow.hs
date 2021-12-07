{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module GUI.HelpWindow (
  buildHelpWindow
, buildAboutDialog
)where

import qualified GI.Gtk as Gtk
import           Data.GI.Base
import           Data.GI.Base.ManagedPtr (unsafeCastTo)

import           Control.Monad

import           Data.Maybe
import           Data.Int
import qualified Data.Text as T

import           GUI.Helper.FilePath


buildHelpWindow :: IO Gtk.Window
buildHelpWindow = do
  builder <- new Gtk.Builder []
  resourcesFolder <- getResourcesFolder
  Gtk.builderAddFromFile builder $ T.pack (resourcesFolder ++ "helpWindow.glade")

  helpWindow  <- Gtk.builderGetObject builder "helpWindow"  >>= unsafeCastTo Gtk.Window     . fromJust
  textBuffer1 <- Gtk.builderGetObject builder "textbuffer1" >>= unsafeCastTo Gtk.TextBuffer . fromJust
  textBuffer2 <- Gtk.builderGetObject builder "textbuffer2" >>= unsafeCastTo Gtk.TextBuffer . fromJust
  textBuffer3 <- Gtk.builderGetObject builder "textbuffer3" >>= unsafeCastTo Gtk.TextBuffer . fromJust
  textBuffer4 <- Gtk.builderGetObject builder "textbuffer4" >>= unsafeCastTo Gtk.TextBuffer . fromJust

  let
    applyTags :: Gtk.TextBuffer -> Int32 -> Int32 -> IO ()
    applyTags buffer ln maxln = do
        start <- Gtk.textBufferGetIterAtLine buffer ln
        end   <- Gtk.textBufferGetIterAtLine buffer ln
        _     <- Gtk.textIterForwardToLineEnd end
        startChar  <- Gtk.textIterGetChar start

        if startChar == '#'
            then do
                Gtk.textBufferApplyTagByName buffer "titleTag" start end
                nextChar <- Gtk.textBufferGetIterAtLineOffset buffer ln 1
                Gtk.textBufferDelete buffer start nextChar
            else return ()

        if ln < maxln
            then applyTags buffer (ln+(1::Int32)) maxln
            else return ()

  let tbuffers = [textBuffer1, textBuffer2, textBuffer3, textBuffer4]
  forM_ tbuffers $ \tbuffer -> do
    tbufferLC <- Gtk.textBufferGetLineCount tbuffer
    applyTags tbuffer 0 (tbufferLC-1)

  -- textBuffer1LC <- Gtk.textBufferGetLineCount textBuffer1
  -- applyTags textBuffer1 0 (textBuffer1LC-1)
  --
  -- textBuffer2LC <- Gtk.textBufferGetLineCount textBuffer2
  -- applyTags textBuffer2 0 (textBuffer2LC-1)
  --
  -- textBuffer3LC <- Gtk.textBufferGetLineCount textBuffer3
  -- applyTags textBuffer3 0 (textBuffer3LC-1)
  --
  -- textBuffer4LC <- Gtk.textBufferGetLineCount textBuffer4
  -- applyTags textBuffer4 0 (textBuffer4LC-1)

  on helpWindow #deleteEvent $ return $ do
      #hide helpWindow
      return True

  return helpWindow


buildAboutDialog :: IO ()
buildAboutDialog = do
  builder <- new Gtk.Builder []
  resourcesFolder <- getResourcesFolder
  Gtk.builderAddFromFile builder $ T.pack (resourcesFolder ++ "aboutWindow.glade")
  dialog  <- Gtk.builderGetObject builder "aboutWin" >>= unsafeCastTo Gtk.AboutDialog. fromJust
  #showAll dialog
  Gtk.dialogRun dialog
  Gtk.widgetDestroy dialog
  return ()
