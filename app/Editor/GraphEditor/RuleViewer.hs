{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module Editor.GraphEditor.RuleViewer (
  createRuleViewerWindow
) where


import qualified GI.Gtk as Gtk
import Data.GI.Base
import Control.Monad.IO.Class
import Data.Graphs

import qualified Data.Map as M

import Editor.Data.EditorState
import Editor.Data.GraphicalInfo
import Editor.GraphEditor.UIBuilders
import Editor.Render.Render
import Editor.Render.Geometry
import Editor.Render.GraphDraw

import Data.IORef


createRuleViewerWindow :: IO (Gtk.Window, Gtk.DrawingArea, Gtk.DrawingArea,
                              IORef EditorState, IORef EditorState, IORef (Graph String String))
createRuleViewerWindow = do

  lesIOR <- newIORef emptyES
  resIOR <- newIORef emptyES
  tgIOR <- newIORef empty

  rvWindow <- new Gtk.Window [ #destroyWithParent := True
                             , #defaultWidth := 640
                             , #defaultHeight := 240
                             , #title := "RuleViewer"]

  (rulePaned, lhsCanvas, rhsCanvas) <- buildRuleViewPanel
  Gtk.containerAdd rvWindow rulePaned

  on rvWindow #deleteEvent $ return $ do
      #hide rvWindow
      return True

  connectRuleViewerCanvasSignals lhsCanvas lesIOR tgIOR
  connectRuleViewerCanvasSignals rhsCanvas resIOR tgIOR
  return (rvWindow, lhsCanvas, rhsCanvas, lesIOR, resIOR, tgIOR)

connectRuleViewerCanvasSignals :: Gtk.DrawingArea -> IORef EditorState -> IORef (Graph String String) -> IO ()
connectRuleViewerCanvasSignals canvas esIOR tgIOR = do
  on canvas #draw $ \context -> do
    es <- readIORef esIOR
    tg <- readIORef tgIOR
    renderWithContext context $ drawHostGraph es Nothing tg
    return False
  return ()
