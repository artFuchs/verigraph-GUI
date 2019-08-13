{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module Editor.GraphEditor.RuleViewer (
  createRuleViewerWindow
) where


import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import Data.GI.Base
import Control.Monad.IO.Class
import Data.Graphs

import qualified Data.Map as M

import Editor.Data.EditorState
import Editor.Data.GraphicalInfo
import Editor.GraphEditor.UIBuilders
import Editor.Render.Render
import Editor.Helper.Geometry
import Editor.Render.GraphDraw

import Data.IORef


createRuleViewerWindow :: IO (Gtk.Window, Gtk.DrawingArea, Gtk.DrawingArea,
                              IORef EditorState, IORef EditorState, IORef (Graph String String))
createRuleViewerWindow = do

  lesIOR <- newIORef emptyES
  resIOR <- newIORef emptyES
  tgIOR <- newIORef empty
  opIOR <- newIORef (0,0)


  rvWindow <- new Gtk.Window [ #destroyWithParent := True
                             , #defaultWidth := 640
                             , #defaultHeight := 240
                             , #title := "RuleViewer"]

  (rulePaned, lhsCanvas, rhsCanvas) <- buildRuleViewPanel
  Gtk.containerAdd rvWindow rulePaned

  on rvWindow #deleteEvent $ return $ do
      #hide rvWindow
      return True

  connectRuleViewerCanvasSignals lhsCanvas lesIOR tgIOR opIOR
  connectRuleViewerCanvasSignals rhsCanvas resIOR tgIOR opIOR
  return (rvWindow, lhsCanvas, rhsCanvas, lesIOR, resIOR, tgIOR)

connectRuleViewerCanvasSignals :: Gtk.DrawingArea -> IORef EditorState -> IORef (Graph String String) -> IORef (Double, Double) -> IO ()
connectRuleViewerCanvasSignals canvas esIOR tgIOR oldPointIOR = do
  on canvas #draw $ \context -> do
    es <- readIORef esIOR
    tg <- readIORef tgIOR
    renderWithContext context $ drawRuleSideGraph es Nothing
    return False

  on canvas #buttonPressEvent $ \eventButton -> do
    x <- get eventButton #x
    y <- get eventButton #y
    es <- readIORef esIOR
    let z = editorGetZoom es
        (px,py) = editorGetPan es
        (x',y') = (x/z - px, y/z - py)
    liftIO $ do
      writeIORef oldPointIOR (x',y')
    return True

  on canvas #motionNotifyEvent $ \eventMotion -> do
    x <- get eventMotion #x
    y <- get eventMotion #y
    ms <- get eventMotion #state
    (ox,oy) <- readIORef oldPointIOR
    es <- readIORef esIOR
    let z = editorGetZoom es
        (px,py) = editorGetPan es
        (x',y') = (x/z - px, y/z - py)
        (dx,dy) = (x'-ox,y'-oy)
    case (Gdk.ModifierTypeButton2Mask `elem` ms) of
      False -> return()
      True -> do
        modifyIORef esIOR (editorSetPan (px+dx, py+dy))
        Gtk.widgetQueueDraw canvas
    return True

  on canvas #scrollEvent $ \eventScroll -> do
    d <- get eventScroll #direction
    es <- readIORef esIOR
    let z = editorGetZoom es
    case d of
      -- if the direction is up, then zoom in
      Gdk.ScrollDirectionUp  -> do
        modifyIORef esIOR (editorSetZoom (z*1.1))
        Gtk.widgetQueueDraw canvas
      -- if the direction is down, then zoom out
      Gdk.ScrollDirectionDown -> do
        let z' = if (z>=0.5) then z*0.9 else z
        modifyIORef esIOR (editorSetZoom z')
        Gtk.widgetQueueDraw canvas
      _ -> return ()
    return True

  return ()
