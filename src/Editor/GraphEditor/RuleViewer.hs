{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module Editor.GraphEditor.RuleViewer (
  createRuleViewerWindow
, buildRuleViewPanel
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


createRuleViewerWindow :: IO (Gtk.Window, Gtk.Label, Gtk.DrawingArea,
                              Gtk.DrawingArea, IORef EditorState,
                              IORef EditorState, IORef (Graph String String),
                              IORef (Graph String String))
createRuleViewerWindow = do

  lesIOR <- newIORef emptyES
  resIOR <- newIORef emptyES
  kIOR   <- newIORef empty
  tgIOR  <- newIORef empty
  opIOR  <- newIORef (0,0)

  rvWindow <- new Gtk.Window [ #destroyWithParent := True
                             , #defaultWidth := 640
                             , #defaultHeight := 240
                             , #title := "RuleViewer"]

  (ruleBox, nameLabel, lhsCanvas, rhsCanvas) <- buildRuleViewPanel
  Gtk.containerAdd rvWindow ruleBox

  on rvWindow #deleteEvent $ return $ do
      #hide rvWindow
      return True

  connectRuleViewerCanvasSignals lhsCanvas lesIOR tgIOR kIOR opIOR
  connectRuleViewerCanvasSignals rhsCanvas resIOR tgIOR kIOR opIOR
  return (rvWindow, nameLabel, lhsCanvas, rhsCanvas, lesIOR, resIOR, tgIOR, kIOR)

connectRuleViewerCanvasSignals :: Gtk.DrawingArea
                                  -> IORef EditorState
                                  -> IORef (Graph String String)
                                  -> IORef (Graph String String)
                                  -> IORef (Double, Double)
                                  -> IO ()
connectRuleViewerCanvasSignals canvas esIOR tgIOR kIOR oldPointIOR = do
  on canvas #draw $ \context -> do
    es <- readIORef esIOR
    tg <- readIORef tgIOR
    k <- readIORef kIOR
    renderWithContext context $ drawRuleSideGraph es Nothing k
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

buildRuleViewPanel :: IO (Gtk.Box, Gtk.Label, Gtk.DrawingArea, Gtk.DrawingArea)
buildRuleViewPanel = do
  ruleBox <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
  nameLabel <- new Gtk.Label [#label := "Name"]
  Gtk.boxPackStart ruleBox nameLabel False False 0
  rulePaned <- new Gtk.Paned [ #orientation := Gtk.OrientationHorizontal]
  Gtk.boxPackStart ruleBox rulePaned True True 0

  lhsFrame <- new Gtk.Frame [ #label := "L"
                            , #shadowType := Gtk.ShadowTypeIn]
  Gtk.panedPack1 rulePaned lhsFrame True True
  lhsCanvas <- Gtk.drawingAreaNew
  Gtk.containerAdd lhsFrame lhsCanvas
  Gtk.widgetSetCanFocus lhsCanvas True
  Gtk.widgetSetEvents lhsCanvas [toEnum $ fromEnum Gdk.EventMaskAllEventsMask - fromEnum Gdk.EventMaskSmoothScrollMask]

  rhsFrame <- new Gtk.Frame [ #label := "R"
                            , #shadowType := Gtk.ShadowTypeIn]
  Gtk.panedPack2 rulePaned rhsFrame True True
  rhsCanvas <- Gtk.drawingAreaNew
  Gtk.containerAdd rhsFrame rhsCanvas
  Gtk.widgetSetCanFocus rhsCanvas True
  Gtk.widgetSetEvents rhsCanvas [toEnum $ fromEnum Gdk.EventMaskAllEventsMask - fromEnum Gdk.EventMaskSmoothScrollMask]

  return (ruleBox, nameLabel, lhsCanvas, rhsCanvas)
