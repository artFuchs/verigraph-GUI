{-|
    This module provides basic canvas callbacks for diagram.
    By manipulation is meant to select and move nodes and edges of the diagram.
-}

{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module GUI.Helper.BasicCanvasCallbacks(
  setBasicCanvasCallbacks
, basicCanvasButtonPressedCallback
, basicCanvasMotionCallBack
, basicCanvasButtonReleasedCallback
, basicCanvasScrollCallback
)where

import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import           Data.GI.Base

import           Control.Monad
import           Data.IORef
import           Data.List
import qualified Data.Map as M

import           Data.Graphs hiding (null, empty)

import           GUI.Data.Diagram
import           GUI.Data.GraphState
import           GUI.Data.GraphicalInfo
import           GUI.Data.Info
import           GUI.Helper.Geometry
import           GUI.Helper.List
import           GUI.Render.Render




type SquareSelection = Maybe (Double,Double,Double,Double)

{-
  Set all basic callbacks to a canvas
-}
setBasicCanvasCallbacks :: Gtk.DrawingArea
                   -> IORef GraphState
                   -> IORef (Graph Info Info )
                   -> Maybe (GraphState -> SquareSelection -> Graph Info Info -> Maybe (Double,Double) -> Render ())
                   -> IORef (Maybe Gtk.DrawingArea) -> IORef (Maybe (IORef GraphState))
                   -> IO (IORef (Double,Double), IORef SquareSelection)
setBasicCanvasCallbacks canvas state refGraph drawMethod focusedCanvas focusedStateIORef = do
    oldPoint        <- newIORef (0.0,0.0) -- last point where a mouse button was pressed
    squareSelection <- newIORef Nothing   -- selection box : Maybe (x1,y1,x2,y2)
    case drawMethod of
        Just draw -> do
            on canvas #draw $ \context -> do
                es <- readIORef state
                rg <- readIORef refGraph
                sq <- readIORef squareSelection
                aloc <- Gtk.widgetGetAllocation canvas
                w <- Gdk.getRectangleWidth aloc >>= return . fromIntegral :: IO Double
                h <- Gdk.getRectangleHeight aloc >>= return . fromIntegral :: IO Double
                renderWithContext context $ draw es sq rg (Just (w,h))
                return False
            return ()
        Nothing -> return ()
    on canvas #buttonPressEvent $ basicCanvasButtonPressedCallback state oldPoint squareSelection canvas
    on canvas #motionNotifyEvent $ basicCanvasMotionCallBack state oldPoint squareSelection canvas
    on canvas #buttonReleaseEvent $ basicCanvasButtonReleasedCallback state squareSelection canvas
    on canvas #scrollEvent $ basicCanvasScrollCallback state canvas
    on canvas #focusInEvent $ \event -> do
        writeIORef focusedCanvas $ Just canvas
        writeIORef focusedStateIORef $ Just state
        return False
    return (oldPoint,squareSelection)


{-|
    When the left mouse button is pressed inside of a canvas, elements of the diagram can be selected and square selection can be started
-}
basicCanvasButtonPressedCallback :: IORef GraphState -> IORef (Double,Double) -> IORef (Maybe (Double,Double,Double,Double)) -> Gtk.DrawingArea -> Gdk.EventButton -> IO Bool
basicCanvasButtonPressedCallback state oldPoint squareSelection canvas eventButton = do
  b <- get eventButton #button
  x <- get eventButton #x
  y <- get eventButton #y
  ms <- get eventButton #state
  click <- get eventButton #type
  es <- readIORef state
  let z = stateGetZoom es
      (px,py) = stateGetPan es
      (x',y') = (x/z - px, y/z - py)
  writeIORef oldPoint (x',y')
  case (b, click == Gdk.EventType2buttonPress) of
    -- left button: select nodes and edges
    (1, False) -> do
      let (oldSN,oldSE) = stateGetSelected es
          graph = stateGetGraph es
          gi = stateGetGI es
          sNode = case selectNodeInPosition gi (x',y') of
            Nothing -> []
            Just nid -> [nid]
          sEdge = case selectEdgeInPosition graph gi (x',y') of
              Nothing -> []
              Just eid -> [eid]
      -- add/remove elements of selection
      case (sNode, sEdge, Gdk.ModifierTypeShiftMask `elem` ms, Gdk.ModifierTypeControlMask `elem` ms) of
        -- clicked in blank space with Shift not pressed -> clean selection, start square seleciton
        ([], [], False, _) -> do
          modifyIORef state (stateSetSelected ([],[]))
          writeIORef squareSelection $ Just (x',y',0,0)
        -- selected nodes or edges without modifier key:
        (n, e, False, _) -> do
          let nS = if null n then False else n!!0 `elem` oldSN
              eS = if null e then False else e!!0 `elem` oldSE
          if nS || eS
          then return ()
          else do
            modifyIORef state (stateSetSelected (n, e))
        -- selected nodes or edges with Shift pressed -> add to selection
        (n, e, True, False) -> do
          let jointSN = removeDuplicates $ sNode ++ oldSN
              jointSE = removeDuplicates $ sEdge ++ oldSE
          modifyIORef state (stateSetGraph graph . stateSetSelected (jointSN,jointSE))
        -- selected nodes or edges with Shift + Ctrl pressed -> remove from selection
        (n, e, True, True) -> do
          let jointSN = if null n then oldSN else delete (n!!0) oldSN
              jointSE = if null e then oldSE else delete (e!!0) oldSE
          modifyIORef state (stateSetGraph graph . stateSetSelected (jointSN,jointSE))
    _ -> return ()
  Gtk.widgetQueueDraw canvas
  Gtk.widgetGrabFocus canvas
  return False

{-|
    When the mouse is moved inside the canvas, if the left buton is hold, elements can be moved or the square selection updated.
    If the middle button is hold, the canvas can be panned
-}
basicCanvasMotionCallBack :: IORef GraphState -> IORef (Double,Double) -> IORef (Maybe (Double,Double,Double,Double)) -> Gtk.DrawingArea -> Gdk.EventMotion -> IO Bool
basicCanvasMotionCallBack state oldPoint squareSelection canvas eventMotion = do
  ms <- get eventMotion #state
  x <- get eventMotion #x
  y <- get eventMotion #y
  (ox,oy) <- readIORef oldPoint
  es <- readIORef state

  let leftButton = Gdk.ModifierTypeButton1Mask `elem` ms
      -- in case of the editor being used in a laptop or with a mouse with just two buttons, ctrl + right button can be used instead of the middle button.
      middleButton = (Gdk.ModifierTypeButton2Mask `elem` ms) || (Gdk.ModifierTypeButton3Mask `elem` ms && Gdk.ModifierTypeControlMask `elem` ms)
      (sNodes, sEdges) = stateGetSelected es
      z = stateGetZoom es
      (px,py) = stateGetPan es
      (x',y') = (x/z - px, y/z - py)

  case (leftButton, middleButton, sNodes, sEdges) of
    -- if left button is pressed and no node is selected, update square selection
    (True, False, [], []) -> do
      modifyIORef squareSelection $ liftM $ (\(a,b,c,d) -> (a,b,x'-a,y'-b))
      sq <- readIORef squareSelection
      Gtk.widgetQueueDraw canvas
    -- if left button is pressed with some elements selected, then move them
    (True, False, n, e) -> do
      modifyIORef state (\es -> moveNodes es (ox,oy) (x',y') )
      modifyIORef state (\es -> if (>0) . length . fst . stateGetSelected $ es
                              then es
                              else moveEdges es (ox,oy) (x',y'))
      writeIORef oldPoint (x',y')
      Gtk.widgetQueueDraw canvas
    -- if middle button is pressed, then move the view
    (False ,True, _, _) -> do
      let (dx,dy) = (x'-ox,y'-oy)
      modifyIORef state (stateSetPan (px+dx, py+dy))
      Gtk.widgetQueueDraw canvas
    _ -> return ()
  return False

-- | When the mouse buton is released, the square selection (if started) is eliminated and the elements inside it are selected
basicCanvasButtonReleasedCallback :: IORef GraphState -> IORef (Maybe (Double,Double,Double,Double)) -> Gtk.DrawingArea -> Gdk.EventButton -> IO Bool
basicCanvasButtonReleasedCallback state squareSelection canvas eventButton = do
  b <- get eventButton #button
  case b of
    1 -> do
      es <- readIORef state
      sq <- readIORef squareSelection
      let (n,e) = stateGetSelected es
      case (stateGetSelected es,sq) of
        -- if release the left button when there's a square selection,
        -- select the elements that are inside the selection
        (([],[]), Just (x,y,w,h)) -> do
          let graph = stateGetGraph es
              (ngiM, egiM) = stateGetGI es
              sNodes = map NodeId $ M.keys $
                                    M.filter (\ngi -> let pos = position ngi
                                                      in pointInsideRectangle pos (x + (w/2), y + (h/2), abs w, abs h)) ngiM
              sEdges = map edgeId $ filter (\e -> let pos = getEdgePosition (graph,(ngiM,egiM)) e
                                                  in pointInsideRectangle pos (x + (w/2), y + (h/2), abs w, abs h)) $ edges graph
              newEs = stateSetSelected (sNodes, sEdges) $ es
          writeIORef state newEs
        ((n,e), Nothing) -> return ()
        _ -> return ()
    _ -> return ()
  writeIORef squareSelection Nothing
  Gtk.widgetQueueDraw canvas
  return False

-- | Control the zoom of the canvas with mouse wheel
basicCanvasScrollCallback :: IORef GraphState -> Gtk.DrawingArea -> Gdk.EventScroll -> IO Bool
basicCanvasScrollCallback state canvas eventScroll = do
  d <- get eventScroll #direction
  ms <- get eventScroll #state
  case (Gdk.ModifierTypeControlMask `elem` ms, d) of
    (True, Gdk.ScrollDirectionUp) -> modifyIORef state (\st -> stateSetZoom (stateGetZoom st * 1.1) st)
    (True, Gdk.ScrollDirectionDown) -> modifyIORef state (\st -> let z = stateGetZoom st * 0.9 in if z >= 0.5 then stateSetZoom z st else st)
    _ -> return ()
  Gtk.widgetQueueDraw canvas
  Gtk.widgetGrabFocus canvas
  return False
