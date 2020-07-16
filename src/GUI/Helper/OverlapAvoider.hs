-- | This module provides functions to avoid overlapping positions of new nodes
module GUI.Helper.OverlapAvoider(
  repositionNode
)where

import qualified Data.Map as M
import GUI.Helper.Geometry
import GUI.Data.GraphicalInfo

repositionNode :: NodeGI -> GraphicalInfo -> NodeGI
repositionNode ngi giM =
  if nodeOverlap ngi giM
    then repositionNode' ngi giM 5 8
    else ngi    

repositionNode' :: NodeGI -> GraphicalInfo -> Int -> Double -> NodeGI
repositionNode' ngi giM tries offset =
  if (nodeOverlap ngi' giM) && (tries > 0)
    then repositionNode' ngi' giM (tries-1) (offset*2)
    else ngi'
  where
    ngi' = repositionNode'' ngi (M.elems $ fst giM) offset
    
repositionNode'' :: NodeGI -> [NodeGI] -> Double -> NodeGI
repositionNode'' ngi [] offset = ngi
repositionNode'' ngi (ngix:ngis) offset = repositionNode'' ngi' ngis offset
  where
    ngi' = if nodesOverlap ngi ngix
            then ngi {position = addPoint (position ngi) (calculateForce ngi ngix offset)}
            else ngi

calculateForce :: NodeGI -> NodeGI -> Double -> (Double,Double)
calculateForce ngi1 ngi2 offset = multPoint dir (strength,strength)
  where
    (x1,y1,_,_,_,r1) = nodeGeometry ngi1 
    (x2,y2,_,_,_,r2) = nodeGeometry ngi2
    (x3,y3) = (x1-x2, y1-y2)
    dir     = if x3 == 0 && y3 == 0 
                then (1,0)
                else normalizeVector (x3,y3)
    strength = (r1+r2+offset)/2

nodeOverlap :: NodeGI -> GraphicalInfo -> Bool
nodeOverlap ngi (nodegiM,_) = or $ map (nodesOverlap ngi) (M.elems nodegiM)

nodesOverlap :: NodeGI -> NodeGI -> Bool
nodesOverlap ngi1 ngi2 = 
  let (x1,y1,w1,h1,l1,r1) = nodeGeometry ngi1
      (x2,y2,w2,h2,l2,r2) = nodeGeometry ngi2
  in case (shape ngi1, shape ngi2) of
      (NCircle, NCircle) -> circleOverlapsCircle (x1,y1,r1) (x2,y2,r2)
      (NCircle, NRect)   -> circleOverlapsRectangle (x1,y1,r1) (x2,y2,w2,h2)
      (NCircle, NSquare) -> circleOverlapsRectangle (x1,y1,r1) (x2,y2,l2,l2)
      (NRect,   NCircle) -> circleOverlapsRectangle (x2,y2,r2) (x1,y1,w1,h1)
      (NRect,   NRect)   -> rectangleOverlapsRectangle (x1,y1,w1,h1) (x2,y2,w2,h2)
      (NRect,   NSquare) -> rectangleOverlapsRectangle (x1,y1,w1,h1) (x2,y2,l2,l2)
      (NSquare, NCircle) -> circleOverlapsRectangle (x2,y2,r2) (x1,y1,w1,h1)
      (NSquare, NRect)   -> rectangleOverlapsRectangle (x1,y1,l1,l1) (x2,y2,w2,h2)
      (NSquare, NSquare) -> rectangleOverlapsRectangle (x1,y1,l1,l1) (x2,y2,l2,l2)

nodeGeometry :: NodeGI -> (Double,Double,Double,Double,Double,Double)
nodeGeometry nodegi = 
      let (x,y) = position nodegi
          (w,h) = dims nodegi
          l     = (max w h)
      in (x,y,w,h,l,l/2)
    