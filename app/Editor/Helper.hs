-- | Module containing auxiliar functions related with the geometry
module Editor.Helper
( pointDistance
, pointLineDistance
, addPoint
, multPoint
, midPoint
, pointInsideRectangle
, pointAt
, toPolarFrom
, angle
, quadrant
, applyPair
, interpolate
)where

import Data.Fixed



-- | calculates the distance between two points
pointDistance :: (Double,Double) -> (Double,Double) -> Double
pointDistance (x1,y1) (x2,y2) = sqrt $ (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2)

-- | calculates the distance between a point and a line
pointLineDistance :: (Double,Double) -> (Double,Double) -> (Double,Double) -> Double
pointLineDistance (x0,y0) (x1,y1) (x2,y2) = ( abs $ (y2-y1)*x0 - (x2-x1)*y0 + x2*y1 - y2*x1 ) / (pointDistance (x1,y1) (x2,y2))

-- | adds two points
addPoint :: (Double,Double) -> (Double,Double) -> (Double,Double)
addPoint (a,b) (c,d) = (a+c,b+d)

-- | multiply two points
multPoint :: (Double,Double) -> (Double,Double) -> (Double,Double)
multPoint (a,b) (c,d) = (a*c,b*d)

-- | calculates the center of two points
midPoint :: (Double,Double) -> (Double,Double) -> (Double,Double)
midPoint (x1,y1) (x2,y2) = (x1 + (x2-x1)/2, y1 + (y2-y1)/2)

-- | given an angle, distance and a initial point, calculates a new point.
pointAt :: Double -> Double -> (Double,Double) -> (Double,Double)
pointAt ang dist (x,y) = (x + dist*cos(ang), y + dist*sin(ang))

toPolarFrom :: (Double,Double) -> (Double,Double) -> (Double,Double)
toPolarFrom  (xRef,yRef) (x,y) =
  let ang  = angle (xRef,yRef) (x,y)
      dist = pointDistance  (xRef,yRef) (x,y)
  in (ang,dist)

-- | check if a point is inside a rectangle
-- considers the rectangle position as it's center
pointInsideRectangle :: (Double,Double) -> (Double,Double,Double,Double) -> Bool
pointInsideRectangle (x,y) (rx,ry,rw,rh) = (abs (x - rx) <= rw/2) && (abs (y - ry) <= rh/2)

-- | angle between tow points
angle :: (Double,Double) -> (Double,Double) -> Double
angle (a,b) (c,d) =
  case (dx `compare` 0,dy `compare` 0) of
       (LT,LT) -> pi + atan(dy/dx)
       (LT,EQ) -> pi
       (LT,GT) -> pi - atan(-dy/dx)
       (EQ,LT) -> 3*pi/2
       (EQ,EQ) -> 0
       (EQ,GT) -> pi/2
       (GT,LT) -> 2*pi - atan(-dy/dx)
       (GT,EQ) -> 0
       (GT,GT) -> atan(dy/dx)
   where  dy = d-b
          dx = c-a

-- | quadrant of an angle
quadrant :: Double -> (Double,Double)
quadrant ang = (c,d)
  where a = ang `mod'` pi
        b = if a < 0 then a + 2*pi else a
        c = if (abs a) <= pi/2 then 1 else -1
        d = if b < pi then 1 else -1

-- | Apply a function in a pair
applyPair :: (a->b) -> (a,a) -> (b,b)
applyPair f (a,b) = (f a, f b)

-- | calculate a point of a line specified by two points in the time t
interpolate :: (Double,Double) -> (Double,Double) -> Double -> (Double,Double)
interpolate (x0,y0) (x1,y1) t = (x,y)
  where
    x = x0 + t * (x1 - x0)
    y = y0 + t * (y1 - y0)
