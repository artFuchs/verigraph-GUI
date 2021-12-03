-- module containing functions related to geometry

module GUI.Helper.Geometry(
  pointDistance
, pointLineDistance
, addPoint
, subPoint
, vectorBetween
, multPoint
, multPointScalar
, midPoint
, pointsToRectangle
, pointInsideRectangle
, circleOverlapsCircle
, rectangleOverlapsRectangle
, circleOverlapsRectangle
, pointAt
, toPolarFrom
, angle
, quadrant
, interpolate
, normalizeVector
, vectorLength
, dotProduct
) where

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

-- | subtract one point from another, getting the vector between them
subPoint :: (Double,Double) -> (Double,Double) -> (Double,Double)
subPoint (a,b) (c,d) = (a-c,b-d)

-- | alias for subPoint
vectorBetween = subPoint

-- | multiply two points
multPoint :: (Double,Double) -> (Double,Double) -> (Double,Double)
multPoint (a,b) (c,d) = (a*c,b*d)

-- | multiply point by a scalar number
multPointScalar :: (Double,Double) -> Double -> (Double,Double)
multPointScalar (a,b) s = (a*s,b*s)

-- | calculates the center of two points
midPoint :: (Double,Double) -> (Double,Double) -> (Double,Double)
midPoint (x1,y1) (x2,y2) = (x1 + (x2-x1)/2, y1 + (y2-y1)/2)

-- | given an angle, distance and a initial point, calculates a new point
pointAt :: Double -> Double -> (Double,Double) -> (Double,Double)
pointAt ang dist (x,y) = (x + dist*cos(ang), y + dist*sin(ang))

-- | given a origin point O and a point P, converts P to polar cordinates
toPolarFrom :: (Double,Double) -> (Double,Double) -> (Double,Double)
toPolarFrom  (xRef,yRef) (x,y) =
  let ang  = angle (xRef,yRef) (x,y)
      dist = pointDistance  (xRef,yRef) (x,y)
  in (ang,dist)



-- | given two points, get the rectangle defined by them
-- a rectangle is defined by the tuple (x,y,w,h), where
-- x,y is the center of the rectangle
-- w is it's width
-- h is it's height
pointsToRectangle :: (Double,Double) -> (Double,Double) -> (Double,Double,Double,Double)
pointsToRectangle p1 p2 = (x,y,w,h)
  where
    (x,y) = midPoint p1 p2
    (w,h) = (\(a,b) -> (abs a, abs b)) $ subPoint p1 p2

-- | check if a point is inside a rectangle
pointInsideRectangle :: (Double,Double) -> (Double,Double,Double,Double) -> Bool
pointInsideRectangle (x,y) (rx,ry,rw,rh) = (abs (x - rx) <= rw/2) && (abs (y - ry) <= rh/2)

-- | check if a circle intersects with another circle
circleOverlapsCircle :: (Double,Double,Double) -> (Double,Double,Double) -> Bool
circleOverlapsCircle (x1,y1,r1) (x2,y2,r2) = pointDistance (x1,y1) (x2,y2) < (r1+r2)

-- | check if a rectangle intersects with another rectangle
rectangleOverlapsRectangle :: (Double,Double,Double,Double) -> (Double,Double,Double,Double) -> Bool
rectangleOverlapsRectangle r1@(x1,y1,w1,h1) r2@(x2,y2,w2,h2) =
  l1<r2 && r1>l2 && t1<b2 && b1>t2
  where
    l1 = x1-w1/2
    l2 = x2-w2/2
    r1 = l1 + w1
    r2 = l2 + w2
    t1 = y1-h1/2
    t2 = y2-h2/2
    b1 = t1+h1
    b2 = t2+h2



clamp :: Double -> Double -> Double -> Double
clamp v min max = case (v < min, v > max) of
                    (True,False)  -> min
                    (False,True)  -> max
                    _             -> v

-- | check if a circle overlaps or is overlapped by a rectangle
circleOverlapsRectangle :: (Double,Double,Double) -> (Double,Double,Double,Double) -> Bool
circleOverlapsRectangle c@(cx,cy,cr) r@(rx,ry,rw,rh) =
    pointInsideRectangle (cx,cy) r || pointDistance (cx,cy) (clampX,clampY) <= cr
  where
    clampX = clamp cx (rx-rw) (rx+rw)
    clampY = clamp cy (ry-rh) (ry+rh)

-- | angle between two points
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

-- | calculate a point of a line specified by two points in the time t
interpolate :: (Double,Double) -> (Double,Double) -> Double -> (Double,Double)
interpolate (x0,y0) (x1,y1) t = (x,y)
  where
    x = x0 + t * (x1 - x0)
    y = y0 + t * (y1 - y0)


normalizeVector :: (Double,Double) -> (Double,Double)
normalizeVector (x,y) = (x/l,y/l)
  where l = vectorLength (x,y)

vectorLength :: (Double,Double) -> Double
vectorLength (x,y) = sqrt $ x*x + y*y

dotProduct ::  (Double,Double) ->  (Double,Double)  -> Double
dotProduct (x0,y0) (x1,y1) = x0*x1 + y0*y1
