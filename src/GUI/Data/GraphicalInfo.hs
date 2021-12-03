module GUI.Data.GraphicalInfo(
  GraphicalInfo (..)
, GIPos
, GIDim
, GIColor
, NodeShape (..)
, NodeGI (..)
, EdgeStyle (..)
, EdgeGI (..)
, getNodeGI
, getEdgeGI
, newNodeGI
, newEdgeGI
, getEGIPosition
)where



import qualified Data.Map as M
import Data.Maybe
import GUI.Helper.Geometry



type GraphicalInfo = (M.Map Int NodeGI, M.Map Int EdgeGI)

getNodeGI :: Int -> M.Map Int NodeGI -> NodeGI
getNodeGI nid giM = fromMaybe newNodeGI $ M.lookup nid giM

getEdgeGI :: Int -> M.Map Int EdgeGI -> EdgeGI
getEdgeGI eid giM = fromMaybe newEdgeGI $ M.lookup eid giM

type GIPos = (Double,Double)
type GIDim = (Double,Double)
type GIColor = (Double,Double,Double)


data NodeShape = NCircle | NRect | NSquare deriving (Eq ,Show, Read)

-- data struct to draw a node.
-- contains:
-- position - the position where the no is draw in the canvas
-- fillColor - the color to fill the node with
-- lineColor - the color to draw the line surrounding the node and it's label
-- dims - dimensions of the node, it's used differently according with the shape of the node
-- shape - specifies if the node is drawn as a Circle, Rectangle or Square
data NodeGI = NodeGI { position :: GIPos
                     , fillColor :: GIColor
                     , lineColor :: GIColor
                     , dims :: GIDim
                     , shape :: NodeShape
                     } deriving (Eq, Show, Read)

data EdgeStyle = ENormal | EPointed | ESlashed deriving (Eq ,Show, Read)

-- data struct to draw an edge.
-- contains:
-- cPosition - position of the control point.
-- color - line and label color.
-- style - specifies how the edge is rendered: if as a solid line, as a slashed line or as a pointed line.
data EdgeGI = EdgeGI { cPosition :: GIPos
                     , color :: GIColor
                     , style :: EdgeStyle
                     } deriving (Eq, Show, Read)

-- constructors with default values
newNodeGI :: NodeGI
newNodeGI = NodeGI  { position = (0,0)
                    , fillColor = (1,1,1)
                    , lineColor = (0,0,0)
                    , dims = (20,20)
                    , shape = NCircle
                    }

newEdgeGI :: EdgeGI
newEdgeGI = EdgeGI { cPosition = (0,0)
                   , color = (0,0,0)
                   , style = ENormal
                   }

-- get edge position in cartesian coordinate system
getEGIPosition :: EdgeGI -> NodeGI -> NodeGI -> (Double,Double)
getEGIPosition egi srcgi tgtgi =
 if srcPos == tgtPos
   then pointAt ae de srcPos
   else pointAt (ae+ang) de pmid
 where
   (ae,de) = cPosition egi
   srcPos = position srcgi
   tgtPos = position tgtgi
   pmid = midPoint srcPos tgtPos
   (ang,dist) = toPolarFrom srcPos tgtPos
