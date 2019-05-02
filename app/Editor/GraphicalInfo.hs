module Editor.GraphicalInfo
( GraphicalInfo (..)
, GIPos (..)
, GIDim (..)
, GIColor (..)
, NodeShape (..)
, NodeGI (..)
, EdgeStyle (..)
, EdgeGI (..)
, getNodeGI
, getEdgeGI
, newNodeGI
, newEdgeGI
, nodeGiSetPosition
, nodeGiSetColor
, nodeGiSetLineColor
, nodeGiSetDims
, nodeGiSetShape
, edgeGiSetPosition
, edgeGiSetColor
, edgeGiSetStyle
)where

import qualified Data.Map as M
import Data.Maybe

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
-- cPosition - position of the control points.
-- color - line and label color.
-- style - specifies how the edge is rendered: if as a solid line, as a slashed line or as a pointed line.
data EdgeGI = EdgeGI { cPosition :: GIPos
                     , color :: GIColor
                     , style :: EdgeStyle
                     } deriving (Eq, Show, Read)
-- contrutores
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

-- methods to "modify" a nodeGI
nodeGiSetPosition :: (Double, Double) -> NodeGI -> NodeGI
nodeGiSetPosition pos gi = NodeGI { position = pos
                              , fillColor = fillColor gi
                              , lineColor = lineColor gi
                              , dims = dims gi
                              , shape = shape gi
                              }

nodeGiSetColor :: (Double, Double, Double) -> NodeGI -> NodeGI
nodeGiSetColor col gi = NodeGI { position = position gi
                               , fillColor = col
                               , lineColor = lineColor gi
                               , dims = dims gi
                               , shape = shape gi
                               }

nodeGiSetLineColor :: (Double, Double, Double) -> NodeGI -> NodeGI
nodeGiSetLineColor col gi = NodeGI  { position = position gi
                                    , lineColor = col
                                    , fillColor = fillColor gi
                                    , dims = dims gi
                                    , shape = shape gi
                                    }

nodeGiSetDims :: (Double,Double) -> NodeGI -> NodeGI
nodeGiSetDims d gi = NodeGI { dims = d
                        , position = position gi
                        , fillColor = fillColor gi
                        , lineColor = lineColor gi
                        , shape = shape gi
                        }

nodeGiSetShape :: NodeShape -> NodeGI -> NodeGI
nodeGiSetShape s gi = NodeGI { position = position gi
                             , fillColor = fillColor gi
                             , lineColor = lineColor gi
                             , dims = dims gi
                             , shape = s
                             }

-- methods to "modify" an edgeGI
edgeGiSetPosition :: (Double,Double) -> EdgeGI -> EdgeGI
edgeGiSetPosition pos gi = EdgeGI { cPosition = pos
                                  , color = color gi
                                  , style = style gi
                                  }

edgeGiSetColor :: (Double,Double,Double) -> EdgeGI -> EdgeGI
edgeGiSetColor col gi = EdgeGI { cPosition = cPosition gi
                                  , color = col
                                  , style = style gi
                                  }

edgeGiSetStyle :: EdgeStyle -> EdgeGI -> EdgeGI
edgeGiSetStyle s gi = EdgeGI { cPosition = cPosition gi
                             , color = color gi
                             , style = s
                             }
