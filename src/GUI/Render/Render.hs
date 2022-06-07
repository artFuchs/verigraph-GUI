module GUI.Render.Render (
  Render
, renderWithContext
) where

import qualified GI.Cairo.Render.Connector as C
import GI.Cairo.Render (Render)

renderWithContext context render = C.renderWithContext render context
