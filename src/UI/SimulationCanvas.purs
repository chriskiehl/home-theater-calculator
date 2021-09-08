module SimulationCanvas where

import Prelude

import Data.Int (toNumber)
import Effect (Effect)
import Graphics.Canvas (CanvasElement)
import React.Basic (JSX)
import React.Basic.DOM (css)
import React.Basic.DOM as R
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler)
import Types (Action(..), ClientPosition, Position)
import Utils (canvasPosition, clientPosition)
import Vector (Vector)

type CanvasProps = {
    dispatch :: (Action -> Effect Unit)
}


mainCanvas :: CanvasProps -> JSX
mainCanvas {dispatch} = 
  R.div {
    style: css {position: "relative"},
    children: [
      R.canvas {
        id: "canvas", 
        width: "896", 
        height: "608",
        style: css {maxWidth: "100%"},
        onMouseDown: onMouseDown MouseDown,
        onMouseUp: onMouseDown MouseUp,
        onMouseMove: onMouseDown MouseMove
      },
      R.button {id: "zoom-button", onClick: handler identity \_ -> dispatch ToggleZoom}
    ]}
  where 
  onMouseDown eventType = handler canvasPosition \event -> do 
    position <- event 
    dispatch $ asType eventType position

asType :: (Vector -> Action) -> Position -> Action 
asType t pos = t pos