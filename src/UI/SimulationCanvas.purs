module SimulationCanvas where

import Prelude

import Data.Int (toNumber)
import Effect (Effect)
import React.Basic (JSX)
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
  R.div {className: "", children: [
    R.canvas {
      id: "canvas", 
      width: "896", 
      height: "608",
      onMouseDown: handler canvasPosition \event -> dispatch $ asType MouseDown event,
      onMouseUp: handler canvasPosition \event -> dispatch $ asType MouseUp event,
      onMouseMove: handler canvasPosition \event -> dispatch $ asType MouseMove event 
    } 
  ]}

asType :: (Vector -> Action) -> Position -> Action 
asType t pos = t pos