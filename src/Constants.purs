module Constants where

import Prelude 
import Types (Degree)
import Vector (Vector)

tileWidth :: Number 
tileWidth = 16.0

canvasWidth :: Number 
canvasWidth = 896.0

canvasHeight :: Number 
canvasHeight = 608.0

speakerAngle :: Degree
speakerAngle = 30.0

middleScreen :: Vector
middleScreen = {x: canvasWidth / 2.0, y: 250.0}


