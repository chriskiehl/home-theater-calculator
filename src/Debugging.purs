-- | Various tooling and utils for debugging 
-- | sprite intereactions and hitboxes and other 
-- | random dev stuff. Just a dumping ground of ad-hoc 
-- | one-off, as-needed throw-away tooling. 
module Debugging where

import Prelude

import Coordinates (isoTransform, toIso)
import Core as Core
import Data.Array (any, fromFoldable)
import Data.Foldable (foldl, for_)
import Data.List (List)
import Data.Map as Map
import Debug (spy)
import DegreeMath (atan, cos)
import Effect (Effect)
import Graphics.Canvas (Context2D)
import Graphics.Canvas as Canvas
import Graphics.Canvas as Cavnas
import Reflections (collectInteractionPoints, leftReflections, rightReflections)
import Types (ApplicationState, PrimaryReflections, Sprite, SpriteID(..), SpriteMap(..), WallInteractionPoints, values, valuesL)
import Vector ((:**:), (:*:), (:+:), (:-:))

spriteSize = 16.0 

paintRed :: Context2D -> Number -> Number -> Effect Unit
paintRed ctx x y = do 
  let v =  ((spriteSize :*: {x, y}) :**: isoTransform) :+: {x: 448.0, y: 250.0}
  Canvas.setFillStyle ctx "red"
  Canvas.fillRect ctx {x: v.x, y: v.y, width: 1.0, height: 1.0}

paintBlue :: Context2D -> Number -> Number -> Effect Unit 
paintBlue ctx x y = do 
  let v =  ((spriteSize :*: {x, y}) :**: isoTransform) :+: {x: 448.0, y: 250.0}
  Canvas.setFillStyle ctx "blue"          
  Canvas.fillRect ctx {x: v.x, y: v.y, width: 1.0, height: 1.0}


stepRange :: Number -> Number -> Number -> Array Number 
stepRange start stop stepSize = if start > stop then [] else [start] <> stepRange (start + stepSize) stop stepSize

--   shows how we're mapping the hitboxes for sprites   
highlightHitboxes :: Context2D -> ApplicationState -> Effect Unit 
highlightHitboxes ctx state = do 
  let xs = stepRange (-4.0) 4.0 0.1 
  let ys = stepRange (-4.0) 4.0 0.1 
  for_ xs \x -> 
    for_ ys \y -> 
      if anyOfEm x y (valuesL state.sprites) 
        then paintRed ctx x y    
        else paintBlue ctx x y    


outlineFootprint :: Context2D -> Sprite -> Effect Unit 
outlineFootprint ctx s = do 
  let {topLeft, topRight, bottomLeft, bottomRight} = Core.footprint s 
  Canvas.setFillStyle ctx "purple"
  -- Canvas.fillRect ctx {x: (toIso bottomLeft).x, y: (toIso bottomLeft).y, width: 2.0, height: 2.0}
  -- Canvas.fillRect ctx {x: (toIso bottomRight).x, y: (toIso bottomRight).y, width: 2.0, height: 2.0}
  -- Canvas.fillRect ctx {x: (toIso topLeft).x, y: (toIso topLeft).y, width: 2.0, height: 2.0}
  -- Canvas.fillRect ctx {x: (toIso topRight).x, y: (toIso topRight).y, width: 2.0, height: 2.0}


anyOfEm :: Number -> Number -> List Sprite -> Boolean 
anyOfEm x y sprites = any (Core.inBounds {x, y}) (fromFoldable sprites) 



drawAllReflectionsBy :: (WallInteractionPoints -> PrimaryReflections) -> Context2D -> ApplicationState -> Effect Unit 
drawAllReflectionsBy f ctx state = do 
  let (SpriteMap sprites) = state.sprites 
  let {firstReflection, secondReflection, thirdReflection} = f $ collectInteractionPoints state.sprites state.geometry
  let frs = toIso firstReflection.source state.worldOrigin state.zoomMultiplier
  let frd = toIso firstReflection.dest state.worldOrigin state.zoomMultiplier
  let frr = toIso firstReflection.reflection state.worldOrigin state.zoomMultiplier

  let srs = toIso secondReflection.source state.worldOrigin state.zoomMultiplier
  let srd = toIso secondReflection.dest state.worldOrigin state.zoomMultiplier
  let srr = toIso secondReflection.reflection state.worldOrigin state.zoomMultiplier

  let trs = toIso thirdReflection.source state.worldOrigin state.zoomMultiplier
  let trd = toIso thirdReflection.dest state.worldOrigin state.zoomMultiplier
  let trr = toIso thirdReflection.reflection state.worldOrigin state.zoomMultiplier
  
  Canvas.beginPath ctx 
  Canvas.moveTo ctx frs.x frs.y  
  Canvas.lineTo ctx frr.x frr.y 
  Canvas.lineTo ctx frd.x frd.y 
  Cavnas.stroke ctx 
  Canvas.closePath ctx

  Canvas.beginPath ctx
  Canvas.moveTo ctx srs.x srs.y  
  Canvas.lineTo ctx srr.x srr.y 
  Canvas.lineTo ctx srd.x srd.y 
  Cavnas.stroke ctx 
  Canvas.closePath ctx

  Canvas.beginPath ctx
  Canvas.moveTo ctx trs.x trs.y  
  Canvas.lineTo ctx trr.x trr.y 
  Canvas.lineTo ctx trd.x trd.y 
  Canvas.stroke ctx 
  Canvas.closePath ctx

  pure unit 
  


drawLeftReflections :: Context2D -> ApplicationState -> Effect Unit  
drawLeftReflections = drawAllReflectionsBy leftReflections

drawRightReflections :: Context2D -> ApplicationState -> Effect Unit  
drawRightReflections = drawAllReflectionsBy rightReflections

drawAllReflections :: Context2D -> ApplicationState -> Effect Unit  
drawAllReflections ctx state = do 
  Canvas.setStrokeStyle ctx "black"
  Canvas.setLineWidth ctx 5.0
  drawLeftReflections ctx state 
  Canvas.setStrokeStyle ctx "white"
  Canvas.setLineWidth ctx 3.0
  drawLeftReflections ctx state 
  
  Canvas.setStrokeStyle ctx "black"
  Canvas.setLineWidth ctx 5.0
  drawRightReflections ctx state
  Canvas.setStrokeStyle ctx "white"
  Canvas.setLineWidth ctx 3.0
  drawRightReflections ctx state
