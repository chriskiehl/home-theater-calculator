-- | Various tooling and utils for debugging 
-- | sprite intereactions and hitboxes and other 
-- | random dev stuff 
module Debugging where

import Prelude

import Coordinates (isoTransform, toIso)
import Core as Core
import Data.Foldable (foldl, for_)
import Data.List (List)
import Data.Map as Map
import Debug (spy)
import DegreeMath (atan, cos)
import Effect (Effect)
import Graphics.Canvas (Context2D)
import Graphics.Canvas as Canvas
import Reflections (collectInteractionPoints, leftReflections, rightReflections)
import Types (ApplicationState, Sprite, SpriteID(..), WallInteractionPoints, PrimaryReflections)
import Utils (unsafeLookup)
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
      if anyOfEm x y (Map.values state.sprites) 
        then paintRed ctx x y    
        else paintBlue ctx x y    


outlineFootprint :: Context2D -> Sprite -> Effect Unit 
outlineFootprint ctx s = do 
  let {topLeft, topRight, bottomLeft, bottomRight} = Core.footprint s 
  Canvas.setFillStyle ctx "purple"
  Canvas.fillRect ctx {x: (toIso bottomLeft).x, y: (toIso bottomLeft).y, width: 2.0, height: 2.0}
  Canvas.fillRect ctx {x: (toIso bottomRight).x, y: (toIso bottomRight).y, width: 2.0, height: 2.0}
  Canvas.fillRect ctx {x: (toIso topLeft).x, y: (toIso topLeft).y, width: 2.0, height: 2.0}
  Canvas.fillRect ctx {x: (toIso topRight).x, y: (toIso topRight).y, width: 2.0, height: 2.0}

anyOfEm :: Number -> Number -> List Sprite -> Boolean 
anyOfEm x y sprites = foldl (\acc val -> if val then val else acc) false aany
  where 
  sprites_' = map (\x -> x{pos=x.pos }) sprites 
  aany = map (Core.inBounds {x, y}) sprites



traceActualTvSize :: Context2D -> ApplicationState -> Effect Unit 
traceActualTvSize ctx state = do 
  let {screenSize, aspectRatio} = state.tvSpecs 
  let leftFront = Core.footprint (unsafeLookup LeftFront state.sprites)
  let tv = (unsafeLookup TV state.sprites)
  let diagonalDegrees = atan (aspectRatio.height / aspectRatio.width )
  let screenWidth = (cos diagonalDegrees) * screenSize 
  let isoWidth = ((cos 30.0) * screenWidth )
  let halfIsoWidth = (isoWidth / 2.0) / 16.0
  let pos = toIso (tv.pos)
  let pos2 = toIso (tv.pos :-: {x: halfIsoWidth, y: 0.0})
  Canvas.setFillStyle ctx "orange"
  Canvas.setStrokeStyle ctx "orange"
  Canvas.fillRect ctx {x: pos.x, y: pos.y, width: 2.0, height: 2.0}
  Canvas.beginPath ctx 
  Canvas.moveTo ctx pos.x pos.y
  Canvas.lineTo ctx pos2.x pos2.y 
  Canvas.stroke ctx 
  pure unit 




drawAllReflectionsBy :: (WallInteractionPoints -> PrimaryReflections) -> Context2D -> ApplicationState -> Effect Unit 
drawAllReflectionsBy f ctx state = do 
  let {firstReflection, secondReflection, thirdReflection} = f $ collectInteractionPoints state.sprites state.geometry
  let frs = toIso firstReflection.source
  let frd = toIso firstReflection.dest
  let frr = toIso firstReflection.reflection

  let srs = toIso secondReflection.source
  let srd = toIso secondReflection.dest
  let srr = toIso secondReflection.reflection

  let trs = toIso thirdReflection.source
  let trd = toIso thirdReflection.dest
  let trr = toIso thirdReflection.reflection
  
  Canvas.beginPath ctx 
  -- Canvas.setLineWidth ctx 3.0
  Canvas.moveTo ctx frs.x frs.y  
  Canvas.lineTo ctx frr.x frr.y 
  Canvas.lineTo ctx frd.x frd.y 

  Canvas.moveTo ctx srs.x srs.y  
  Canvas.lineTo ctx srr.x srr.y 
  Canvas.lineTo ctx srd.x srd.y 

  Canvas.moveTo ctx trs.x trs.y  
  Canvas.lineTo ctx trr.x trr.y 
  Canvas.lineTo ctx trd.x trd.y 

  Canvas.stroke ctx 
  Canvas.closePath ctx


drawLeftReflections :: Context2D -> ApplicationState -> Effect Unit  
drawLeftReflections = drawAllReflectionsBy leftReflections

drawRightReflections :: Context2D -> ApplicationState -> Effect Unit  
drawRightReflections = drawAllReflectionsBy rightReflections

drawAllReflections :: Context2D -> ApplicationState -> Effect Unit  
drawAllReflections ctx state = do 
  Canvas.setStrokeStyle ctx "red"
  Canvas.setLineWidth ctx 3.0
  drawLeftReflections ctx state 
  Canvas.setStrokeStyle ctx "white"
  Canvas.setLineWidth ctx 2.0
  drawLeftReflections ctx state 
  
  Canvas.setStrokeStyle ctx "green"
  Canvas.setLineWidth ctx 3.0
  drawRightReflections ctx state
  Canvas.setStrokeStyle ctx "white"
  Canvas.setLineWidth ctx 2.0
  drawRightReflections ctx state
