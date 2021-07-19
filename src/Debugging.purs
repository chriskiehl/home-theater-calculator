-- | Various tooling and utils for debugging 
-- | sprite intereactions and hitboxes 
module Debugging where

import Prelude

import Core (FrontReflection, ReflectionPoints, WallInteractionPoints, firstReflection, toIso)
import Core as Core
import Data.Foldable (foldl, for_)
import Data.List (List)
import Data.Map as Map
import Debug (spy)
import Effect (Effect)
import Graphics.Canvas (Context2D)
import Graphics.Canvas as Canvas
import Types (Sprite, SpriteID(..), ApplicationState)
import Utils (unsafeLookup)
import Vector ((:**:), (:*:), (:+:), (:-:))

spriteSize = 16.0 

paintRed :: Context2D -> Number -> Number -> Effect Unit
paintRed ctx x y = do 
  let v =  ((spriteSize :*: {x, y}) :**: Core.isoTransform) :+: {x: 448.0, y: 250.0}
  Canvas.setFillStyle ctx "red"
  Canvas.fillRect ctx {x: v.x, y: v.y, width: 1.0, height: 1.0}

paintBlue :: Context2D -> Number -> Number -> Effect Unit 
paintBlue ctx x y = do 
  let v =  ((spriteSize :*: {x, y}) :**: Core.isoTransform) :+: {x: 448.0, y: 250.0}
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
  Canvas.fillRect ctx {x: (Core.toIso bottomLeft).x, y: (Core.toIso bottomLeft).y, width: 2.0, height: 2.0}
  Canvas.fillRect ctx {x: (Core.toIso bottomRight).x, y: (Core.toIso bottomRight).y, width: 2.0, height: 2.0}
  Canvas.fillRect ctx {x: (Core.toIso topLeft).x, y: (Core.toIso topLeft).y, width: 2.0, height: 2.0}
  Canvas.fillRect ctx {x: (Core.toIso topRight).x, y: (Core.toIso topRight).y, width: 2.0, height: 2.0}

anyOfEm :: Number -> Number -> List Sprite -> Boolean 
anyOfEm x y sprites = foldl (\acc val -> if val then val else acc) false aany
  where 
  sprites_' = map (\x -> x{pos=x.pos }) sprites 
  aany = map (Core.inBoundsDebug {x, y}) sprites



traceActualTvSize :: Context2D -> ApplicationState -> Effect Unit 
traceActualTvSize ctx state = do 
  let {screenSize, aspectRatio} = state.tvSpecs 
  let leftFront = Core.footprint (unsafeLookup LeftFront state.sprites)
  let tv = (unsafeLookup TV state.sprites)
  let diagonalDegrees = Core.atan (aspectRatio.height / aspectRatio.width )
  let screenWidth = (Core.cos diagonalDegrees) * screenSize 
  let isoWidth = ((Core.cos 30.0) * screenWidth )
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



drawFirstReflections :: Context2D -> ApplicationState -> Effect Unit 
drawFirstReflections ctx state = do 
  let {center, radius} = (Core.computeGeometry state.sprites)
  let points = Core.firstReflection $ Core.figureOutPoints2 state.sprites state.geometry{center=center, radius=radius}
  let ePos = Core.toIso points.e 
  let fPos = Core.toIso points.f 
  let cPos = Core.toIso points.c 
  Canvas.beginPath ctx 
  Canvas.setStrokeStyle ctx "black"
  Canvas.setLineWidth ctx 5.0
  Canvas.moveTo ctx ePos.x ePos.y  
  Canvas.lineTo ctx fPos.x fPos.y 
  Canvas.lineTo ctx cPos.x cPos.y 
  Canvas.stroke ctx 
  Canvas.closePath ctx

  Canvas.beginPath ctx 
  Canvas.setStrokeStyle ctx "white"
  Canvas.setLineWidth ctx 3.0
  Canvas.moveTo ctx ePos.x ePos.y  
  Canvas.lineTo ctx fPos.x fPos.y 
  Canvas.lineTo ctx cPos.x cPos.y 
  Canvas.stroke ctx 
  Canvas.closePath ctx
  Canvas.setLineDash ctx []


drawRearReflections :: Context2D -> ApplicationState -> Effect Unit 
drawRearReflections ctx state = do 
  let {center, radius} = (Core.computeGeometry state.sprites)
  let points = Core.rearReflection $ Core.figureOutRearReflectionPoints state.sprites state.geometry{center=center, radius=radius}
  let ePos = Core.toIso points.e 
  let fPos = Core.toIso points.f 
  let cPos = Core.toIso points.c 
  Canvas.beginPath ctx 
  Canvas.setStrokeStyle ctx "black"
  Canvas.setLineWidth ctx 5.0
  Canvas.moveTo ctx ePos.x ePos.y  
  Canvas.lineTo ctx fPos.x fPos.y 
  Canvas.lineTo ctx cPos.x cPos.y 
  Canvas.stroke ctx 
  Canvas.closePath ctx

  Canvas.beginPath ctx 
  Canvas.setStrokeStyle ctx "white"
  
  Canvas.setLineWidth ctx 3.0
  Canvas.moveTo ctx ePos.x ePos.y  
  Canvas.lineTo ctx fPos.x fPos.y 
  Canvas.lineTo ctx cPos.x cPos.y 
  Canvas.stroke ctx 
  Canvas.closePath ctx
  Canvas.setLineDash ctx []

drawAllReflectionsBy :: (WallInteractionPoints -> FrontReflection) -> Context2D -> ApplicationState -> Effect Unit 
drawAllReflectionsBy f ctx state = do 
  let {center, radius} = (Core.computeGeometry state.sprites)
  let {firstReflection, secondReflection, thirdReflection} = f $ Core.collectReflectionPoints state.sprites state.geometry{center=center, radius=radius}
  let frs = Core.toIso firstReflection.source
  let frd = Core.toIso firstReflection.dest
  let frr = Core.toIso firstReflection.reflection

  let srs = Core.toIso secondReflection.source
  let srd = Core.toIso secondReflection.dest
  let srr = Core.toIso secondReflection.reflection

  let trs = Core.toIso thirdReflection.source
  let trd = Core.toIso thirdReflection.dest
  let trr = Core.toIso thirdReflection.reflection
  
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
drawLeftReflections = drawAllReflectionsBy Core.leftReflections

drawRightReflections :: Context2D -> ApplicationState -> Effect Unit  
drawRightReflections = drawAllReflectionsBy Core.rightReflections

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
