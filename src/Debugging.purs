-- | Various tooling and utils for debugging 
-- | sprite intereactions and hitboxes 
module Debugging where

import Prelude

import Core as Core
import Data.Foldable (foldl, for_)
import Data.List (List)
import Data.Map as Map
import Debug (spy)
import Effect (Effect)
import Graphics.Canvas (Context2D)
import Graphics.Canvas as Canvas
import Types (Sprite, ApplicationState)
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