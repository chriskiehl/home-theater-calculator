-- | Various tooling and utils for debugging 
-- | sprite intereactions and hitboxes 
module Debugging where

import Prelude

import Effect (Effect)
import Graphics.Canvas (Context2D)
import Types (Sprite)
import Vector ((:**:), (:*:), (:+:))

spriteSize = 16.0 

-- paintRed :: Context2D -> Number -> Number -> Effect Unit
-- paintRed ctx x y = do 
--   let v =  ((spriteSize :*: {x, y}) :**: isoTransform) :+: {x: 448.0, y: 250.0}
--   Canvas.setFillStyle ctx "red"
--   Canvas.fillRect ctx {x: v.x, y: v.y, width: 1.0, height: 1.0}

-- paintBlue :: Context2D -> Number -> Number -> Effect Unit 
-- paintBlue ctx x y = do 
--   let v =  ((spriteSize :*: {x, y}) :**: isoTransform) :+: {x: 448.0, y: 250.0}
--   Canvas.setFillStyle ctx "blue"          
--   Canvas.fillRect ctx {x: v.x, y: v.y, width: 1.0, height: 1.0}


-- highlightHitboxes :: 
--   -- shows how we're mapping the hitboxes for sprites   
--   -- let xs = stepRange (-1.0) 10.0 0.1 
--   -- let ys = stepRange (-2.0) 10.0 0.1 
--   -- for_ xs \x -> 
--   --   for_ ys \y -> 
--   --     if anyOfEm x y (Map.values entity.sprites) 
--   --         then paintRed ctx x y    
--   --         else paintBlue ctx x y    


-- anyOfEm :: Number -> Number -> List Sprite -> Boolean 
-- anyOfEm x y sprites = foldl (\acc val -> if val then val else acc) false aany
--   where 
--   sprites_ = map getSprite spriteWrappers
--   sprites_' = map (\x -> x{pos=x.pos :-: {x: x.size.z, y: x.size.z}}) sprites_
--   aany = map (inBounds2 {x, y}) sprites_'   