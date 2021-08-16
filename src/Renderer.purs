module Renderer where

import Coordinates
import Prelude

import CanvasSupport (fromDataSource)
import Core as Core
import Data.Array as Array
import Data.Foldable (for_)
import Data.Int (fromNumber, toNumber)
import Data.List (sortBy, reverse)
import Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Number.Format (fixed, toStringWith)
import Debug (spy)
import Debugging (drawAllReflections, highlightHitboxes, outlineFootprint, traceActualTvSize)
import DegreeMath (cos)
import Effect (Effect)
import Graphics.Canvas (Context2D)
import Graphics.Canvas as Canvas
import Math ((%))
import Sprites as Sprites
import Types (AnchorPosition(..), ApplicationState, Degree, Sprite, SpriteID(..), SpriteMap(..), values, valuesL)
import Vector (Matrix2D, Vector, dist, rotate, (:**:), (:*:), (:+:), (:-:))

render :: Context2D -> ApplicationState -> Effect Unit
render ctx state = do 
  -- let r = spy "render: " (unsafeLookup Chair state.sprites).pos.x 
  Canvas.clearRect ctx {x: 0.0, y: 0.0, width: 900.0, height: 900.0}
  tilebackground ctx 
  renderFloor ctx state 
  renderWalls ctx state 
  -- drawLines ctx state 
  renderSprites ctx state
  -- highlightHitboxes ctx state
  for_ (values state.sprites) (outlineFootprint ctx)
  traceActualTvSize ctx state 
  -- drawFirstReflections ctx state 
  -- drawRearReflections ctx state 
  drawAllReflections ctx state
  Canvas.setFont ctx "30px arial black"
  Canvas.fillText ctx (toStringWith (fixed 2) (Core.fieldOfView state.sprites state.tvSpecs)) 100.0 100.0
  


-- | Tiles the background with an alternating 32x32 checkerboard pattern
tilebackground :: Context2D -> Effect Unit 
tilebackground ctx = do 
  -- let _ = spy "running?" true
  for_ (Array.range 0 38) \x -> 
    for_ (Array.range 0 38) \y -> do 
      let color = if y `mod` 2 == 0
                  then if x `mod` 2 == 0 then "#D8D8D8" else "#EBEBEB" 
                  else if x `mod` 2 == 0 then "#EBEBEB" else "#D8D8D8"
      Canvas.setFillStyle ctx color
      Canvas.fillRect ctx {x: (toNumber x) * 32.0, y: (toNumber y) *32.0, width: 32.0, height: 32.0} 


renderSprites :: Context2D -> ApplicationState -> Effect Unit 
renderSprites ctx state = do 
  for_ (reverse (sortBy (comparing (_.pos.x)) (valuesL state.sprites))) \s -> 
    let sprt = Core.anchorAdjusted s
    in if sprt.enabled then drawSprite ctx sprt else pure unit 

  Canvas.setFillStyle ctx "red"
  for_ (values state.sprites) \s -> 
    let ss = ((16.0 :*: s.pos) :**: isoTransform) :+: {x: 448.0, y: 250.0} 
    in Canvas.fillRect ctx {x: ss.x, y: ss.y, width: 2.0, height: 2.0}


renderFloor :: Context2D -> ApplicationState -> Effect Unit 
renderFloor ctx {geometry} = do 
  Canvas.setLineWidth ctx 1.0
  let width = guaranteedInt geometry.width
  let depth = guaranteedInt geometry.depth
  for_ (Array.range 0 (width - 1)) \x -> 
    for_ (Array.range 0 (depth - 1)) \y -> do 
      Canvas.setStrokeStyle ctx "black"
      strokeIso3 ctx (toNumber x) (toNumber y) 


-- | TODO: refactor me once graphics are complete and larger sprites 
-- | are available.  Currenlty builds the walls "by hand" 
renderWalls :: Context2D -> ApplicationState -> Effect Unit 
renderWalls ctx {geometry} = do 
  let width = guaranteedInt geometry.width
      depth = guaranteedInt geometry.depth
      s = Core.anchorOrigin (Sprites.blockSprite AdHoc){anchor=LogicalOrigin, pos={x: 0.0, y: 0.0}}
  for_ [0, -1, -2, -3] \yOffset ->
    for_ (Array.range (-1) (depth - 1)) \y -> do 
      drawSprite ctx (Core.anchorOrigin s{pos={x: toNumber ((-1) + yOffset), y: (toNumber (y + yOffset))}})
  
  for_ [0, -1, -2, -3] \yOffset ->
    for_ (Array.range 0 (width - 1)) \x -> do 
      drawSprite ctx (Core.anchorOrigin s{pos={x: (toNumber (x + yOffset)), y: toNumber ((-1) + yOffset)}})


drawLines :: Context2D -> ApplicationState -> Effect Unit 
drawLines ctx state = do 
  -- for_ [30.0, -30.0, 120.0, -120.0] \deg -> do  
  --for_ [0.0, 30.0, -30.0, 90.0, -90.0, 180.0] \deg -> do  
  let SpriteMap sprites = state.sprites
      center = sprites.chair
      tv = sprites.tv 
      d = dist center.pos tv.pos 
      r = d / cos 30.0 
  for_ (Array.range 0 360) \degNum -> do  
    let deg = toNumber degNum
        qq = {x: 0.0, y: -r} 
        qq' = rotate qq deg 
        p = center.pos 
        o = qq' :+: p 
        pp = toIso p 
        oo = toIso o

    Canvas.fillRect ctx {x: (toIso p).x, y: (toIso p).y, width: 1.0, height: 1.0}
    Canvas.setLineWidth ctx 3.0 
    Canvas.beginPath ctx 
    Canvas.moveTo ctx pp.x pp.y  
    Canvas.lineTo ctx oo.x oo.y 
    Canvas.closePath ctx 
    Canvas.stroke ctx 



drawSprite :: Context2D -> Sprite -> Effect Unit 
drawSprite ctx sprite = do 
  -- let _ = spy "drawNormal?" $ sprite.image == sprite.images.normal
  _ <- Canvas.drawImage ctx (fromDataSource sprite.image) pos.x pos.y 
  pure unit 
  where 
  pos = ((16.0 :*: (sprite.pos)) :**: isoTransform) :+: {x: canvCenter, y: 250.0} 
  canvCenter = 448.0 


strokeIso3 :: Context2D -> Number -> Number -> Effect Unit 
strokeIso3 ctx x y = do 
  _ <- Canvas.beginPath ctx
  _ <- Canvas.moveTo ctx p1.x p1.y
  _ <- Canvas.lineTo ctx p1.x p1.y
  _ <- Canvas.lineTo ctx p2.x p2.y
  _ <- Canvas.lineTo ctx p3.x p3.y
  _ <- Canvas.lineTo ctx p4.x p4.y
  _ <- Canvas.lineTo ctx p1.x p1.y
  _ <- Canvas.closePath ctx 
  _ <- Canvas.stroke ctx
  pure unit
  where 
  pos = {x, y}
  f = \xx -> ((16.0 :*: xx) :**: isoTransform) :+: {x: 448.0, y: 250.0}
  p1 = f pos 
  p2 = f {x: pos.x+1.0, y: pos.y}
  p3 = f {x:pos.x+1.0, y: pos.y + 1.0}
  p4 = f {x:pos.x, y: pos.y + 1.0}



-- | Used when converting a known statically defined number 
-- | back to an integer for purposes of looping / comparison
guaranteedInt :: Number -> Int 
guaranteedInt x = fromMaybe 0 (fromNumber x)