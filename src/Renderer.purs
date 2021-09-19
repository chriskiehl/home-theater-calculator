module Renderer where

import Coordinates
import Prelude

import CanvasSupport (fromDataSource)
import Constants (tileWidth)
import Coordinates as C
import Core (chairTvDistance)
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
import Graphcs (block, blockChunkie, bottomWallRev, wallCapLeft, wallCapRight)
import Graphics.Canvas (Context2D)
import Graphics.Canvas as Canvas
import Graphics.Canvas as Cavnas
import Math ((%))
import Reflections (collectInteractionPoints, leftReflections)
import Sprites as Sprites
import Types (AnchorPosition(..), ApplicationState, Degree, Footprint, Position, Sprite, SpriteID(..), SpriteMap(..), values, valuesL)
import Vector (Matrix2D, Vector, dist, rotate, (:**:), (:*:), (:+:), (:-:))

render :: Context2D -> ApplicationState -> Effect Unit
render ctx state = do 
  -- let r = spy "render: " (unsafeLookup Chair state.sprites).pos.x 
  Canvas.clearRect ctx {x: 0.0, y: 0.0, width: 900.0, height: 900.0}
  
  tilebackground ctx 
  renderFloor ctx state 
  renderWalls ctx state 
  drawCenterLine ctx state 
  -- drawLines ctx state 
  -- drawStudioLines ctx state 
  drawAllReflections ctx state
  renderSprites ctx state
  -- highlightHitboxes ctx state
  for_ (values state.sprites) (outlineFootprint ctx)
  
  -- drawFirstReflections ctx state 
  -- drawRearReflections ctx state 
  

  let uu = C.toIso {x: 0.0, y: 0.0} state.worldOrigin state.zoomMultiplier

  Canvas.setFont ctx "30px Arial"
  let ggg = ((tileWidth :*: {x: 0.0, y: 0.0}) :**: isoTransform) :+: state.worldOrigin
  let con = C.localToIso {x: 0.0, y: 0.0} state.worldOrigin
  -- Canvas.fillText ctx ("local: (" <> (show state.cursor.localPosition.x) <> ", " <> (show state.cursor.localPosition.y) <> ")")  600.0 30.0
  -- Canvas.fillText ctx ("iso: (" <> (toStringWith (fixed 2) state.cursor.isoPosition.x) <> ", " <> (toStringWith (fixed 2) state.cursor.isoPosition.y) <> ")")  600.0 60.0
  -- Canvas.fillText ctx ("iso: (" <> (show ggg.x) <> ", " <> (show ggg.y) <> ")")  600.0 90.0
  -- Canvas.fillText ctx ("round trip: " <> (show uu))  500.0 120.0

  

  -- Canvas.fillRect ctx {x: 800.0, y: 500.0, width: 64.0, height: 64.0}   
    
  let iii = C.localToIso state.worldOrigin state.worldOrigin   
      jjj = C.toIso {x: 0.0, y: 0.0} state.worldOrigin   
  -- Cavnas.fillRect ctx {x: ggg.x, y: ggg.y, width: 20.0, height: 20.0}  
  
  -- let sss = Core.anchorAdjusted (Sprites.bottomWallSprite AdHoc){anchor=LogicalOrigin, pos={x: 0.0, y: 0.0}}  
  -- drawSprite ctx sss state.worldOrigin state.zoomMultiplier  
  
  -- Cavnas.setFillStyle ctx "green"  
  -- Cavnas.fillRect ctx {x: 347.0, y: 238.0, width: 10.0, height: 10.0}  
    
  let (SpriteMap sm) = state.sprites  
    
  let currentCenter = sprites.chair 
  let tv = sprites.tv 
  let distance = (dist currentCenter.pos tv.pos) 
  let rad = distance / (cos 30.0) 

  let chairTvInterDistance = (Core.chairTvDistance state.sprites state.tvSpecs) / 16.0
  let pos = toIso sm.chair.pos state.worldOrigin state.zoomMultiplier 
  let width = (state.geometry.width / 2.0) - 1.0
  let radius = (width / (cos 20.0)) -- ~7.981 
  let maxDistance = radius * (cos 30.0)
  let oneTen = (rotate {x: 0.0, y: -radius} (-110.0)) :+: (sm.chair.pos) -- :+: {x: 1.0, y: 0.0})
  let oneTen' = toIso oneTen state.worldOrigin state.zoomMultiplier 

  let r30 = (rotate {x: 0.0, y: -radius} (30.0)) :+: (sm.chair.pos) -- :+: {x: 1.0, y: 0.0})
  let r30' = toIso r30 state.worldOrigin state.zoomMultiplier 
  let cc = toIso (sm.chair.pos :-: {x: 0.0, y: chairTvInterDistance}) state.worldOrigin state.zoomMultiplier 

  -- Cavnas.fillText ctx ("distance: " <> show distance) 100.0 100.0
  -- Cavnas.fillText ctx ("max dist: " <> show maxDistance) 100.0 130.0
  -- Cavnas.fillText ctx ("inter chair-tv: " <> show chairTvInterDistance) 100.0 160.0
  -- -- Cavnas.fillText ctx (show $ stats.distanceFromTv / 16.0) 100.0 220.0
  -- Cavnas.fillText ctx (show $ stats.maxDisplacement) 100.0 220.0

  Canvas.beginPath ctx 
  -- Canvas.moveTo ctx pos.x pos.y 
  -- Cavnas.setLineWidth ctx 1.0
  -- Cavnas.setStrokeStyle ctx "black"
  -- Cavnas.lineTo ctx oneTen'.x oneTen'.y
  -- Canvas.closePath ctx 
  -- Canvas.stroke ctx

  -- Canvas.moveTo ctx pos.x pos.y 
  -- Cavnas.setLineWidth ctx 1.0
  -- Cavnas.setStrokeStyle ctx "black"
  -- Cavnas.lineTo ctx r30'.x r30'.y
  -- Canvas.closePath ctx 
  -- Canvas.stroke ctx
  
  where 

  stats = Core.homeTheaterStats state
  (SpriteMap sprites) = state.sprites 
  {firstReflection, secondReflection, thirdReflection} = leftReflections $ collectInteractionPoints state.sprites state.geometry

-- renderDistances :: Context2D -> ApplicationState -> Effect Unit 
-- renderDistances ctx state = do 
--   let (SpriteMap sm) = state.sprites 
--   sm.rightFront
--   pure unit 





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
  for_ (sortBy (comparing (_.pos.x)) (valuesL state.sprites)) \s -> do
    let sprt = Core.anchorAdjusted s
    if sprt.enabled 
      then do 
        renderFootprint ctx s state
        drawSprite ctx sprt state.worldOrigin state.zoomMultiplier
      else pure unit 


  for_ (sortBy (comparing (_.pos.x)) (valuesL state.sprites)) \s -> do
    let sprt = Core.anchorAdjusted s    
    if sprt.isBeingDragged
      then do 
        Canvas.setFont ctx "30px 'Boxy-Bold'"
        Canvas.setFillStyle ctx "white"  
        Canvas.fillText ctx (show sprt.id) (toIso sprt.pos state.worldOrigin state.zoomMultiplier).x (toIso sprt.pos state.worldOrigin state.zoomMultiplier).y
      else pure unit 

  Canvas.setFillStyle ctx "red"
  for_ (values state.sprites) \s -> 
    let ss = ((tileWidth :*: s.pos) :**: isoTransform) :+: {x: 435.5, y: 295.5} 
    in Canvas.fillRect ctx {x: ss.x, y: ss.y, width: 2.0, height: 2.0}


renderFloor :: Context2D -> ApplicationState -> Effect Unit 
renderFloor ctx {geometry, worldOrigin, zoomMultiplier} = do 
  Canvas.setLineWidth ctx 1.0
  let width = guaranteedInt geometry.width
  let depth = guaranteedInt geometry.depth
  for_ (Array.range (-1) (width - 1)) \x -> 
    for_ (Array.range (-1) (depth - 1)) \y -> do 
      Canvas.setStrokeStyle ctx "black"
      strokeIso3 ctx (toNumber x) (toNumber y) worldOrigin zoomMultiplier
      let sss = Core.anchorAdjusted (Sprites.floorSprite AdHoc){anchor=LogicalOrigin, pos={x: toNumber (x + 1), y: toNumber (y + 1)}}  
      drawSprite ctx sss worldOrigin zoomMultiplier


-- | TODO: refactor me once graphics are complete and larger sprites 
-- | are available. Currenlty builds the walls "by hand" 
renderWalls :: Context2D -> ApplicationState -> Effect Unit 
renderWalls ctx {geometry, worldOrigin, zoomMultiplier} = do 
  let width = guaranteedInt geometry.width
      depth = guaranteedInt geometry.depth
      sss = Core.anchorAdjusted (Sprites.bottomWallSprite AdHoc){anchor=LogicalOrigin, pos={x: 0.0, y: 0.0}}  
      sss2 = Core.anchorAdjusted (Sprites.bottomWallSprite AdHoc){anchor=LogicalOrigin, pos={x: 0.0, y: 0.0}, image=wallCapRight}
      rightWall = Core.anchorAdjusted (Sprites.bottomWallSprite AdHoc){anchor=LogicalOrigin, pos={x: 0.0, y: 0.0}, image=bottomWallRev}  
  for_ (Array.range (-1) (depth - 1)) \y -> do 
    if y == -1 
      then drawSprite ctx (Core.anchorOrigin sss{pos={x: toNumber (-1), y: (toNumber (y ))}}) worldOrigin zoomMultiplier
      else drawSprite ctx (Core.anchorOrigin sss2{pos={x: toNumber (-1), y: (toNumber (y ))}}) worldOrigin zoomMultiplier
  
  for_ (Array.range 0 (width - 1)) \x -> do 
    drawSprite ctx (Core.anchorOrigin rightWall{pos={x: (toNumber (x)), y: toNumber ((-1))}}) worldOrigin zoomMultiplier


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
        pp = toIso p state.worldOrigin state.zoomMultiplier 
        oo = toIso o state.worldOrigin state.zoomMultiplier

    Canvas.fillRect ctx {x: (toIso p state.worldOrigin state.zoomMultiplier).x, y: (toIso p state.worldOrigin state.zoomMultiplier).y, width: 1.0, height: 1.0}
    Canvas.setLineWidth ctx 3.0 
    Canvas.beginPath ctx 
    Canvas.moveTo ctx pp.x pp.y  
    Canvas.lineTo ctx oo.x oo.y 
    Canvas.closePath ctx 
    Canvas.stroke ctx 

drawStudioLines :: Context2D -> ApplicationState -> Effect Unit 
drawStudioLines ctx state = do 
  let SpriteMap sprites = state.sprites
      center = sprites.chair
      tv = sprites.tv 
      d = dist center.pos tv.pos 
      r = d / cos 30.0 
  for_ (Array.range 330 390) \degNum -> do  
    let deg = toNumber degNum
        qq = {x: 0.0, y: -r} 
        qq' = rotate qq deg 
        p = center.pos 
        o = qq' :+: p 
        pp = toIso p state.worldOrigin state.zoomMultiplier
        oo = toIso o state.worldOrigin state.zoomMultiplier

    Canvas.fillRect ctx {x: (toIso p state.worldOrigin state.zoomMultiplier).x, y: (toIso p state.worldOrigin state.zoomMultiplier).y, width: 1.0, height: 1.0}
    Canvas.setLineWidth ctx 3.0 
    Canvas.beginPath ctx 
    Canvas.moveTo ctx pp.x pp.y  
    Canvas.lineTo ctx oo.x oo.y 
    Canvas.closePath ctx 
    Canvas.stroke ctx 



drawCenterLine :: Context2D -> ApplicationState -> Effect Unit 
drawCenterLine ctx state = do 
  let center = toIso {x: 0.0, y: state.geometry.depth / 2.0} state.worldOrigin state.zoomMultiplier
      end = toIso ({x: 0.0, y: state.geometry.depth / 2.0} :+: {x: state.geometry.width, y:0.0}) state.worldOrigin state.zoomMultiplier
  Canvas.beginPath ctx 
  Canvas.setStrokeStyle ctx "blue"
  Canvas.setLineWidth ctx 3.0  
  Canvas.moveTo ctx center.x center.y
  Canvas.lineTo ctx end.x end.y
  Canvas.stroke ctx 

drawSprite :: Context2D -> Sprite -> Position -> Number -> Effect Unit 
drawSprite ctx sprite worldOrigin zoomMultiplier = do 
  -- let _ = spy "drawNormal?" $ sprite.image == sprite.images.normal
  _ <- Canvas.drawImageScale ctx (fromDataSource sprite.image) pos.x pos.y (width * zoomMultiplier) (height * zoomMultiplier)
  pure unit 
  where 
  -- TODO: explain this scaling math... 
  tilesize = zoomMultiplier * tileWidth
  pos = ((tilesize :*: (sprite.pos)) :**: isoTransform) :+: worldOrigin
  width  = 16.0 + (sprite.size.x * 16.0)
  height = 8.0 + (sprite.size.x * 8.0) + (sprite.size.z * 16.0)


strokeIso3 :: Context2D -> Number -> Number -> Position -> Number -> Effect Unit 
strokeIso3 ctx x y worldOrigin zoomLevel = do 
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
  f = \xx -> (((tileWidth * zoomLevel) :*: xx) :**: isoTransform) :+: worldOrigin
  p1 = f pos 
  p2 = f {x: pos.x+1.0, y: pos.y}
  p3 = f {x:pos.x+1.0, y: pos.y + 1.0}
  p4 = f {x:pos.x, y: pos.y + 1.0}



-- | Used when converting a known statically defined number 
-- | back to an integer for purposes of looping / comparison
guaranteedInt :: Number -> Int 
guaranteedInt x = fromMaybe 0 (fromNumber x)


isoFootprint :: Footprint -> Position -> Number -> Footprint 
isoFootprint {topLeft, topRight, bottomLeft, bottomRight} worldOrigin zoomMultiplier = {
  bottomLeft: toIso (bottomLeft) worldOrigin  zoomMultiplier,
  bottomRight: toIso (bottomRight) worldOrigin  zoomMultiplier,
  topRight: toIso (topRight) worldOrigin  zoomMultiplier,
  topLeft: toIso (topLeft) worldOrigin  zoomMultiplier 
}


renderFootprint :: Context2D -> Sprite -> ApplicationState ->  Effect Unit 
renderFootprint ctx sprite {worldOrigin, zoomMultiplier, tvSpecs} = do 
  let {topLeft, topRight, bottomLeft, bottomRight} = if sprite.id == TV 
    then isoFootprint (Core.tvFootprint tvSpecs sprite) worldOrigin zoomMultiplier
    else isoFootprint (Core.footprint sprite) worldOrigin zoomMultiplier
  Canvas.beginPath ctx 
  Canvas.moveTo ctx bottomLeft.x bottomLeft.y 
  Canvas.lineTo ctx bottomRight.x bottomRight.y 
  Canvas.lineTo ctx topRight.x topRight.y 
  Canvas.lineTo ctx topLeft.x topLeft.y 
  Canvas.lineTo ctx bottomLeft.x bottomLeft.y 
  Canvas.setFillStyle ctx if sprite.isBeingHovered then "rgba(255, 255, 255, 1.0)" else "rgba(255, 255, 255, 0.2)"
  Canvas.setStrokeStyle ctx "black"
  Cavnas.setLineDash ctx [3.0]
  Cavnas.setLineWidth ctx 1.0
  Cavnas.fill ctx 
  Canvas.stroke ctx 
  Cavnas.closePath ctx
  Cavnas.setLineDash ctx [0.0]
