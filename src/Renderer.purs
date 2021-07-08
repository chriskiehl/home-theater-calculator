module Renderer where

import Prelude

import CanvasSupport (fromDataSource)
import Core (anchorCenterEast, anchorCenterSouth, anchorCenterWest)
import Data.Array as Array
import Data.Foldable (for_)
import Data.Int (fromNumber, toNumber)
import Data.Map as M
import Data.Maybe (fromMaybe)
import Debug (spy)
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Refs
import Graphics.Canvas (Context2D)
import Graphics.Canvas as Canvas
import Math ((%))
import Math as Math
import Sprites as Sprites
import Trig (toRadians)
import Types (ApplicationState, Degree, Sprite, SpriteID(..))
import Utils (unsafeLookup)
import Vector (Matrix2D, Vector, (:**:), (:*:), (:+:), (:-:))

render :: Context2D -> ApplicationState -> Effect Unit
render ctx state = do 
  Canvas.clearRect ctx {x: 0.0, y: 0.0, width: 900.0, height: 900.0}
  tilebackground ctx 
  renderFloor ctx state 
  renderSprites ctx state
  -- drawLines ctx state 


-- | Tiles the background with an alternating 32x32 checkerboard pattern
tilebackground :: Context2D -> Effect Unit 
tilebackground ctx = do 
  -- let _ = spy "running?" true
  for_ (Array.range 0 38) \x -> 
    for_ (Array.range 0 38) \y -> do 
      let color = if (toNumber y) % 2.0 == 0.0
                  then if (toNumber x) % 2.0 == 0.0 then "#D8D8D8" else "#EBEBEB" 
                  else if (toNumber x) % 2.0 == 0.0 then "#EBEBEB" else "#D8D8D8"
      Canvas.setFillStyle ctx color
      Canvas.fillRect ctx {x: (toNumber x) * 32.0, y: (toNumber y) *32.0, width: 32.0, height: 32.0} 


renderSprites :: Context2D -> ApplicationState -> Effect Unit 
renderSprites ctx state = do 
  -- let _ = spy "renderSpritesBefore?" $ (unsafeLookup Chair state.sprites).image == (unsafeLookup Chair state.sprites).images.normal
  for_ (M.values state.sprites) \s -> 
    let sprt = s
        -- cdd  = spy "renderSpritesNormal?" $ s.image == s.images.normal 
    in if sprt.id == TV 
      then pure unit 
      else drawSprite ctx sprt{pos=sprt.pos :-: sprt.originOffset}
    -- in drawSprite ctx sprt{pos=(sprt.pos :-: sprt.originOffset)}


renderFloor :: Context2D -> ApplicationState -> Effect Unit 
renderFloor ctx {geometry} = do 
  Canvas.setLineWidth ctx 1.0
  let width = fromMaybe 0 (fromNumber geometry.width)
  let depth = fromMaybe 0 (fromNumber geometry.depth)
  for_ (Array.range 0 (width - 1)) \x -> 
    for_ (Array.range 0 (depth - 1)) \y -> do 
      -- if y == 0
      -- then do 
      --   drawSprite ctx state.chair{pos=({x: (toNumber x), y: (toNumber y) - 1.0} :-: state.chair.originOffset)}
      --   drawSprite ctx state.chair{pos=({x: (toNumber x) - 1.0, y: (toNumber y) - 2.0} :-: state.chair.originOffset)}
      --   drawSprite ctx state.chair{pos=({x: (toNumber x) - 2.0, y: (toNumber y) - 3.0} :-: state.chair.originOffset)}
      --   drawSprite ctx state.chair{pos=({x: (toNumber x) - 3.0, y: (toNumber y) - 4.0} :-: state.chair.originOffset)}
      --   drawSprite ctx state.chair{pos=({x: (toNumber x) - 4.0, y: (toNumber y) - 5.0} :-: state.chair.originOffset)}
      -- else pure unit 
      
      -- if x == 0
      -- then do 
      --   drawSprite ctx state.chair{pos=({x: (toNumber x) - 1.0, y: (toNumber y) - 0.0} :-: state.chair.originOffset)}
      --   drawSprite ctx state.chair{pos=({x: (toNumber x) - 2.0, y: (toNumber y) - 1.0} :-: state.chair.originOffset)}
      --   drawSprite ctx state.chair{pos=({x: (toNumber x) - 3.0, y: (toNumber y) - 2.0} :-: state.chair.originOffset)}
      --   drawSprite ctx state.chair{pos=({x: (toNumber x) - 4.0, y: (toNumber y) - 3.0} :-: state.chair.originOffset)}
      --   drawSprite ctx state.chair{pos=({x: (toNumber x) - 5.0, y: (toNumber y) - 4.0} :-: state.chair.originOffset)}
      -- else pure unit 



      -- drawSprite ctx state.chair{pos=({x: toNumber x, y: toNumber y} :-: state.chair.originOffset)}
      
      Canvas.setStrokeStyle ctx "black"
      strokeIso3 ctx (toNumber x) (toNumber y) 


drawLines :: Context2D -> ApplicationState -> Effect Unit 
drawLines ctx state = do 
  -- for_ [30.0, -30.0, 120.0, -120.0] \deg -> do  
  --for_ [0.0, 30.0, -30.0, 90.0, -90.0, 180.0] \deg -> do  
  for_ (Array.range 0 360) \degNum -> do  
    let deg = toNumber degNum
    let qq = {x: 0.0, y: -state.geometry.radius} 
    let qq' = rotate qq deg 
    let p = state.geometry.center 
    let o = qq' :+: p 
    let pp = toIso p 
    let oo = toIso o

    let controlSprite = Sprites.tvSprite{id=LeftFront, pos={x:0.0, y: 0.0}}

    let centerize = p :-: (0.5 :*: {x: Sprites.blockSprite.size.x, y: Sprites.blockSprite.size.y})
    let tempChair = Sprites.blockSprite{id=Chair, pos=centerize}

    let anglePos = o -- :-: {x: 4.0, y: 3.0}   -- (0.5 :*: {x: twoStackSprite.size.x, y: twoStackSprite.size.y})
    let tmpSpeakerA = Sprites.twoStackSprite{id=LeftFront, pos=anglePos}
    -- let tmpSpeakerA = twoStackSprite{id=LeftFront, pos=anglePos}

    -- experiment begin


    -- drawSprite ctx (anchorCenterWest controlSprite) -- {pos=controlSprite.pos :-:controlSprite.originOffset}
    -- drawSprite ctx (anchorCenterSouth controlSprite) -- {pos=controlSprite.pos :-:controlSprite.originOffset}
    -- drawSprite ctx (anchorCenterEast controlSprite) -- {pos=controlSprite.pos :-:controlSprite.originOffset}
    -- drawSprite ctx (anchorCenterWest controlSprite) -- {pos=controlSprite.pos :-:controlSprite.originOffset}
    -- drawSprite ctx tmpSpeakerA{pos=tmpSpeakerA.pos}
    -- experiment end 

    -- let _ = Trace.spy "chair" tempChair.pos 
    -- let _ = Trace.spy "speaker" anglePos 

    Canvas.fillRect ctx {x: (toIso p).x, y: (toIso p).y, width: 1.0, height: 1.0}
    Canvas.setLineWidth ctx 3.0 
    Canvas.beginPath ctx 
    Canvas.moveTo ctx pp.x pp.y  
    Canvas.lineTo ctx oo.x oo.y 
    Canvas.closePath ctx 
    Canvas.stroke ctx 

    drawSprite ctx tempChair{pos=tempChair.pos :-: tempChair.originOffset}
    if deg == 30.0 || deg == (330.0)
    then drawSprite ctx (anchorCenterSouth tmpSpeakerA)
    else pure unit 

    if deg == 120.0
    then drawSprite ctx (anchorCenterWest tmpSpeakerA)
    else pure unit 

    if deg == (360.0 - 120.0)
    then drawSprite ctx (anchorCenterEast tmpSpeakerA)
    else pure unit 


drawSprite :: Context2D -> Sprite -> Effect Unit 
drawSprite ctx sprite = do 
  -- let _ = spy "drawNormal?" $ sprite.image == sprite.images.normal
  _ <- Canvas.drawImage ctx (fromDataSource sprite.image) pos.x pos.y 
  pure unit 
  where 
  pos = ((16.0 :*: (sprite.pos)) :**: isoTransform) :+: {x: canvCenter, y: 250.0} 
  canvCenter = 448.0


isoTransform :: Matrix2D
isoTransform = {a1:  1.0, a2: 0.5, 
                b1: -1.0, b2: 0.5}


sin :: Degree -> Number 
sin = Math.sin <<< toRadians 

cos :: Degree -> Number 
cos = Math.cos <<< toRadians 


rotate :: Vector -> Degree -> Vector 
rotate v angle = v :**: a 
  where 
  a = {a1: cos angle,    a2: sin angle,
       b1: -(sin angle), b2: cos angle}


toIso :: Vector -> Vector 
toIso v = ((16.0 :*: (v)) :**: isoTransform) :+: {x: 448.0, y: 250.0} -- :+: {x : -16.0, y: 0.0}       


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