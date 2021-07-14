module Core where 

import Prelude

import Data.Int (toNumber)
import Data.List (find, foldl)
import Data.Map as M
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..))
import Debug (spy)
import Math as Math
import ParseInt (parseBase10, parseInt)
import Types (ApplicationState, AudioChannels(..), Degree, FOV, FormID(..), Geometry, LocalPosition, Position, ScreenSize, Sprite, SpriteID(..), TvSpecs)
import Utils (unsafeLookup)
import Vector (Matrix2D, Vector, norm, (:**:), (:*:), (:+:), (:-:))

origin :: Position 
origin = {x: 0.0, y: 0.0}

parseChannel :: String -> Maybe AudioChannels
parseChannel s = case s of 
  "2.0" -> Just Channels2_0
  "2.1" -> Just Channels2_1
  "5.1" -> Just Channels5_1
  _ -> Nothing 


betweenZeroAndTwoHundred :: Int -> Maybe Int 
betweenZeroAndTwoHundred x = if x > 0 && x < 200 then Just x else Nothing 

parseRoomDimension :: String -> Maybe Int 
parseRoomDimension = parseBase10 .<<. betweenZeroAndTwoHundred


-- I can't figure out how to program generically against Records 
-- in the same way you would against plain Javascript Objects. 
-- I similarly haven't been able for figure out how to dynamically 
-- access a record by key (get Proxy doesn't seem to allow dynmic varables). 
-- Thus: the repeated boilerplate below. 
updateField :: ApplicationState -> FormID -> String -> ApplicationState
updateField state id value = case id of 
  Channels -> case (parseChannel value) of 
    Just _ -> state{form{channels{value=value}}}
    Nothing -> state{form{channels{error=Just "Invalid input"}}}
  Width -> case (parseRoomDimension value) of 
    Just width -> state{form{roomWidth{value=width}}, geometry{width=(toNumber width), center{x=((toNumber width) / 2.0)}}}
    Nothing -> state{form{roomWidth{error=Just "Must be a valid number!"}}}
  Depth -> case (parseRoomDimension value) of 
    Just width -> state{form{roomDepth{value=width}}, geometry{depth=(toNumber width)}}
    Nothing -> state{form{roomDepth{error=Just "Must be a valid number!"}}}


spriteInBounds :: LocalPosition -> Sprite -> Boolean 
spriteInBounds pos s = inBounds4 pos s

-- screenSize 
-- aspectRatio 

forScreenSize :: Geometry -> TvSpecs -> Geometry
forScreenSize currentGeometry tv = currentGeometry{center{y=units}, radius=radius / 16.0}
  where 
  {screenSize, aspectRatio} = spy "tv:" tv
  goo = spy "input:" (aspectRatio.height / aspectRatio.width )
  diagonalDegrees = spy "diagonal:" $ atan (aspectRatio.height / aspectRatio.width )
  screenWidth = spy "screenWidth: " $ (cos diagonalDegrees) * screenSize 

  fov = 20.0 
  distance = spy "distance:" $ (screenWidth / 2.0) / (tan fov)
  radius = spy "radius:" $ distance / (cos 30.0)
  units = spy "units: " $ distance / 16.0






setDragging :: LocalPosition -> Sprite -> Sprite 
setDragging pos s = s{isBeingDragged=true, clickOffset=(localToIso pos) :-: s.pos}
  where 
  _ = spy "[cart] cursor pos: " [pos.x, pos.y]
  _ = spy "[Iso] cursor pos: " [(localToIso pos).x, (localToIso pos).y]
  _ = spy "[Iso] sprite pos: " [s.pos.x, s.pos.y]


handleMouseDown :: ApplicationState -> LocalPosition -> ApplicationState 
handleMouseDown state cursorPos = find (spriteInBounds cursorPos) (M.values state.sprites) 
    # map (setDragging cursorPos)
    # case _ of 
      Just updatedSprite -> state{sprites=M.insert updatedSprite.id updatedSprite state.sprites} 
      Nothing -> state 
  where 
  _ = spy "found?" $ find (spriteInBounds cursorPos) (M.values state.sprites) 

handleMouseMove :: ApplicationState -> LocalPosition -> ApplicationState 
handleMouseMove state pos = repositionSprites $ (updateState state pos)


updateSprites :: (Sprite -> Sprite) -> ApplicationState -> ApplicationState
updateSprites f state = state{sprites=map f state.sprites}


updateSprites_ :: Sprite -> (Sprite -> Sprite) -> ApplicationState -> ApplicationState
updateSprites_ sprite_ f state = updateSprites (\s -> if s.id == target.id then f s else s) state 
  where target = sprite_

updateState :: ApplicationState -> Vector -> ApplicationState
updateState state v = res 
  where 
  sprites = Map.values state.sprites
  res = foldl (\acc s -> updateFuck acc (unsafeLookup s.id acc.sprites) v) state sprites 
  _ = (unsafeLookup Chair res.sprites).isBeingDragged

repositionSprites :: ApplicationState -> ApplicationState 
repositionSprites state = state{sprites=Map.union updates state.sprites}
  where 
  sprites = state.sprites
  geometry = state.geometry 
  distToTv = (cos 30.0) * geometry.radius
  tvPos = geometry.center :-: {x: 0.0, y: distToTv}
  updates = Map.fromFoldable [
    Tuple Chair $ centerXY (unsafeLookup Chair sprites){pos=geometry.center},
    Tuple TV    $ centerXY (unsafeLookup TV sprites){pos=tvPos},
    -- Tuple Center    $ centerXY (unsafeLookup Center sprites){pos=geometry.center :-: {x: 0.0, y: geometry.radius}},
    Tuple LeftFront $ positionSprite (unsafeLookup LeftFront sprites) geometry (-30.0),
    Tuple RightFront $ positionSprite (unsafeLookup RightFront sprites) geometry (30.0),
    Tuple RightRear $ positionSprite (unsafeLookup RightRear sprites) geometry (110.0),
    Tuple LeftRear $ positionSprite (unsafeLookup LeftRear sprites) geometry (-110.0)
    -- Tuple LeftRear $ positionSprite (unsafeLookup RightFront sprites) geometry (90.0)
  ]

positionSprite :: Sprite -> Geometry -> Number -> Sprite 
positionSprite sprite {center, radius} degrees = centerXY sprite{pos=targetPos}
  where 
  forwardVector = {x: 0.0, y: -radius} 
  rotatedVector = rotate forwardVector degrees 
  targetPos = rotatedVector :+: center


updateFuck :: ApplicationState -> Sprite -> Vector -> ApplicationState
updateFuck state s v = case s.id of 
  -- Chair -> onHoverr v s state
  -- Chair -> ((onHoverr `cc` onDragdd)) v s state
  Chair -> ((onHoverr `cc` onDragdd) `cc` recenterGeometry) v s state
  -- LeftFront -> onHoverr v s state
  _ -> ((onHoverr `cc` onDragdd) `cc` translateGeometry) v s state


cc :: (LocalPosition -> Sprite -> ApplicationState -> ApplicationState) 
   -> (LocalPosition -> Sprite -> ApplicationState -> ApplicationState) 
   -> (LocalPosition -> Sprite -> ApplicationState -> ApplicationState)
cc f g = (\cursor sprite state -> 
  let state' = f cursor sprite state 
  in g cursor (unsafeLookup sprite.id state'.sprites) state')


onHoverr :: Vector -> Sprite -> ApplicationState -> ApplicationState
onHoverr cursorPos s state = state{sprites=Map.insert s.id updatedSprite state.sprites}
  where 
  -- _ = if s.id == Chair then spy "chair in bounds?" (inBounds3 cursorPos s) else false 
  updatedSprite = if (inBounds4 cursorPos s) 
                  then s{image=s.images.hover} 
                  else s{image=s.images.normal}
  -- _ = spy "being dragged?" updatedSprite.isBeingDragged

center :: Sprite -> Vector 
center s = s.pos :+: {x: s.size.x / 2.0, y: 0.0}

recenterGeometry :: LocalPosition -> Sprite -> ApplicationState -> ApplicationState
recenterGeometry _ s state = if s.isBeingDragged then state{geometry{center=(anchorXY s).pos, radius=newRadius}} else state 
  where 
  cc = (center s)
  {center, radius} = state.geometry
  length = (Math.cos (toRadians 30.0)) * radius
  d = center :-: (anchorXY s).pos 
  newLength = length + (-d.y)
  newRadius = newLength / (Math.cos (toRadians 30.0)) --cc.y / (Math.cos (toRadians 30.0))
  -- _ = if s.isBeingDragged then Trace.spy "new radius: " newRadius else newRadius 

dist :: Vector -> Vector -> Number 
dist v1 v2 = Math.sqrt $ (Math.pow (v2.x - v1.x) 2.0) + (Math.pow (v2.y - v1.y) 2.0)

translateGeometry :: LocalPosition -> Sprite -> ApplicationState -> ApplicationState
translateGeometry cursorPos s state = state 
-- | below is current commented out because I need the delta from the 
-- | drag calculation available for recomputing the geometry, which I don't 
-- | have in this function. The architecture here is bad. I need state, state-1 to 
-- | make decisions
-- translateGeometry cursorPos s state = if s.isBeingDragged then state{geometry{center=newCenter}} else state 
--   where 
--   {center, radius, width} = state.geometry
--   ay = {x: 0.0, y: ((localToIso cursorPos) :-: s.clickOffset).y} 
--   by = {x: 0.0, y: s.pos.y}
--   d = dist ay by 
--   -- _ = if s.isBeingDragged then spy "dist: " [ay.y, by.y, d] else []
--   -- _ = if s.isBeingDragged then spy "sprite: " [s.pos.x, s.pos.y] else []
--   -- _ = if s.isBeingDragged then spy "Cursor: " [isoPos.x, isoPos.y] else []
--   -- _ = if s.isBeingDragged then spy "Cursor2: " [((localToIso pos) :-: s.pos).x, ((localToIso pos) :-: s.pos).y] else []
--   length = (Math.cos (toRadians 30.0)) * radius
--   newCenter = center{y=(s.pos.y + (s.size.y / 2.0)) + length, x = width / 2.0}


anchorXY :: Sprite -> Sprite 
anchorXY s = s{pos=s.pos :+: (0.5 :*: {x: s.size.x, y: s.size.y})} 

-- this handles dragging + updating the main geometry (and thus the other sprites) 
-- need to figure out how to break these apart. We need the change info in order 
-- to be able to compute the geometry
onDragdd :: LocalPosition -> Sprite -> ApplicationState -> ApplicationState
onDragdd cursorPos s state = state{sprites=updatedSprites, geometry=newCenter}
  where 
  isDrag = s.isBeingDragged
  isChair = s.id == Chair 
  nextPos = (localToIso cursorPos) :-: s.clickOffset
  leftRear = unsafeLookup LeftRear state.sprites 
  leftFront = unsafeLookup LeftFront state.sprites
  -- _ = if isDrag then spy "nextPos:" [nextPos.x, nextPos.y] else []
  heading = norm $ nextPos :-: s.pos 
  forward = heading.y > 0.0
  delta = dist {x: 0.0, y: nextPos.y} {x: 0.0, y: s.pos.y}
  deltaDirection = if forward then delta else -delta
  xConstrainedPos = {x: s.pos.x, y: nextPos.y} 
  ---FUUUUCUUUUCCCKKK 
  --- this geometry thing is broken by design 
  -- I need to be able to do collision detection on the sprites 
  -- which get moved in response to the current sprite. and THOSE don't 
  -- get moved until repositionSprites, so... yeah, I don't have enough info here 
  -- to do the collision detection. Arg. 
  -- unless I move repositionSprites here, but then this method does EVERYTHING... 
  -- ... ... 
  -- hm. 
  -- Not sure what to do. Everything _needs_ to know about everything else because moving 
  -- one thing affects all the things....... 
  updatedSprite = if s.isBeingDragged then s{pos=xConstrainedPos, image=s.images.hover} else s
  updatedSprites = Map.insert s.id updatedSprite state.sprites 
  newCenter = if isDrag && (not isChair) && (leftRear.pos.y < state.geometry.depth && leftFront.pos.y > -2.0)
              then state.geometry{center = {x: state.geometry.center.x, y: state.geometry.center.y + deltaDirection}} 
              else state.geometry
  -- _ = if isDrag then spy "distance: " delta else 0.0
  -- _ = if isDrag then spy "norm: " [heading.x, heading.y] else []



toRadians :: Degree -> Number 
toRadians x = x * (Math.pi / 180.0)

toDegrees :: Number -> Degree 
toDegrees rad = rad * (180.0 / Math.pi)

sin :: Degree -> Number 
sin = Math.sin <<< toRadians

cos :: Degree -> Number 
cos = Math.cos <<< toRadians 

tan :: Degree -> Number 
tan = Math.tan <<< toRadians

atan :: Degree -> Number 
atan = toDegrees <<< Math.atan


rotate :: Vector -> Degree -> Vector 
rotate v angle = v :**: a 
  where 
  a = {a1: cos angle,    a2: sin angle,
       b1: -(sin angle), b2: cos angle}


centerXY :: Sprite -> Sprite
centerXY s = s{pos=s.pos :-: (0.5 :*: {x: s.size.x, y: s.size.y})} 

anchorOrigin :: Sprite -> Sprite  
anchorOrigin s = s{pos = s.pos :-: {x: xOffset, y: yOffset}} 
  where 
  yOffset = s.size.z - (s.size.y * 0.5) 
  xOffset = s.size.y + yOffset 


anchorBottom :: Sprite -> Sprite 
anchorBottom s = s{pos=s.pos :-: offset} 
  where
  offsetX = (s.size.y * 0.5) + s.size.z 
  offsetY = (s.size.y * 0.5) + s.size.z 
  offset = {x: offsetX, y: offsetY}


anchorCenterSouth :: Sprite -> Sprite 
anchorCenterSouth s = s{pos=(anchorBottom s).pos :-: {x: s.size.x / 2.0, y: 0.0}}


anchorCenterEast :: Sprite -> Sprite 
anchorCenterEast s = s{pos = centerEast}
  where 
  bottomLeft = (anchorBottom s).pos
  bottomRight = bottomLeft :-: {x: s.size.x, y: 0.0}
  centerEast = bottomRight :+: {x: 0.0, y: s.size.y / 2.0}


anchorCenterNorth :: Sprite -> Sprite 
anchorCenterNorth s = s{pos=northCenter} 
  where 
  southCenter = (anchorCenterSouth s).pos 
  northCenter = southCenter :+: {x: 0.0, y: s.size.y}


anchorCenterWest :: Sprite -> Sprite 
anchorCenterWest s = s{pos=northCenter} 
  where 
  southCenter = (anchorBottom s).pos 
  northCenter = southCenter :+: {x: 0.0, y: s.size.y / 2.0}  


isoTransform :: Matrix2D
isoTransform = {a1:  1.0, a2: 0.5, 
                b1: -1.0, b2: 0.5}

cartTransform :: Matrix2D
cartTransform = {a1: 0.5, a2: -0.5, 
                 b1: 1.0, b2:  1.0}             



toIso :: Vector -> Vector 
toIso v = ((16.0 :*: (v)) :**: isoTransform) :+: {x: 448.0, y: 250.0} -- :+: {x : -16.0, y: 0.0}


localToIso :: LocalPosition -> Position 
localToIso p = (1.0/16.0) :*: ((p :-: {x: 448.0, y: 250.0}) :**: cartTransform )

inBounds4 :: LocalPosition -> Sprite -> Boolean 
inBounds4 cursorPos s = inBounds3 (localToIso cursorPos) s

inBounds3 :: Vector -> Sprite -> Boolean 
inBounds3 cursorPos s = inBounds2 cursorPos s{pos=s.pos :-: {x: s.size.z, y: s.size.z}}


inBounds2 :: Vector -> Sprite -> Boolean 
inBounds2 pos fullSprite = (pos.x > sprite.x
                        && pos.x < sprite.x + (width + depth)
                        && pos.y > sprite.y
                        && pos.y < sprite.y + (height + depth)
                        -- lower edge
                        && pos.y <= (pos.x * 1.0) + lowerIntercept
                        && pos.x >= (pos.y - lowerIntercept) / 1.0
                        -- upper edge 
                        && pos.y >= (pos.x * 1.0) + upperIntercept
                        && pos.x <= pos.y - upperIntercept / 1.0)
  where 
  height = fullSprite.size.y
  width = fullSprite.size.x
  depth = fullSprite.size.z
  sprite = fullSprite.pos
  -- y-mx=b 
  lowerIntercept =  (sprite.y + height) - sprite.x
  -- y-mx=b
  upperIntercept =  sprite.y - (sprite.x + width)

  s = sprite 
  p = pos
  -- _ = if fullSprite.id == Chair then spy "chair?" [s.x, s.y, p.x, p.y] else [-1.0, -1.0, -1.0, -1.0]


inBounds :: Vector -> Vector -> Boolean 
inBounds pos actor = pos.x > actor.x 
                      && pos.x < actor.x + 1.0 
                      && pos.y > actor.y 
                      && pos.y < actor.y + 1.0



-- | Compose two monadic functions together. 
-- | this surely exists, but I cannot find it :| 
compMonad :: forall a b c m. Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
compMonad f g = (\x -> (f x) >>= g)

infix 5 compMonad as .<<.
