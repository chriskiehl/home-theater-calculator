module Core where 

import Prelude

import Data.Int (toNumber)
import Data.List (find, foldl)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Debug (spy)
import DegreeMath (atan, cos, sin, tan)
import Math as Math
import ParseInt (parseBase10)
import Types (AnchorPosition(..), ApplicationState, AspectRatio, AudioChannels(..), Degree, FOV, FormID(..), Geometry, LocalPosition, Position, Sprite, SpriteID(..), SpriteMap, TvSpecs)
import Utils (unsafeLookup, (.<<.))
import Vector (Matrix2D, Vector, (:**:), (:*:), (:+:), (:-:))

{-

baselineXPosition :: ApplicationState -> ApplicationState 


updateField :: ApplicationState -> FormID -> String -> ApplicationState
handleMouseDown :: ApplicationState -> LocalPosition -> ApplicationState 
handleMouseMove :: ApplicationState -> LocalPosition -> ApplicationState 
updateSprites :: (Sprite -> Sprite) -> ApplicationState -> ApplicationState

updateState :: ApplicationState -> Vector -> ApplicationState
updateFuck :: ApplicationState -> Sprite -> Vector -> ApplicationState

fooBarBazChaz :: Sprite -> LocalPosition -> State -> State

-}


handleMouseDown :: ApplicationState -> LocalPosition -> ApplicationState 
handleMouseDown state cursorPos = state{sprites=updateWhen (spriteInBounds cursorPos) (setDragging cursorPos) state.sprites}

handleMouseMove :: ApplicationState -> LocalPosition -> ApplicationState 
handleMouseMove state pos = (updateState2 state pos) --repositionSprites $ (updateState state pos)

handleMouseUp :: ApplicationState -> ApplicationState 
handleMouseUp state = state{sprites=map (\s -> s{isBeingDragged=false, image=s.images.normal}) state.sprites} 


origin :: Position 
origin = {x: 0.0, y: 0.0}

parseChannel :: String -> Maybe AudioChannels
parseChannel s = case s of 
  "2.0" -> Just Channels2_0
  "2.1" -> Just Channels2_1
  "5.1" -> Just Channels5_1
  _ -> Nothing 


parseAspectRatio :: String -> Maybe AspectRatio
parseAspectRatio ratio = case ratio of 
  "16:9" -> Just {width: 16.0, height: 9.0}
  "2.4:1" -> Just {width: 2.4, height: 1.0}
  _ -> Nothing

betweenZeroAndTwoHundred :: Int -> Maybe Int 
betweenZeroAndTwoHundred x = if x > 0 && x < 200 then Just x else Nothing 

parseRoomDimension :: String -> Maybe Int 
parseRoomDimension = parseBase10 .<<. betweenZeroAndTwoHundred


-- I can't figure out how to program generically against Records 
-- in the same way you would against plain Javascript Objects / maps. 
-- I similarly haven't been able for figure out how to dynamically 
-- access a record by key (get Proxy doesn't seem to allow dynmic varables). 
-- Thus: the repeated boilerplate below. 
updateField :: ApplicationState -> FormID -> String -> ApplicationState
updateField state id value = case id of 
  SimulationMode -> state{form{mode{value=value}}} 
  Channels -> case (parseChannel value) of 
    Just _ -> state{form{channels{value=value}}}
    Nothing -> state{form{channels{error=Just "Invalid input"}}}
  Width -> case (parseRoomDimension value) of 
    Just width -> state{form{roomWidth{value=width}}, geometry{width=(toNumber width)}}
    Nothing -> state{form{roomWidth{error=Just "Must be a valid number!"}}}
  Depth -> case (parseRoomDimension value) of 
    Just depth -> state{form{roomDepth{value=depth}}, geometry{depth=(toNumber depth)}}
    Nothing -> state{form{roomDepth{error=Just "Must be a valid number!"}}}
  ScreenSize -> case (parseBase10 value) of 
    Just newSize -> state{form{screenSize{value=newSize}}, tvSpecs{screenSize=toNumber newSize}}
    Nothing -> state{form{screenSize{error=Just "Must be a valid number"}}}
  AspectRatio -> case (parseAspectRatio value) of 
    Just newRatio -> state{form{aspectRatio{value=value}}, tvSpecs{aspectRatio=newRatio}}
    Nothing -> state{form{aspectRatio{error=Just "Unknown aspect ratio"}}}



baselineXPosition :: ApplicationState -> ApplicationState 
baselineXPosition state = state{sprites=nextSprites} 
  where 
  delta = (state.geometry.width / 2.0) - (unsafeLookup Chair state.sprites).pos.x 
  nextSprites = translateSprites state.sprites {x: delta, y: 0.0}


forScreenSize :: Geometry -> TvSpecs -> Geometry
forScreenSize currentGeometry tv = currentGeometry{center{y=units}, radius=radius / 16.0}
  where 
  {screenSize, aspectRatio} = spy "tv:" tv
  goo = spy "input:" (aspectRatio.height / aspectRatio.width )
  diagonalDegrees = spy "diagonal:" $ atan (aspectRatio.height / aspectRatio.width )
  screenWidth = spy "screenWidth: " $ (cos diagonalDegrees) * screenSize 

  fov = 20.0 
  distance = (screenWidth / 2.0) / (tan fov)
  radius = distance / (cos 30.0)
  units =  distance / 16.0


-- | computes the horizontal width of the screen from 
-- | the diagonal and aspect ratio. 
screenWidth :: TvSpecs -> Number 
screenWidth {screenSize, aspectRatio} = (cos diagonalDegrees) * screenSize 
  where 
  diagonalDegrees = atan (aspectRatio.height / aspectRatio.width) 

 
    
setDragging :: LocalPosition -> Sprite -> Sprite  
setDragging pos s = s{isBeingDragged=true, clickOffset=(localToIso pos) :-: s.pos}   


updateSprites :: (Sprite -> Sprite) -> ApplicationState -> ApplicationState
updateSprites f state = state{sprites=map f state.sprites}


updateWhen :: (Sprite -> Boolean) -> (Sprite -> Sprite) -> SpriteMap -> SpriteMap 
updateWhen predicate f = map (\s -> if (predicate s) then (f s) else s)


updateState2 :: ApplicationState -> LocalPosition -> ApplicationState 
updateState2 state cursor = (handleHover cursor nextState) 
  where 
  nextState = find isBeingDragged state.sprites 
    # map (updateFuck2 state cursor) 
    # fromMaybe state 



updateFuck :: ApplicationState -> Sprite -> Vector -> ApplicationState
updateFuck state s v = case s.id of 
  Chair -> let ss = (onHoverr v s state) in onCenterDrag v (unsafeLookup s.id ss.sprites) ss 
  -- LeftFront -> onHoverr v s state
  _ -> (onHoverr `cc` onDragdd) v s state
  -- _ -> state
  where 
  _ = 1234 

updateFuck2 :: ApplicationState -> Vector -> Sprite -> ApplicationState
updateFuck2 state v s = case s.id of 
  Chair -> onCenterDrag v s state 
  -- LeftFront -> onHoverr v s state
  _ -> (onHoverr `cc` onDragdd) v s state
  -- _ -> state
  where 
  _ = 1234 


handleHover :: LocalPosition -> ApplicationState -> ApplicationState 
handleHover cursorPos state = state{sprites=map (onHover2 cursorPos) state.sprites}

-- handleMove :: LocalPosition -> ApplicationState -> ApplicationState 
-- handleMove cursorPos state = state 
--   where 
--   _ = updateWhen isBeingDragged  (updateFuck )


isBeingDragged :: Sprite -> Boolean 
isBeingDragged = _.isBeingDragged

spriteInBounds :: LocalPosition -> Sprite -> Boolean 
spriteInBounds pos s = inBoundsDebug (localToIso pos) s


withinBoundaries :: Geometry -> SpriteMap -> Boolean 
withinBoundaries {width, depth} sprites = rearInBounds && frontInBounds 
 -- foldl (\acc s -> acc && s.pos.y > 0.0 && s.pos.y <= depth && s.pos.x > 0.0) true []
  where 
  leftRear = footprint (unsafeLookup LeftRear sprites)
  leftFront = footprint (unsafeLookup LeftFront sprites)
  frontInBounds = leftFront.topLeft.y >= 0.0 && leftFront.topLeft.x >= 0.0
  rearInBounds = leftRear.bottomLeft.y <= depth && leftRear.bottomLeft.x >= 0.0
  -- ssprites = leftRear.x > 0.0 && leftRear.y + leftRear
  _ = foldl (\acc s -> s.pos.y >= -0.0 && s.pos.y <= depth) true (Map.values sprites)


isCollidingWithTv :: TvSpecs -> SpriteMap -> Boolean 
isCollidingWithTv tv sprites = leftFront.bottomRight.x >= tvSprite.bottomLeft.x
  where 
  {screenSize, aspectRatio} = tv
  leftFront = footprint (unsafeLookup LeftFront sprites)
  tvSprite = footprint (unsafeLookup TV sprites)
  diagonalDegrees = atan (aspectRatio.height / aspectRatio.width )
  screenWidth = (cos diagonalDegrees) * screenSize 
  isoWidth = (cos 30.0) * screenWidth / 16.0


footprint :: Sprite -> {topLeft :: Position, topRight :: Position, bottomLeft :: Position, bottomRight :: Position}
footprint s = {topLeft, topRight, bottomLeft, bottomRight}   
  where 
  bottomLeft = case s.anchor of 
    CenterNorth -> {x: s.pos.x - (s.size.x / 2.0), y: s.pos.y + s.size.y}
    CenterSouth -> {x: s.pos.x - (s.size.x / 2.0), y: s.pos.y}
    CenterEast  -> {x: s.pos.x - s.size.x, y: s.pos.y + s.size.y / 2.0}
    CenterWest  -> {x: s.pos.x, y: s.pos.y + s.size.y / 2.0}
    _ -> {x: -1.0, y: -1.0}
  topLeft = bottomLeft :+: {x: 0.0, y: -s.size.y}
  topRight = topLeft :+: {x: s.size.x, y: 0.0}
  bottomRight = bottomLeft :+: {x: s.size.x, y: 0.0}


computeGeometry :: SpriteMap -> {center :: Position, radius :: Number}
computeGeometry sprites = {center: chair.pos, radius: hypotenuse}
  where 
  tv = unsafeLookup TV sprites 
  chair = unsafeLookup Chair sprites 
  distance = dist chair.pos tv.pos
  horizontalDistance = (tan (30.0 * Math.pi / 180.0)) * distance
  hypotenuse =  Math.sqrt $ (distance*distance) + (horizontalDistance * horizontalDistance)


positionSprite :: Sprite -> Geometry -> Number -> Sprite 
positionSprite sprite {center, radius} degrees = sprite{pos=targetPos}
  where 
  forwardVector = {x: 0.0, y: -radius} 
  rotatedVector = rotate forwardVector degrees 
  targetPos = rotatedVector :+: center


cc :: (LocalPosition -> Sprite -> ApplicationState -> ApplicationState) 
   -> (LocalPosition -> Sprite -> ApplicationState -> ApplicationState) 
   -> (LocalPosition -> Sprite -> ApplicationState -> ApplicationState)
cc f g = (\cursor sprite state -> 
  let state' = f cursor sprite state 
  in g cursor (unsafeLookup sprite.id state'.sprites) state')


onHoverr :: LocalPosition -> Sprite -> ApplicationState -> ApplicationState
onHoverr cursorPos s state = state{sprites=Map.insert s.id updatedSprite state.sprites}
  where 
  -- _ = if s.id == Chair then spy "chair in bounds?" (inBounds3 cursorPos s) else false 
  updatedSprite = if (inBoundsDebug (localToIso cursorPos) s) 
                  then s{image=s.images.hover} 
                  else s{image=s.images.normal}
  -- _ = spy "being dragged?" updatedSprite.isBeingDragged


dist :: Vector -> Vector -> Number 
dist v1 v2 = Math.sqrt $ (Math.pow (v2.x - v1.x) 2.0) + (Math.pow (v2.y - v1.y) 2.0)

translateGeometry :: LocalPosition -> Sprite -> ApplicationState -> ApplicationState
translateGeometry cursorPos s state = state 


fieldOfView :: SpriteMap -> TvSpecs -> FOV 
fieldOfView sprites specs = angle * 2.0
  where 
  halfScreenWidth = (screenWidth specs) / 2.0 
  center = unsafeLookup Chair sprites
  tv = unsafeLookup TV sprites
  distance = (dist center.pos tv.pos) * 16.0
  angle = atan (halfScreenWidth / distance)
  


fromConfig ::ApplicationState -> ApplicationState
fromConfig state = state{sprites=updates}
  where 
  sprites = state.sprites 
  {width, depth} = state.geometry
  screenHorizontal = screenWidth state.tvSpecs 
  fov = 20.0 
  distance = (screenHorizontal / 2.0) / (tan fov)
  radius = (distance / (cos 30.0)) / 16.0
  units = distance / 16.0

  center = {x: width/2.0, y: units + 1.0}
  tv = {x: width / 2.0, y: 1.0}

  distToTv = dist center tv
  -- radius = distToTv / (cos 30.0)
  -- distToTv = (cos 30.0) * geometry.radius
  -- tvPos = geometry.center :-: {x: 0.0, y: distToTv}
  updates = Map.fromFoldable [
    Tuple Chair (unsafeLookup Chair sprites){pos=center},
    Tuple TV    (unsafeLookup TV sprites){pos=tv},
    -- Tuple Center    $ centerXY (unsafeLookup Center sprites){pos=geometry.center :-: {x: 0.0, y: geometry.radius}},
    Tuple LeftFront $ positionSpriteNEW (unsafeLookup LeftFront sprites) {center, radius} (-30.0),
    Tuple RightFront $ positionSpriteNEW (unsafeLookup RightFront sprites) {center, radius} (30.0),
    Tuple RightRear $ positionSpriteNEW (unsafeLookup RightRear sprites) {center, radius} (110.0),
    Tuple LeftRear $ positionSpriteNEW (unsafeLookup LeftRear sprites) {center, radius} (-110.0)
  ]    


layoutSprites :: SpriteMap -> Geometry -> SpriteMap 
layoutSprites sprites geometry = Map.union updates sprites
  where 
  -- chair = (unsafeLookup Chair sprites)
  -- tv = (unsafeLookup TV sprites)
  -- distToTv = dist chair.pos tv.pos
  distToTv = (cos 30.0) * geometry.radius
  tvPos = geometry.center :-: {x: 0.0, y: distToTv}
  updates = Map.fromFoldable [
    Tuple Chair $ (unsafeLookup Chair sprites){pos=geometry.center},
    Tuple TV    $ (unsafeLookup TV sprites){pos=tvPos},
    -- Tuple Center    $ centerXY (unsafeLookup Center sprites){pos=geometry.center :-: {x: 0.0, y: geometry.radius}},
    Tuple LeftFront $ positionSprite (unsafeLookup LeftFront sprites) geometry (-30.0),
    Tuple RightFront $ positionSprite (unsafeLookup RightFront sprites) geometry (30.0),
    Tuple RightRear $ positionSprite (unsafeLookup RightRear sprites) geometry (110.0),
    Tuple LeftRear $ positionSprite (unsafeLookup LeftRear sprites) geometry (-110.0)
  ]

recenterSprites :: SpriteMap -> Vector -> SpriteMap 
recenterSprites sprites delta = Map.union updates sprites 
  where 
  currentCenter = (unsafeLookup Chair sprites) 
  tv = (unsafeLookup TV sprites) 
  newCenter = currentCenter.pos :+: delta 
  distance = (dist newCenter tv.pos)
  radius = distance / (cos 30.0) 
  updates = Map.fromFoldable [
    Tuple Chair $ (unsafeLookup Chair sprites){pos=newCenter},
    -- Tuple TV    $ (unsafeLookup TV sprites),
    -- Tuple Center    $ centerXY (unsafeLookup Center sprites){pos=geometry.center :-: {x: 0.0, y: geometry.radius}},
    Tuple LeftFront $ positionSpriteNEW (unsafeLookup LeftFront sprites) {center: newCenter, radius} (-30.0),
    Tuple RightFront $ positionSpriteNEW (unsafeLookup RightFront sprites) {center: newCenter, radius} (30.0),
    Tuple RightRear $ positionSpriteNEW (unsafeLookup RightRear sprites) {center: newCenter, radius} (110.0),
    Tuple LeftRear $ positionSpriteNEW (unsafeLookup LeftRear sprites) {center: newCenter, radius} (-110.0)
  ]


positionSpriteNEW :: Sprite -> {center::Position, radius:: Number} -> Number -> Sprite 
positionSpriteNEW sprite {center, radius} degrees = sprite{pos=targetPos}
  where 
  forwardVector = {x: 0.0, y: -radius} 
  rotatedVector = rotate forwardVector degrees 
  targetPos = rotatedVector :+: center


translateSprites :: SpriteMap -> Vector -> SpriteMap 
translateSprites sprites v = map (\s -> s{pos=s.pos :+: v}) sprites



onCenterDrag :: LocalPosition -> Sprite -> ApplicationState -> ApplicationState
onCenterDrag cursorPos s state = refactoredState
-- onCenterDrag cursorPos s state = if isDrag && (withinBoundaries state.geometry oopdated) && (not (isCollidingWithTv state.tvSpecs oopdated))
--                                  then state{sprites=oopdated, geometry=newCenter} 
--                                  else state 
  where 
  isDrag = s.isBeingDragged 
  -- isChair = s.id == Chair  
  -- -- _ = spy "is chair drag?" [isDrag, isChair]
  -- nextPos = (localToIso cursorPos) :-: s.clickOffset 
  -- delta = dist {x: 0.0, y: nextPos.y} {x: 0.0, y: s.pos.y} 
  -- xConstrainedPos = {x: s.pos.x, y: nextPos.y} 
  -- heading = norm $ nextPos :-: s.pos  
  -- forward = heading.y > 0.0 
  -- deltaDirection = if forward then delta else -delta 

  -- updatedChair = if s.isBeingDragged then s{pos=xConstrainedPos, image=s.images.hover} else s
  -- tv = unsafeLookup TV state.sprites 

  -- distance = dist updatedChair.pos tv.pos 
  -- -- _ = spy "distance?" distance 
  -- -- _ = spy "d?" [xConstrainedPos.x, xConstrainedPos.y, state.geometry.center.x, state.geometry.center.y]
  -- newRadius = distance / cos 30.0

  -- newCenter = if isDrag && isChair
  --             then state.geometry{center = xConstrainedPos, radius=newRadius} 
  --             else state.geometry
  -- -- newGeo = if isDrag then recenterGeometry2 s newCenter else state.geometry 
  -- oopdated = if isDrag then (layoutSprites state.sprites newCenter) else state.sprites
  -- -- ooopdated = Map.insert Chair ( if s.isBeingDragged then s{pos=xConstrainedPos, image=s.images.hover} else s) state.sprites
  -- -- ooopdated = Map.insert Chair ( if s.isBeingDragged then s{pos=xConstrainedPos, image=s.images.hover} else s) oopdated

  isColliding = \sm -> not (withinBoundaries state.geometry sm && (not (isCollidingWithTv state.tvSpecs sm)))
  -- mover = translateSprites
  mover = recenterSprites
  -- ogState = if isDrag && (withinBoundaries state.geometry oopdated) && (not (isCollidingWithTv state.tvSpecs oopdated))
  --                                then state{sprites=oopdated, geometry=newCenter} 
  --                                else state 
  refactoredState = if isDrag then (onMove mover isColliding s cursorPos state){geometry=state.geometry} else state 
  -- _ = if s.isBeingDragged then spy "og center:" [(unsafeLookup Chair ogState.sprites).pos.x, (unsafeLookup Chair ogState.sprites).pos.y] else []
  -- _ = if s.isBeingDragged then spy "rf center:" [(unsafeLookup Chair refactoredState.sprites).pos.x, (unsafeLookup Chair refactoredState.sprites).pos.y] else []



booleanAnd :: (SpriteMap -> Boolean) -> (SpriteMap -> Boolean) -> (SpriteMap -> Boolean) 
booleanAnd f g = (\s -> (f s) && (g s))

infixr 5 booleanAnd as &&&&

onHover2 :: LocalPosition -> Sprite -> Sprite 
onHover2 cursorPos s = if (inBoundsDebug (localToIso cursorPos) s) 
                       then s{image=s.images.hover} 
                       else s{image=s.images.normal} 

-- onDrag2 :: Sprite -> LocalPosition -> Sprite
-- onDrag2 s cursorPos = if s.isBeingDragged

-- get the vector describing how the sprites position has changed 
-- relative to the cursor's drag position
positionDelta :: Sprite -> LocalPosition -> Vector 
positionDelta s cursorPos = {x: 0.0, y: nextPos.y} :-: {x: 0.0, y: s.pos.y} 
  where 
  nextPos = (localToIso cursorPos) :-: s.clickOffset 


onMove 
  :: (SpriteMap -> Vector -> SpriteMap) 
  -> (SpriteMap -> Boolean) 
  -> Sprite 
  -> LocalPosition 
  -> ApplicationState
  -> ApplicationState
onMove moveFn isColliding s cursorPos state = nextState 
  where 
  delta = positionDelta s cursorPos 
  repositionedSprites = moveFn state.sprites delta 
  nextState = if s.isBeingDragged && not (spy "isColliding?" (isColliding repositionedSprites))
              then state{sprites=repositionedSprites}
              else state 






-- this handles dragging + updating the main geometry (and thus the other sprites) 
-- need to figure out how to break these apart. We need the change info in order 
-- to be able to compute the geometry
onDragdd :: LocalPosition -> Sprite -> ApplicationState -> ApplicationState
onDragdd cursorPos s state = refactoredState --state{sprites=if (withinBoundaries state.geometry updatedSprties2) then updatedSprties2 else state.sprites} 
  where 
  isDrag = s.isBeingDragged 
  isColliding = \sm -> not (withinBoundaries state.geometry sm && (not (isCollidingWithTv state.tvSpecs sm)))
  mover = translateSprites
  refactoredState = if isDrag then (onMove mover isColliding s cursorPos state){geometry=state.geometry} else state 






lineIntersection :: Position -> Position -> Position -> Position -> Position 
lineIntersection a1 a2 b1 b2 = {x: xIntersection, y: yIntersection}
  where 
  slopeCA = (a2.y - a1.y) / (a2.x - a1.x)
  interceptCA = a2.y - (slopeCA * a2.x)

  slopeDB = (b2.y - b1.y) / (b2.x - b1.x) 
  interceptEB = b2.y - (slopeDB * b2.x)

  xIntersection = (interceptEB - interceptCA) / (slopeCA - slopeDB)
  yIntersection = (slopeCA * xIntersection) + interceptCA



leftReflections :: WallInteractionPoints -> PrimaryReflections
leftReflections {a, b, c, d, e, f, g, h, i, j} = {firstReflection, secondReflection, thirdReflection}
  where 
  firstIntersection = lineIntersection a f e b  
  secondIntersection = lineIntersection b g f d 
  thirdIntersection = lineIntersection b i h f 

  firstReflection = {source: b, reflection: {x: a.x, y: firstIntersection.y}, dest: f}
  secondReflection = {source: b, reflection: {x: d.x, y: secondIntersection.y}, dest: f}
  thirdReflection = {source: b, reflection: {x: thirdIntersection.x, y: i.y}, dest: f}

rightReflections :: WallInteractionPoints -> PrimaryReflections
rightReflections {a, b, c, d, e, f, g, h, i, j} = {firstReflection, secondReflection, thirdReflection}
  where 
  firstIntersection = lineIntersection c g f d   
  secondIntersection = lineIntersection e c a f 
  thirdIntersection = lineIntersection c i j f 

  firstReflection = {source: c, reflection: {x: d.x, y: firstIntersection.y}, dest: f}
  secondReflection = {source: c, reflection: {x: a.x, y: secondIntersection.y}, dest: f}
  thirdReflection = {source: c, reflection: {x: thirdIntersection.x, y: i.y}, dest: f}


type LineSegment = {p1 :: Vector, p2 :: Vector}

--
--
--   a     b       c      d
--   o     o       o      o
--   |      \     /       |
--   |       \   /        |
--   |        \ /         |
-- e o         o          o g
--   |         f          |
--   |                    |
--   +-----o---o---o------+
--         h   i   j
-- 
collectReflectionPoints :: SpriteMap -> Geometry -> WallInteractionPoints 
collectReflectionPoints sprites {width, depth} = {a, b, c, d, e, f, g, h, i, j}
  where 
  f = (unsafeLookup Chair sprites).pos
  b = (unsafeLookup LeftFront sprites).pos 
  c = (unsafeLookup RightFront sprites).pos 
  a = {x: 0.0, y: b.y}
  d = {x: width, y: b.y}
  e = {x: 0.0, y: f.y}
  g = {x: width, y: f.y}
  h = {x: b.x, y: depth}
  i = {x: f.x, y: depth}
  j = {x: c.x, y: depth}


type WallInteractionPoints = {
  a :: Position, 
  b :: Position, 
  c :: Position, 
  d :: Position, 
  e :: Position,
  f :: Position, 
  g :: Position, 
  h :: Position, 
  i :: Position, 
  j :: Position
}

type PrimaryReflections = {
 firstReflection :: {source :: Position, reflection :: Position, dest :: Position},
 secondReflection :: {source :: Position, reflection :: Position, dest :: Position},
 thirdReflection :: {source :: Position, reflection :: Position, dest :: Position}
}





rotate :: Vector -> Degree -> Vector 
rotate v angle = v :**: a 
  where 
  a = {a1: cos angle,    a2: sin angle,
       b1: -(sin angle), b2: cos angle}


-- | returns a version of the sprite with its anchor 
-- | point locked to its logical position  
anchorAdjusted :: Sprite -> Sprite
anchorAdjusted s = case s.anchor of 
  CenterEast -> anchorCenterEast s
  CenterWest -> anchorCenterWest s 
  CenterNorth -> anchorCenterNorth s 
  CenterSouth -> anchorCenterSouth s 
  Bottom -> anchorBottom s 
  LogicalOrigin -> s{pos=s.pos :-: s.originOffset}
  _ -> s 


anchorXY :: Sprite -> Sprite 
anchorXY s = s{pos=s.pos :+: (0.5 :*: {x: s.size.x, y: s.size.y})} 

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

-- OK. I think this is **finally** correct. 
-- Sprites now store 'pure' positions, completely independent of their
-- origin / anchor position. So, this does two actions before collision detection 
--   1. position the sprite via its anchor
--   2. applies a small offset so that the "origin" of the sprite starts at the 
--      'visual' top of the rotated iso square. By default, 'origin' is still treated 
--      as the top-left corner of the square, when, because of the rotation, we want it 
--      to instead be 'centered'. 
inBoundsDebug :: LocalPosition -> Sprite -> Boolean 
inBoundsDebug isoPos s = inBounds2 isoPos (anchorAdjusted s{pos=s.pos :+: {x: s.size.y / 2.0, y: -(s.size.y / 2.0)}}) 

inBounds5 :: LocalPosition -> Sprite -> Boolean 
inBounds5 cursorPos s = inBounds3 (localToIso cursorPos) (anchorAdjusted s{pos=s.pos :+: {x: s.size.z, y: s.size.z}})

inBounds4 :: LocalPosition -> Sprite -> Boolean 
inBounds4 cursorPos s = inBounds3 (localToIso cursorPos) s

inBounds3 :: Vector -> Sprite -> Boolean 
inBounds3 cursorPos s = inBounds2 cursorPos s{pos=s.pos :-: {x: s.size.z, y: s.size.z}}


--
--              +----+
--              |    |\ upper edge
--              |    | \
--              +----+  \
--              \     \ |
--         -->   \     \|
--  lower edge    \_____\
-- 
--
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





