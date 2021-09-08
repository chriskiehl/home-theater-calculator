module Core where 

import Coordinates
import Prelude

import Constants (tileWidth)
import Control.Apply (lift2)
import Data.Array (all, elem)
import Data.Int (floor, toNumber)
import Data.List (find, foldl)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString)
import Data.Number.Format (fixed, toStringWith)
import Data.Tuple (Tuple(..))
import Debug (spy)
import DegreeMath (atan, cos, sin, tan)
import Math (round)
import Math as Math
import ParseInt (parseBase10)
import Types (AnchorPosition(..), ApplicationState, AspectRatio, AudioChannels(..), Degree, FOV, FeetInches, Footprint, FormID(..), Geometry, IsometricPosition, LayoutDescriptor, LayoutStatistics, LocalPosition, Mode(..), Position, PresenceRating(..), Sprite, SpriteID(..), SpriteMap(..), TvSpecs, WorldPosition, values)
import Vector (Matrix2D, Vector, dist, (:**:), (:*:), (:+:), (:-:), rotate)
import Web.Storage.Event.StorageEvent (newValue)


handleMouseDown :: ApplicationState -> LocalPosition -> ApplicationState 
handleMouseDown state cursorPos = state{sprites=updateWhen inBoundsAndEnabled (setDragging isoPos) state.sprites}
  where 
  isoPos = (localToIso cursorPos state.worldOrigin state.zoomMultiplier)
  inBoundsAndEnabled :: Sprite -> Boolean
  inBoundsAndEnabled = spriteInBounds isoPos && _.enabled


handleMouseUp :: ApplicationState -> ApplicationState 
handleMouseUp state = state{sprites=map (\s -> s{isBeingDragged=false, image=s.images.normal}) state.sprites} 


handleMouseMove :: ApplicationState -> LocalPosition -> ApplicationState 
handleMouseMove state cursor = (handleHover cursor nextState){cursor{localPosition=cursor, isoPosition=isoPosition}} 
  where 
  nextState = find isBeingDragged state.sprites 
    # map (dispatchDrag state isoPosition) 
    # fromMaybe state 
  isoPosition = (localToIso cursor state.worldOrigin state.zoomMultiplier)


dispatchDrag :: ApplicationState -> WorldPosition -> Sprite -> ApplicationState
dispatchDrag state v s = case s.id of 
  Chair -> onMove recenterSpritesByDelta isColliding s v state
  _ -> onMove translateSprites isColliding s v state
  where 
  isColliding = (isCollidingWithBoundaries state.geometry) 
                || (isCollidingWithTv state.tvSpecs) 
                || areSpeakersColliding


onMove 
  :: (SpriteMap Sprite -> Vector -> SpriteMap Sprite) 
  -> (SpriteMap Sprite -> Boolean) 
  -> Sprite 
  -> LocalPosition 
  -> ApplicationState
  -> ApplicationState
onMove moveFn isColliding s cursorPos state = nextState 
  where 
  delta = positionDelta s cursorPos
  repositionedSprites = moveFn state.sprites delta 
  nextState = if s.isBeingDragged && not (isColliding repositionedSprites)
              then state{sprites=repositionedSprites}
              else state 


handleHover :: LocalPosition -> ApplicationState -> ApplicationState 
handleHover cursorPos state = state{sprites=map (setHovering cursorPos) state.sprites}
  where 
  setHovering :: LocalPosition -> Sprite -> Sprite 
  setHovering pos s = if (inBounds (localToIso pos state.worldOrigin state.zoomMultiplier) s) || s.isBeingDragged
                       then s{image=s.images.hover} 
                       else s{image=s.images.normal} 


-- I can't figure out how to program generically against Records 
-- in the same way you would against plain Javascript Objects / maps. 
-- I similarly haven't been able for figure out how to dynamically 
-- access a record by key (get Proxy doesn't seem to allow dynmic varables). 
-- Thus: the repeated boilerplate below. 
updateField :: ApplicationState -> FormID -> String -> ApplicationState
updateField state id value = case id of 
  SimulationMode -> fromMaybe state $ updateMode state <$> (parseMode value)
  Channels -> case parseChannel value of 
    Just channel -> updateChannels (state{form{channels{value=value}}}) channel
    Nothing -> state{form{channels{error=Just "Invalid input"}}}
  Width -> case parseRoomDimension value of 
    Just width -> baselineXPosition $ state{form{roomWidth{value=width}}, geometry{width=(toNumber width)}}
    Nothing -> state{form{roomWidth{error=Just "Must be a valid number!"}}}
  Depth -> case parseRoomDimension value of 
    Just depth -> state{form{roomDepth{value=depth}}, geometry{depth=(toNumber depth)}}
    Nothing -> state{form{roomDepth{error=Just "Must be a valid number!"}}}
  ScreenSize -> case parseBase10 value of 
    Just newSize -> state{form{screenSize{value=newSize}}, tvSpecs{diagonalLength=toNumber newSize}}
    Nothing -> state{form{screenSize{error=Just "Must be a valid number"}}}
  AspectRatio -> case parseAspectRatio value of 
    Just newRatio -> state{form{aspectRatio{value=value}}, tvSpecs{aspectRatio=newRatio}}
    Nothing -> state{form{aspectRatio{error=Just "Unknown aspect ratio"}}}
  
   
handleSliderChange :: ApplicationState -> String -> ApplicationState  
handleSliderChange state rawValue = case (fromString rawValue) of  
  Just newValue -> dispatchDrag state {x: chair.pos.x, y: newValue + tv.pos.y} (spy "uhh???" chair{isBeingDragged=true}  )
  Nothing -> (spy "it is failing????" state  )
  where  
  (SpriteMap sm) = state.sprites  
  chair = sm.chair  
  tv = sm.tv
  _ = spy "chair:" [chair.pos.x, chair.pos.y]
  _ = spy "new pos:"  [chair.pos.x, (fromMaybe 0.0 (fromString rawValue)) + tv.pos.y]
  -- localPos = \val -> toIso  state.worldOrigin state.zoomMultiplier  
  


parseMode :: String -> Maybe Mode 
parseMode raw = case raw of 
  "Home Theater" -> Just HomeTheater 
  "Studio" -> Just Studio
  _ -> Nothing 


updateMode :: ApplicationState -> Mode -> ApplicationState 
updateMode state mode = case mode of 
  HomeTheater -> state{form{mode{value=mode}, channels{value="5.0"}}, sprites=map _{enabled=true} state.sprites}
  Studio -> state{form{mode{value=mode}, channels{value="2.0"}}, sprites=disable [TV, LeftRear, RightRear] state.sprites}


updateChannels :: ApplicationState -> AudioChannels -> ApplicationState 
updateChannels state channel = case (spy "channel?" channel) of 
  TwoDot  -> state{sprites=(enable [LeftFront, RightFront] state.sprites) # (disable [LeftRear, RightRear])} 
  FiveDot -> state{sprites=map _{enabled=true} state.sprites}


baselineXPosition :: ApplicationState -> ApplicationState 
baselineXPosition state = state{sprites=nextSprites} 
  where 
  (SpriteMap sprites) = state.sprites 
  delta = (state.geometry.width / 2.0) - sprites.chair.pos.x 
  nextSprites = translateSprites state.sprites {x: delta, y: 0.0}


isBeingDragged :: Sprite -> Boolean 
isBeingDragged = _.isBeingDragged


spriteInBounds :: IsometricPosition -> Sprite -> Boolean 
spriteInBounds pos s = inBounds pos s


isCollidingWithBoundaries :: Geometry -> SpriteMap Sprite -> Boolean 
isCollidingWithBoundaries {depth} sprites = not (allInBounds)
  where 
  isInBounds :: Sprite -> Boolean 
  isInBounds s = if s.enabled then inBounds else true
    where 
    {bottomLeft, topLeft} = footprint s
    inBounds = bottomLeft.y <= depth && bottomLeft.x >= 0.0
      && topLeft.y >= 0.0 && topLeft.x >= 0.0
  allInBounds = all isInBounds (values sprites)


areSpeakersColliding :: SpriteMap Sprite -> Boolean 
areSpeakersColliding (SpriteMap sprites) = colliding 
  where 
  leftFront = footprint sprites.leftFront
  rightFront = footprint sprites.rightFront
  colliding = leftFront.bottomRight.x >= rightFront.bottomLeft.x 

-- | Collision detects against the 'physical' size of the TV in ISO space. 
-- | this accounts for the shortening which takes place during the ISO transform
-- | and allows us to check against arbitrary TV sizes rather than fixed sprite sizes
isCollidingWithTv :: TvSpecs -> SpriteMap Sprite -> Boolean 
isCollidingWithTv tv (SpriteMap sprites) = sprites.tv.enabled && leftFront.bottomRight.x >= tvBottomLeft
  where 
  {diagonalLength, aspectRatio} = tv
  leftFront = footprint sprites.leftFront 
  diagonalDegrees = atan (aspectRatio.height / aspectRatio.width )
  physicalScreenWidth = (cos diagonalDegrees) * diagonalLength
  isoWidth = (cos 30.0) * physicalScreenWidth / tileWidth
  tvBottomLeft = sprites.tv.pos.x - (isoWidth / 2.0)


-- Checks if the supplied point falls within the boundaries of the sprite. 
-- 
-- Sprites store 'pure' (for lack of better term) positions, which are independent of their
-- origin / anchor position. So, to get accurate collision detection, the following needs to happen:
--   1. position the sprite via its anchor
--   2. applies a small offset so that the "origin" of the sprite starts at the 
--      visual top of the rotated iso square. By default, 'origin' is still treated 
--      as the top-left corner of the square, when, because of the rotation, we want it 
--      to instead be 'centered'. 
inBounds :: LocalPosition -> Sprite -> Boolean 
inBounds isoPos s = isWithinSprite isoPos (anchorAdjusted s{pos=s.pos :+: {x: s.size.y / 2.0, y: -(s.size.y / 2.0)}}) 

--
-- Collision detection uses this world view (a de-isometric'd tile in Cartesian space): 
-- 
--              +----+
--              |    |\ upper edge
--              |    | \
--              +----+  \
--              \     \ |
--         -->   \     \|
--  lower edge    \_____\
-- 
-- Performs collision detection to check if the supplied point is inside 
-- the bounds of the given sprite. Traditional AABB detection falls short 
-- after the isometric conversion for any collisions which need to occur against 
-- the full visual space of sprite, as it'll only detect points inside of the top-most 
-- part of the cube. 
--
-- Everything except the 'lower edge' and 'upper edge' works just like normal AABB. It's 
-- only side diagonal edges which need special care. This is accomplished by just checking 
-- that points are above the line formed by the lower edge, and below the upper edge. 
isWithinSprite :: Position -> Sprite -> Boolean 
isWithinSprite pos fullSprite = (pos.x > sprite.x
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



footprint :: Sprite -> Footprint
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


chairTvDistance :: SpriteMap Sprite -> TvSpecs -> Number
chairTvDistance (SpriteMap sprites) specs = (dist center.pos tv.pos) 
  where 
  center = sprites.chair
  tv = sprites.tv


fieldOfView :: SpriteMap Sprite -> TvSpecs -> FOV 
fieldOfView (SpriteMap sprites) specs = angle * 2.0
  where 
  halfScreenWidth = (screenWidth specs) / 2.0 
  center = sprites.chair
  tv = sprites.tv
  distance = (dist center.pos tv.pos) * 16.0
  angle = atan (halfScreenWidth / distance)


presenceRating :: FOV -> PresenceRating
presenceRating fov 
  | fov < 25.0 = ForAnts
  | fov < 30.0 = Low 
  | fov < 40.0 = Medium 
  | fov < 45.0 = High 
  | fov < 50.0 = Ridiculous 
  | otherwise  = BleeingEyes


homeTheaterStats ::  ApplicationState -> LayoutStatistics
homeTheaterStats state = {fov, distanceFromTv, presence, speakerDistance, frontsDistanceFromWall, maximumListeningDistance, maxDisplacement}
  where
  {sprites , tvSpecs, geometry} = state 
  (SpriteMap sm) = sprites 
  frontsDistanceFromWall = (sm.rightFront.pos.y - 1.0) 
  distanceFromTv = chairTvDistance sprites tvSpecs 
  fov = fieldOfView sprites tvSpecs 
  presence = presenceRating fov 
  speakerDistance = distanceFromTv / (cos 30.0)
  maximumListeningDistance = computeMaxListeningPostion state 
  maxDisplacement = computeMaxDisplacement state 



computeMaxListeningPostion :: ApplicationState -> Number 
computeMaxListeningPostion {geometry, form} = if form.channels.value == "5.0" then radius50 * cos 30.0 else radius20 
  where 
  width = (geometry.width / 2.0) - 1.0
  radius50 = (width / (cos 20.0)) 
  radius20 = width / tan 30.0


computeMaxDisplacement :: ApplicationState -> Number 
computeMaxDisplacement {geometry, form, sprites, tvSpecs} = 
  if form.channels.value == "5.0" 
    then geometry.depth - radius50 - distanceFromTv
    else geometry.depth - distanceFromTv - 2.0
  where 
  distanceFromTv = (chairTvDistance sprites tvSpecs) 
  radius = distanceFromTv / (cos 30.0)
  radius50 = (radius / (cos 20.0)) + 1.0




segmentLines :: ApplicationState -> String 
segmentLines state = "" 
  where 
  center = state.geometry.width / 2.0 
  first3rd = state.geometry.depth / 3.0 

-- | computes the horizontal width of the screen from 
-- | the diagonal length and aspect ratio. 
screenWidth :: TvSpecs -> Number 
screenWidth {diagonalLength, aspectRatio} = (cos diagonalDegrees) * diagonalLength 
  where 
  diagonalDegrees = atan (aspectRatio.height / aspectRatio.width) 


layoutFromConfig ::ApplicationState -> ApplicationState
layoutFromConfig state = state{sprites=recenterSprites state.sprites (computeLayout state)}


computeLayout :: ApplicationState -> LayoutDescriptor
computeLayout state = {center, radius}
  where 
  {width} = state.geometry
  screenHorizontal = screenWidth state.tvSpecs 
  targetFov = 20.0 
  distance = ((screenHorizontal / 2.0) / (tan targetFov)) / 16.0
  radius = (distance / (cos 30.0)) 
  wallOffset = 1.75 -- pushes the initial layout 1ft away from the wall
  center = {x: width/2.0, y: distance + wallOffset}


recenterSpritesByDelta :: SpriteMap Sprite -> Vector -> SpriteMap Sprite 
recenterSpritesByDelta (SpriteMap sprites) delta = recenterSprites (SpriteMap sprites) {center: newCenter, radius} 
  where 
  currentCenter = sprites.chair 
  tv = sprites.tv 
  newCenter = currentCenter.pos :+: delta  
  distance = (dist newCenter tv.pos) 
  radius = distance / (cos 30.0) 


recenterSprites :: SpriteMap Sprite -> LayoutDescriptor -> SpriteMap Sprite 
recenterSprites (SpriteMap sprites) layout = SpriteMap {
    chair: sprites.chair{pos=layout.center},
    tv: sprites.tv{pos={x: layout.center.x, y: layout.center.y - tvDistance}},
    leftFront: positionSprite sprites.leftFront layout (-30.0),
    rightFront: positionSprite sprites.rightFront layout (30.0),
    rightRear: positionSprite sprites.rightRear layout (110.0),
    leftRear: positionSprite sprites.leftRear layout (-110.0)
  }
  where 
  tvDistance = (cos 30.0) * layout.radius


positionSprite :: Sprite -> LayoutDescriptor -> Degree -> Sprite 
positionSprite sprite {center, radius} degrees = sprite{pos=targetPos}
  where 
  forwardVector = {x: 0.0, y: -radius} 
  rotatedVector = rotate forwardVector degrees 
  targetPos = rotatedVector :+: center


translateSprites :: SpriteMap Sprite -> Vector -> SpriteMap Sprite 
translateSprites sprites v = map (\s -> s{pos=s.pos :+: v}) sprites


-- | get the vector describing how the sprites position has changed 
-- | relative to the cursor's drag position
positionDelta :: Sprite -> IsometricPosition -> Vector 
positionDelta s cursorPos = {x: 0.0, y: nextPos.y} :-: {x: 0.0, y: s.pos.y} 
  where 
  nextPos = cursorPos :-: s.clickOffset


-- | returns a version of the sprite with its origin 
-- | positioned at the sprite's anchor
anchorAdjusted :: Sprite -> Sprite
anchorAdjusted s = case s.anchor of 
  CenterEast -> anchorCenterEast s
  CenterWest -> anchorCenterWest s 
  CenterNorth -> anchorCenterNorth s 
  CenterSouth -> anchorCenterSouth s 
  Bottom -> anchorBottom s 
  LogicalOrigin -> s{pos=s.pos :-: s.originOffset}
  _ -> s 


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


parseChannel :: String -> Maybe AudioChannels
parseChannel s = case s of 
  "2.0" -> Just TwoDot 
  "5.0" -> Just FiveDot 
  _ -> Nothing 


parseAspectRatio :: String -> Maybe AspectRatio
parseAspectRatio ratio = case ratio of 
  "16:9" -> Just {width: 16.0, height: 9.0}
  "2.4:1" -> Just {width: 2.4, height: 1.0}
  _ -> Nothing


betweenZeroAndTwoHundred :: Int -> Maybe Int 
betweenZeroAndTwoHundred x = if x > 0 && x < 200 then Just x else Nothing 


parseRoomDimension :: String -> Maybe Int 
parseRoomDimension = parseBase10 >=> betweenZeroAndTwoHundred


updateSprites :: (Sprite -> Sprite) -> ApplicationState -> ApplicationState
updateSprites f state = state{sprites=map f state.sprites}

updateWhen :: (Sprite -> Boolean) -> (Sprite -> Sprite) -> SpriteMap Sprite -> SpriteMap Sprite 
updateWhen predicate f = map (\s -> if (predicate s) then (f s) else s)

enable :: Array SpriteID -> SpriteMap Sprite -> SpriteMap Sprite 
enable = setEnabled true 

disable :: Array SpriteID -> SpriteMap Sprite -> SpriteMap Sprite 
disable = setEnabled false

setEnabled :: Boolean -> Array SpriteID -> SpriteMap Sprite -> SpriteMap Sprite 
setEnabled isEnabled ids sm = map (\s -> if elem s.id ids then s{enabled=isEnabled} else s) sm
-- setEnabled isEnabled ids sprites = foldl (\acc sId -> Map.insert sId (unsafeLookup sId acc){enabled=isEnabled} acc) sprites ids

setDragging :: Position -> Sprite -> Sprite  
setDragging pos s = s{isBeingDragged=true, clickOffset=pos :-: s.pos, image=s.images.hover}   

tofeetInches :: Number -> FeetInches
tofeetInches inches = if (Math.round remainingInches) == 12.0  -- nearest8th = 
                      then {feet: feet + 1, inches: 0.0}
                      else {feet: feet, inches: remainingInches}
  where
    feet = floor $ inches / 12.0 
    remainingInches = inches - (toNumber feet) * 12.0
    -- fraction = (toNumber (floor remainingInches)) - remainingInches


showFeetInches :: FeetInches -> String 
showFeetInches {feet, inches} = (show feet) <> "\"" <> (toStringWith (fixed 2) inches) <> "'"


toggleZoomLevel :: ApplicationState -> ApplicationState 
toggleZoomLevel state = if state.zoomMultiplier == 1.0 
  then state{zoomMultiplier=2.0, worldOrigin={x: 448.0, y: 100.0}}
  else state{zoomMultiplier=1.0, worldOrigin={x: 448.0, y: 248.0}}


