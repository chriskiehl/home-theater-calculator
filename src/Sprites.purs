module Sprites where

import Graphcs

import Prelude ((-), negate)
import Types (AnchorPosition(..), Sprite, SpriteID(..))


thiccWallSprite :: SpriteID -> Sprite
thiccWallSprite spriteId = {
  id: spriteId,
  pos: {x: 0.0, y: 0.0},
  originOffset: {x: 3.0, y: 1.0},
  clickOffset: {x: 0.0, y: 0.0},
  image: thiccWall, 
  images: {normal: block4x3, hover: block4x3hover},
  isBeingDragged: false, 
  isBeingHovered: false, 
  size: {x: 2.0, y: 2.0, z: 2.0},
  anchor: LogicalOrigin,
  enabled: true
}


blockSprite :: SpriteID -> Sprite
blockSprite spriteId = {
  id: spriteId,
  pos: {x: 3.5, y: 6.0},
  originOffset: {x: 1.5, y: 0.5},
  clickOffset: {x: 0.0, y: 0.0},
  image: block, 
  images: {normal: block, hover: blockhover},
  isBeingDragged: false, 
  isBeingHovered: false, 
  size: {x: 1.0, y: 1.0, z: 1.0},
  anchor: CenterNorth,
  enabled: true
}

floorSprite :: SpriteID -> Sprite 
floorSprite spriteId = {
  id: spriteId,
  pos: {x: 3.5, y: 6.0},
  originOffset: {x: 1.5, y: 0.5},
  clickOffset: {x: 0.0, y: 0.0},
  image: floorTile, 
  images: {normal: floorTile, hover: floorTile},
  isBeingDragged: false, 
  isBeingHovered: false, 
  size: {x: 1.0, y: 1.0, z: 1.0},
  anchor: CenterNorth,
  enabled: true
}



twoBlockSprite :: SpriteID -> Sprite
twoBlockSprite spriteId = {
  id: spriteId,
  pos: {x: 0.0, y: 0.0},
  originOffset: {x: 1.5, y: 0.5},
  clickOffset: {x: 0.0, y: 0.0},
  image: twoblock, 
  images: {normal: twoblock, hover: twoblock},
  isBeingDragged: false, 
  isBeingHovered: false, 
  size: {x: 2.0, y: 1.0, z: 1.0},
  anchor: CenterSouth,
  enabled: true
}

threeBlockTwoZSprite :: SpriteID -> Sprite
threeBlockTwoZSprite spriteId = {
  id: spriteId,
  pos: {x: 0.0, y: 0.0},
  originOffset: {x: 1.5, y: 0.5},
  clickOffset: {x: 0.0, y: 0.0},
  image: tv16x9, 
  images: {normal: tv24x1, hover: tv24x1Highlight},
  isBeingDragged: false, 
  isBeingHovered: false, 
  size: {x: 3.0, y: 1.0, z: 2.0},
  anchor: CenterSouth,
  enabled: true
}

tvSprite :: SpriteID -> Sprite 
tvSprite spriteId = {
  id: spriteId,
  pos: {x: 0.0, y: 0.0},
  originOffset: {x: 1.5, y: 0.5},
  clickOffset: {x: 0.0, y: 0.0},
  image: tv16x9, 
  images: {normal: tv16x9, hover: tv16x9Highlight},
  isBeingDragged: false, 
  isBeingHovered: false, 
  size: {x: 3.0, y: 1.0, z: 2.0},
  anchor: CenterSouth,
  enabled: true
}

twoStackSprite :: SpriteID -> Sprite
twoStackSprite spriteId= {
  id: spriteId,
  pos: {x: 0.0, y: 0.0},
  originOffset: {x: 2.5, y: 1.5},
  clickOffset: {x: 0.0, y: 0.0},
  image: twostack, 
  images: {normal: twostack, hover: twostackHover},
  isBeingDragged: false, 
  isBeingHovered: false, 
  size: {x: 1.0, y: 1.0, z: 2.0},
  anchor: CenterSouth,
  enabled: true 
}


bottomWallSprite :: SpriteID -> Sprite
bottomWallSprite spriteId = {
  id: spriteId,
  pos: {x: 0.0, y: 0.0},
  originOffset: {x: 4.5, y: 3.5},
  clickOffset: {x: 0.0, y: 0.0},
  image: bottomWall, 
  images: {normal: twostack, hover: twostackHover},
  isBeingDragged: false, 
  isBeingHovered: false, 
  size: {x: 1.0, y: 1.0, z: 4.0},
  anchor: LogicalOrigin,
  enabled: true 
}

listenerGuy :: SpriteID -> Sprite
listenerGuy spriteId = {
  id: spriteId,
  pos: {x: 0.0, y: 0.0},
  originOffset: {x: 2.5, y: 1.5},
  clickOffset: {x: 0.0, y: 0.0},
  image: listener, 
  images: {normal: listener, hover: listenerHighlight},
  isBeingDragged: false, 
  isBeingHovered: false, 
  size: {x: 1.0, y: 1.0, z: 2.0},
  anchor: CenterNorth,
  enabled: true 
}

couchGuy :: SpriteID -> Sprite
couchGuy spriteId = {
  id: spriteId,
  pos: {x: 0.0, y: 0.0},
  originOffset: {x: 2.5, y: 1.5},
  clickOffset: {x: 0.0, y: 0.0},
  image: couch, 
  images: {normal: couch, hover: couchHighlight},
  isBeingDragged: false, 
  isBeingHovered: false, 
  size: {x: 3.0, y: 1.0, z: 2.0},
  anchor: CenterNorth,
  enabled: true 
}

frontSpeaker :: SpriteID -> Sprite
frontSpeaker spriteId = {
  id: spriteId,
  pos: {x: 0.0, y: 0.0},
  originOffset: {x: 2.5, y: 1.5},
  clickOffset: {x: 0.0, y: 0.0},
  image: speaker, 
  images: {normal: speaker, hover: frontSpeakerHighlight},
  isBeingDragged: false, 
  isBeingHovered: false, 
  size: {x: 1.0, y: 1.0, z: 2.0},
  anchor: CenterSouth,
  enabled: true 
}

leftRearSpeaker :: SpriteID -> Sprite
leftRearSpeaker spriteId = (frontSpeaker spriteId){
  images={normal: leftRear, hover: leftRearHighlight},
  anchor=CenterEast
}

rightRearSpeaker :: SpriteID -> Sprite
rightRearSpeaker spriteId = (frontSpeaker spriteId){
  images={normal: rightRear, hover: rightRearHighlight},
  anchor=CenterWest
}

-- oneTwoOneSprite :: Sprite
-- oneTwoOneSprite = {
--   id: Placeholder,
--   pos: {x: -1.0, y: -1.0},
--   originOffset: {x: 1.5, y: 0.5},
--   clickOffset: {x: 0.0, y: 0.0},
--   image: oneTwoOne, 
--   images: {normal: oneTwoOne, hover: oneTwoOne},
--   isBeingDragged: false, 
--   size: {x: 1.0, y: 2.0, z: 1.0}
-- }

-- tvSprite :: Sprite 
-- tvSprite = {
--   id: Placeholder,
--   pos: {x: 2.5, y: 0.0},
--   originOffset: {x: 3.5, y: 2.5},
--   clickOffset: {x: 0.0, y: 0.0},
--   image: tv, 
--   images: {normal: block4x3, hover: block4x3hover},
--   isBeingDragged: false, 
--   size: {x: 4.2, y: 1.0, z: 3.0}
--   -- width: 50.0, 
--   -- aspectRatio: 1.24
-- }