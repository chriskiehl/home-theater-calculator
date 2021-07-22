module Sprites where

import Graphcs 
import Prelude ((-), negate)
import Types (AnchorPosition(..), Sprite, SpriteID(..))


thiccWallSprite :: Sprite
thiccWallSprite = {
  id: Placeholder,
  pos: {x: 0.0, y: 0.0},
  originOffset: {x: 3.0, y: 1.0},
  clickOffset: {x: 0.0, y: 0.0},
  image: thiccWall, 
  images: {normal: block4x3, hover: block4x3hover},
  isBeingDragged: false, 
  size: {x: 2.0, y: 2.0, z: 2.0},
  anchor: LogicalOrigin,
  enabled: true
}


blockSprite :: Sprite
blockSprite = {
  id: Placeholder,
  pos: {x: 3.5, y: 6.0},
  originOffset: {x: 1.5, y: 0.5},
  clickOffset: {x: 0.0, y: 0.0},
  image: block, 
  images: {normal: block, hover: blockhover},
  isBeingDragged: false, 
  size: {x: 1.0, y: 1.0, z: 1.0},
  anchor: CenterNorth,
  enabled: true
}

twoBlockSprite :: Sprite
twoBlockSprite = {
  id: Placeholder,
  pos: {x: 0.0, y: 0.0},
  originOffset: {x: 1.5, y: 0.5},
  clickOffset: {x: 0.0, y: 0.0},
  image: twoblock, 
  images: {normal: twoblock, hover: twoblock},
  isBeingDragged: false, 
  size: {x: 2.0, y: 1.0, z: 1.0},
  anchor: CenterSouth,
  enabled: true
}

threeBlockTwoZSprite :: Sprite
threeBlockTwoZSprite = {
  id: Placeholder,
  pos: {x: 0.0, y: 0.0},
  originOffset: {x: 1.5, y: 0.5},
  clickOffset: {x: 0.0, y: 0.0},
  image: twoblock, 
  images: {normal: block3x1x2, hover: block3x1x2},
  isBeingDragged: false, 
  size: {x: 3.0, y: 1.0, z: 2.0},
  anchor: CenterSouth,
  enabled: true
}

twoStackSprite :: Sprite
twoStackSprite = {
  id: Placeholder,
  pos: {x: 0.0, y: 0.0},
  originOffset: {x: 2.5, y: 1.5},
  clickOffset: {x: 0.0, y: 0.0},
  image: twostack, 
  images: {normal: twostack, hover: twostackHover},
  isBeingDragged: false, 
  size: {x: 1.0, y: 1.0, z: 2.0},
  anchor: CenterSouth,
  enabled: true 
}

-- speakerSprite :: Sprite
-- speakerSprite = {
--   id: Placeholder,
--   pos: {x: -1.0, y: -1.0},
--   originOffset: {x: 1.5, y: 0.5},
--   clickOffset: {x: 0.0, y: 0.0},
--   image: speaker, 
--   images: {normal: block4x3, hover: block4x3hover},
--   isBeingDragged: false, 
--   size: {x: 1.0, y: 1.0, z: 2.0}
-- }

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