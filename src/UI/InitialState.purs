module InitialState where

import Constants (tileWidth)
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Prelude ((/))
import Sprites as Sprites
import Types (AnchorPosition(..), ApplicationState, AudioChannels(..), FormID(..), Geometry, Mode(..), Ratio(..), SpriteID(..), SpriteMap(..), TvSpecs)

initialWidth = 15
initialDepth = 18
initialScreenSize = 50

initialGeometry :: Geometry
initialGeometry = {
  width: toNumber initialWidth, 
  depth: toNumber initialDepth
}

defaultSpecs :: TvSpecs
defaultSpecs = {
  diagonalLength: toNumber initialScreenSize,
  aspectRatio: {width: 16.0, height: 9.0}
}


initialState :: ApplicationState
initialState = {
  cursor: {localPosition: {x: 0.0, y: 0.0}, isoPosition: {x: 0.0, y: 0.0}},
  worldOrigin: {x: 448.0, y: 100.0},
  zoomMultiplier: 2.0,
  sprites: SpriteMap {
    chair: (Sprites.blockSprite Chair){pos={x: 0.0, y: 0.0}},
    leftFront: Sprites.twoStackSprite LeftFront,
    rightFront: Sprites.twoStackSprite RightFront,
    tv: (Sprites.threeBlockTwoZSprite TV){pos={x: 3.0/2.0, y: 0.0}},
    leftRear: (Sprites.twoStackSprite LeftRear){anchor=CenterEast},
    rightRear: (Sprites.twoStackSprite RightRear){anchor=CenterWest}
  },
  geometry: initialGeometry,
  tvSpecs: defaultSpecs,
  form: {
    mode: {id: SimulationMode, value: HomeTheater, error: Nothing, options: [HomeTheater, Studio]},
    roomWidth: {id: Width, value: initialWidth, error: Nothing},
    roomDepth: {id: Depth, value: initialDepth, error: Nothing},
    channels: {id: Channels, value: FiveDot, error: Nothing, options: [TwoDot, FiveDot]},
    screenSize: {id: ScreenSize, value: initialScreenSize, error: Nothing},
    aspectRatio: {id: AspectRatio, value: SixteenByNine, error: Nothing, options: [SixteenByNine, TwoPointFourByOne]}
  }
}