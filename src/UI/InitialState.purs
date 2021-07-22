module InitialState where

import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Prelude ((/))
import Sprites as Sprites
import Types (AnchorPosition(..), ApplicationState, FormID(..), Geometry, SpriteID(..), TvSpecs)

initialWidth = 13
initialDepth = 15
initialScreenSize = 50

initialGeometry :: Geometry
initialGeometry = {
  width: toNumber initialWidth, 
  depth: toNumber initialDepth
}

defaultSpecs :: TvSpecs
defaultSpecs = {
  screenSize: toNumber initialScreenSize,
  aspectRatio: {width: 16.0, height: 9.0}
}


initialState :: ApplicationState
initialState = {
  sprites: Map.fromFoldable [
    Tuple Chair       Sprites.blockSprite{id=Chair, pos={x: 0.0, y: 0.0}},
    Tuple LeftFront   Sprites.twoStackSprite{id=LeftFront},
    Tuple RightFront  Sprites.twoStackSprite{id=RightFront},
    Tuple TV          Sprites.threeBlockTwoZSprite{id=TV, pos={x: 3.0/2.0, y: 0.0}},
    Tuple LeftRear    Sprites.twoStackSprite{id=LeftRear, anchor=CenterEast},
    Tuple RightRear   Sprites.twoStackSprite{id=RightRear, anchor=CenterWest}
  ],
  geometry: initialGeometry,
  tvSpecs: defaultSpecs,
  form: {
    mode: {id: SimulationMode, value: "Home Theater", error: Nothing, options: ["Home Theater", "Studio"]},
    roomWidth: {id: Width, value: initialWidth, error: Nothing},
    roomDepth: {id: Depth, value: initialDepth, error: Nothing},
    channels: {id: Channels, value: "2.0", error: Nothing, options: ["2.0", "5.0"]},
    screenSize: {id: ScreenSize, value: initialScreenSize, error: Nothing},
    aspectRatio: {id: AspectRatio, value: "16:9", error: Nothing, options: ["16:9", "2.4:1"]}
  }
}