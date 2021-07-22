module Types where 

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Vector (Vector, Vector3)
import Web.HTML.HTMLElement (DOMRect)

-- | An angle as measured in degrees (rather than radians) 
type Degree = Number 

-- | An abitrary 2D position in any coordinate system
type Position = Vector 

-- | A position with its cartesian origin set to 
-- | the top-left corner of the canvas
type LocalPosition = Position 

-- | A position where its origin is set to the top-center 
-- | i.e. (0, 0) of the isometric coordinate system. 
-- | Here, "local" is defined as 'roughly the middle of 
-- | the canvas where things are redered' for most purposes
type IsometricPosition = LocalPosition

-- | A raw DOM event client position. The is the current position 
-- | relative to the user's browser window.  
type ClientPosition = {clientX :: Int, clientY :: Int}


type ScreenSize = Number 
type DiagonalLength = Number 

-- | Angle of the current "Field of View" based on 
-- | the distance from the TV. 
type FOV = Degree

-- | Convenience type alias for our main map 
-- | of sprites. 
-- | Usage Note: it's a Map only because that's the only 
-- | way I can figure out how to program updates against the 
-- | collection of items in the map. Which is to say, in vanilla 
-- | js, I'd use an object. The items within the map are fixed 
-- | throughout the runtime of the app. 
type SpriteMap = Map.Map SpriteID Sprite

-- | IDs of the items in SpriteMap 
data SpriteID
  = TV
  | LeftFront 
  | RightFront 
  | LeftRear 
  | RightRear 
  | Center 
  | Chair  
  | Placeholder 

-- | Sprites maintain a 'pure' position in space sans any offsets
-- | or shifting to account for their actual size or shape. it's only during 
-- | collision detection or rendering that we adjust their position based on 
-- | where we want their sprite to be achored. 
-- | This makes doing math between the sprites much, much easier. 
data AnchorPosition 
  = CenterNorth
  | CenterSouth 
  | CenterEast
  | CenterWest 
  | Bottom 
  | Top 
  | TopLeft 
  | TrueOrigin
  | LogicalOrigin 

derive instance eqSpriteId :: Eq SpriteID  
derive instance ordSpriteId :: Ord SpriteID  
derive instance eqFormID :: Eq FormID
derive instance ordFormID :: Ord FormID
derive instance eqAnchorPos :: Eq AnchorPosition
derive instance ordAnchorPos :: Ord AnchorPosition



type FormField a r = (
  id :: FormID, 
  value :: a,
  error :: Maybe String
  | r
)

type TextField = Record (FormField String ()) 
type NumericField = Record (FormField Int ()) 
type SelectField = Record (FormField String (options :: Array String))

type Geometry = {
  width :: Number,
  depth :: Number 
}

type Sprite = {
  id :: SpriteID,
  pos :: Vector, 
  originOffset :: Vector, 
  clickOffset :: Vector,
  image :: String, 
  images :: Images, 
  isBeingDragged :: Boolean,
  size :: Vector3,
  anchor :: AnchorPosition,
  enabled :: Boolean
}

type AspectRatio = {
  width :: Number, 
  height :: Number
}

type Images = {
  normal :: String, 
  hover :: String 
}

type FormFields = {
  mode :: SelectField,
  roomWidth :: NumericField, 
  roomDepth :: NumericField, 
  channels :: SelectField, 
  screenSize :: NumericField, 
  aspectRatio :: SelectField 
}

type TvSpecs = {
  screenSize :: DiagonalLength, 
  aspectRatio :: AspectRatio 
}

type ApplicationState = {
  tvSpecs :: TvSpecs,
  sprites :: SpriteMap,
  geometry :: Geometry,
  form :: FormFields
}

-- | Every layout / configuration boils down to placing some sprites 
-- | at specifc locations around a circle. As such, knowing the radius 
-- | of that circle + where it's centered in the world is all that's 
-- | needed to power the various layout strategies. 
type LayoutDescriptor = {
  center :: Position, 
  radius :: Number
}

type LineSegment = {p1 :: Vector, p2 :: Vector}

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



data FormID = Channels | Width | Depth | ScreenSize | AspectRatio | SimulationMode

data AudioChannels 
    = Channels2_0
    | Channels2_1
    | Channels5_1


instance showAudioChannels :: Show AudioChannels where 
  show x = case x of 
    Channels2_0 -> "2.0"
    Channels2_1 -> "2.1"
    Channels5_1 -> "5.1"


instance parseAudioChannels :: ParseRaw AudioChannels where 
  parseString s = case s of 
    "2.0" -> Just Channels2_0 
    "2.1" -> Just Channels2_1
    "5.1" -> Just Channels5_1
    _ -> Nothing 


class ParseRaw a where 
  parseString :: String -> Maybe a 

data Action 
  = UpdateField FormID String  
  | MouseMove LocalPosition 
  | MouseUp LocalPosition
  | MouseDown LocalPosition
  | StartDrag Sprite LocalPosition
  | StopDrag 
  



data Mode = HomeTheater | AudioStudio

