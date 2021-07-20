module Types where 

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Vector (Vector, Vector3)
import Web.HTML.HTMLElement (DOMRect)

-- | The Degree in an angle 
type Degree = Number 

-- | An abitrary 2D position in any coordinate system
type Position = Vector 

-- | A position with its cartesian origin set to 
-- | the top-left corner of the canvas
type LocalPosition = Position 
type IsometricPosition = LocalPosition

-- | DOM event client position 
type ClientPosition = {clientX :: Int, clientY :: Int}


type CanvasPosition = {
  client :: Position, 
  canvasRect :: DOMRect
}

type ScreenSize = Number 
type DiagonalLength = Number 
type FOV = Degree

type SpriteMap = Map.Map SpriteID Sprite

data SpriteID
  = TV
  | LeftFront 
  | RightFront 
  | LeftRear 
  | RightRear 
  | Center 
  | Chair  
  | Placeholder 

-- | 
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
  radius :: Number, 
  center :: Vector,
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
  anchor :: AnchorPosition
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
  sprites :: Map.Map SpriteID Sprite,
  geometry :: Geometry,
  form :: FormFields
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

