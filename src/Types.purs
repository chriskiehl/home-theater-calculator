module Types where 

import Prelude

import Data.Array as Array
import Data.Foldable (class Foldable)
import Data.Functor.App (App)
import Data.List as List
import Data.List (List(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Record (get)
import Record.Extra (mapRecord)
import Type.Proxy (Proxy(..))
import Vector (Vector, Vector3)
import Web.HTML.HTMLElement (DOMRect)


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
type FOV = Number


type Base64Data = String 


-- | IDs of the items in SpriteMap 
data SpriteID
  = TV
  | LeftFront 
  | RightFront 
  | LeftRear 
  | RightRear 
  | Center 
  | Chair  
  | AdHoc 

-- | Sprites maintain a 'pure' position in space sans any offsets
-- | or shifting to account for their actual size or shape. it's only during 
-- | collision detection or rendering that we adjust their position based on 
-- | where we want their sprite to be achored. 
-- | This makes doing math between the sprites much, much easier. 
-- | 
-- | All of these anchor positions should be visualized against the standard 
-- | Isometric cube. 
data AnchorPosition 
  = CenterNorth
  | CenterSouth 
  | CenterEast
  | CenterWest 
  | Bottom 
  | Top 
  | TopLeft 
  -- the actual top-left origin of the Cartesian rectangle containing the sprite 
  -- generally not used for anything other than testing, as it's completely 
  -- disconnected from the visuals contained within the sprite. 
  | TrueOrigin
  -- In (x,y,z) logical origin would be the location on the sprite which 'logically' 
  -- sits at the Sprite's "visual" Isometric coordinate (0,0,1). Meaning the top-left of 
  -- the bottom, back-most cube. This is used to sit large, irregular non-32x32 sprites 
  -- such that rendering them at (0,0) makes sense. And for *that* description to make 
  -- sense, load one of the large sprites and render it at TrueOrigin (0,0). The need for
  -- establishing a logical origin will become clear. 
  | LogicalOrigin 


derive instance eqSpriteId :: Eq SpriteID  
derive instance ordSpriteId :: Ord SpriteID  
derive instance eqFormID :: Eq FormID
derive instance ordFormID :: Ord FormID
derive instance eqAnchorPos :: Eq AnchorPosition
derive instance ordAnchorPos :: Ord AnchorPosition
instance showSpriteId :: Show SpriteID where 
  show TV = "TV"
  show LeftFront = "Left Front"
  show RightFront = "Right Front"
  show LeftRear = "Left Rear"
  show RightRear = "Right Rear"
  show Center = "Center"
  show Chair = "Listening Position" 
  show AdHoc = "Ad Hoc"


type FormField a r = (
  id :: FormID, 
  value :: a,
  error :: Maybe String
  | r
)

type TextField = Record (FormField String ()) 
type NumericField = Record (FormField Int ()) 
type SelectField = Record (FormField String (options :: Array String))
type TypedSelectField a = Record (FormField a (options :: Array String))

type Geometry = {
  width :: Number,
  depth :: Number 
}

-- | Record of all the primary notable sprites used throughout the application. 
newtype SpriteMap a = SpriteMap {
  chair :: a,
  leftFront :: a,
  rightFront :: a, 
  tv :: a, 
  leftRear :: a, 
  rightRear :: a 
}


instance functorSpriteMap :: Functor SpriteMap where 
  map f (SpriteMap sm) = SpriteMap $ mapRecord f sm 


instance pureSpriteMap :: Applicative SpriteMap where 
  pure f = SpriteMap {
    chair: f, 
    leftFront: f,
    rightFront: f, 
    tv: f, 
    leftRear: f, 
    rightRear: f
  }


instance applicativeSpriteMap :: Apply SpriteMap where 
  apply (SpriteMap fm) (SpriteMap sm) = SpriteMap {
    chair: fm.chair sm.chair,
    leftFront: fm.leftFront sm.leftFront,
    rightFront: fm.rightFront sm.rightFront, 
    tv: fm.tv sm.tv, 
    leftRear: fm.leftRear sm.leftRear, 
    rightRear: fm.rightRear sm.rightRear
  }


instance foldableSpriteMap :: Foldable SpriteMap where 
  foldr f init xs = Array.foldr f init (values xs) 
  foldl f init xs = Array.foldl f init (values xs) 
  foldMap f xs = Array.foldMap f (values xs)


instance showSpriteMap :: (Show a) => Show (SpriteMap a) where 
  show (SpriteMap sm) = (show sm)

derive instance eqSpriteMap :: (Eq a) => Eq (SpriteMap a)
derive instance ordSpriteMap :: (Ord a) => Ord (SpriteMap a)


values :: forall a. SpriteMap a -> Array a  
values (SpriteMap sm) = [sm.chair, sm.tv, sm.leftFront,sm.rightFront, sm.leftRear, sm.rightRear]

valuesL :: forall a. SpriteMap a -> List a 
valuesL sm = List.fromFoldable $ values sm

-- -- | Convenience type alias for our main map 
-- -- | of sprites. 
-- -- | Usage Note: it's a Map only because that's the only 
-- -- | way I can figure out how to program updates against the 
-- -- | collection of items in the map. Which is to say, in vanilla 
-- -- | js, I'd use an object. The items within the map are fixed 
-- -- | throughout the runtime of the app. 
-- type SpriteMap = Map.Map SpriteID Sprite



type Sprite = {
  id :: SpriteID,
  pos :: Vector, 
  originOffset :: Vector, 
  clickOffset :: Vector,
  image :: DataUrl, 
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
  normal :: DataUrl, 
  hover :: DataUrl 
}

type FormFields = {
  mode :: TypedSelectField Mode,
  roomWidth :: NumericField, 
  roomDepth :: NumericField, 
  channels :: SelectField, 
  screenSize :: NumericField, 
  aspectRatio :: SelectField 
}

type TvSpecs = {
  diagonalLength :: DiagonalLength, 
  aspectRatio :: AspectRatio 
}

type ApplicationState = {
  tvSpecs :: TvSpecs,
  sprites :: SpriteMap Sprite,
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


-- | Models a room and objects within it as a collection 
-- | of notable points. This is used for computing the points 
-- | along the walls where waves emitted by a speaker would be 
-- | reflected back to the listener. 
-- | See Reflections.purs for full diagram of usage. 
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

type Reflection = {source :: Position, reflection :: Position, dest :: Position}


type PrimaryReflections = {
 firstReflection :: Reflection,
 secondReflection :: Reflection,
 thirdReflection :: Reflection
}


type Footprint = {
  topLeft :: Position, 
  topRight :: Position, 
  bottomLeft :: Position, 
  bottomRight :: Position
}


data FormID = Channels | Width | Depth | ScreenSize | AspectRatio | SimulationMode


data AudioChannels = TwoDot | FiveDot 


instance showAudioChannels :: Show AudioChannels where 
  show x = case x of 
    TwoDot -> "2.0"
    FiveDot -> "5.0"


instance parseAudioChannels :: ParseRaw AudioChannels where 
  parseString s = case s of 
    "2.0" -> Just TwoDot 
    "5.0" -> Just FiveDot 
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
  



data Mode = HomeTheater | Studio

derive instance eqMode :: Eq Mode 

instance showMode :: Show Mode where 
  show HomeTheater = "Home Theater" 
  show Studio = "Studio"  

-- | A DataURL containing a b64 encoded .png
type DataUrl = String 

-- | type for reflecting measurments in terms 
-- | of feet and inches. e.g. 5"2' 
type FeetInches = {
  feet :: Int, 
  inches :: Number 
}


data PresenceRating 
  = ForAnts 
  | Low 
  | Medium 
  | High 
  | Ridiculous
  | BleeingEyes


instance showPresenceRating :: Show PresenceRating where 
  show ForAnts = "For ants?!"
  show Low = "Low" 
  show Medium = "Medium" 
  show High = "Recommended"
  show Ridiculous = "Ridiculous"
  show BleeingEyes = "Probably painful"  


type LayoutStatistics = {
  fov :: FOV, 
  distanceFromTv :: Number, 
  presence :: PresenceRating, 
  speakerDistance :: Number,
  -- distance from the front speakers to the 
  -- forward wall. 
  frontsDistanceFromWall :: Number 
}    