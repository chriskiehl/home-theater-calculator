module Application where

import Prelude

import CanvasSupport (Environment)
import CanvasSupport as CanvasSupport
import Components (controls, homeTheaterForm, report)
import Control.Monad.Trans.Class (lift)
import Core as Core
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Nullable (Nullable, notNull, null)
import Data.Tuple (Tuple(..))
import Debug (spy)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Ref as Refs
import Graphics.Canvas (CanvasElement, Context2D)
import ParseInt (parseInt)
import Partial.Unsafe (unsafePartial)
import React.Basic.DOM (css)
import React.Basic.DOM as R
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (SyntheticEvent, handler)
import React.Basic.Hooks (Component, JSX, Reducer(..), Reducer, Ref, component, fragment, mkReducer, useEffect, useLayoutEffect, useReducer, useRef, (/\))
import React.Basic.Hooks as React
import Record (get, set)
import Renderer as CanvasRenderer
import SimulationCanvas (mainCanvas)
import Sprites as Sprites
import Type.Proxy (Proxy(..))
import Types (Action(..), AnchorPosition(..), ApplicationState, AudioChannels(..), FormField, FormID(..), Geometry, NumericField, SelectField, SpriteID(..), TextField, TvSpecs, AspectRatio)
import Unsafe.Coerce (unsafeCoerce)
import Utils (unsafeLookup)

initialWidth = 13
initialDepth = 15
initialScreenSize = 50

initialGeometry :: Geometry
initialGeometry = {
  width: toNumber initialWidth, 
  depth: toNumber initialDepth, 
  radius: 90000.0, 
  center: {x: 15.0/2.0, y: 1.0}
}

defaultSpecs :: TvSpecs
defaultSpecs = {
  screenSize: toNumber initialScreenSize,
  aspectRatio: {width: 16.0, height: 9.0}
}

-- theaterOptions = {
--   fields: [Width, Depth],
--   homeTheater +
-- }

{-

Studio: 
 - limit 2 channels 
  - no collision on rear or center
  - no tv collision
  - center movement keeps speakers inline with "tv" (even though not visible)
 - only width/depth options in form 

Theater: 
 - all options 
 - 2.0 mode: 
  - collision on fronts
  - collision on tv 
  - center movement keeps speakers inline with tv
    - i.e. radius is computed as a cos 30deg angle 
 - 5.0 mode: 
  - collision on front, center, rear, and TV 
  - center movement treats tv as radius and adjust speakers accordingly. 
-}

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
  geometry: Core.forScreenSize initialGeometry defaultSpecs,
  tvSpecs: defaultSpecs,
  form: {
    mode: {id: SimulationMode, value: "Home Theater", error: Nothing, options: ["Home Theater", "Studio"]},
    roomWidth: {id: Width, value: initialWidth, error: Nothing},
    roomDepth: {id: Depth, value: initialDepth, error: Nothing},
    channels: {id: Channels, value: "2.0", error: Nothing, options: ["2.0", "2.1", "5.1"]},
    screenSize: {id: ScreenSize, value: initialScreenSize, error: Nothing},
    aspectRatio: {id: AspectRatio, value: "16:9", error: Nothing, options: ["16:9", "2.4:1"]}
  }
}


reducer :: ApplicationState -> Action -> ApplicationState 
reducer state action = case action of 
  UpdateField id value -> Core.baselineXPosition $ Core.updateField state id value 
  MouseDown pos -> Core.handleMouseDown state pos 
  MouseUp pos -> Core.handleMouseUp state 
  MouseMove pos -> Core.handleMouseMove state pos  
  _ -> state 

application :: Component Unit 
application = do 
  component "Reducer" \_ -> React.do
    pure $ fragment [
      R.div_ [R.text "uhh..."]
    ]

-- application :: Component Unit 
-- application = do 
--   envRef <- spy "again?" $ Refs.new Nothing -- {canvas: Nothing, ctx: Nothing}
--   reducer' <- mkReducer reducer 
--   component "Reducer" \_ -> React.do
--     state /\ dispatch <- useReducer (Core.fromConfig initialState) reducer'
--     -- state /\ dispatch <- useReducer initialState{sprites=Core.translateSprites (Core.layoutSprites initialState.sprites initialState.geometry) {x: 0.0, y: 1.0}} reducer'
--     -- state /\ dispatch <- useReducer initialState{sprites=Core.translateSprites (Core.layoutSprites initialState.sprites initialState.geometry) {x:0.0, y:1.0}} reducer'
--     -- state /\ dispatch <- useReducer initialState reducer'

--     -- lookup the canvas element in the DOM and store it 
--     -- in our mutable Ref. 
--     useEffect "once" $ unsafePartial do 
--       maybeEnv <- CanvasSupport.getCanvasEnvironment "canvas"
--       -- for some reason, this only seems to actually write when
--       -- I put it on its own line and do <-. Trying pure $ Refs.write (...) results 
--       -- in the ref never being modified 
--       _ <- Refs.write maybeEnv envRef
--       -- ditto for this. It only appears to actually run with the <- magic
--       _ <- case maybeEnv of  
--         Just context -> CanvasRenderer.render context.ctx state
--         Nothing -> pure unit 
--       _ <- case maybeEnv of  
--         Just context -> CanvasRenderer.render context.ctx state
--         Nothing -> pure unit 
--       pure (pure unit)

    

--     useLayoutEffect state $ unsafePartial do 
--       env <- Refs.read envRef 
--       -- Ok. Now evident that I don't know how things get executed. *Hours* lost on this line 
--       -- because when the <- is not there, it seemed to execute with an n-1 
--       -- view of the state, rather than the actual current state, which leads to 
--       -- absolutely bonkers behavior (like all hover behaviors being inverted)
--       -- finally traced it back to this. Baffling.. 
--       _ <- case env of 
--         Just context -> CanvasRenderer.render context.ctx state
--         Nothing -> pure unit 
--       pure (pure unit)

--     pure $ fragment [
--       R.div {children: [
--         R.div_ [
--           mainCanvas {dispatch},
--           controls {dispatch, fields: state.form}
--         ],
--         report 
--       ]}
--     ]
