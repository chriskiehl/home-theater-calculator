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
import Types (Action(..), AnchorPosition(..), ApplicationState, AudioChannels(..), FormField, FormID(..), Geometry, NumericField, SelectField, SpriteID(..), TextField, TvSpecs)
import Unsafe.Coerce (unsafeCoerce)
import Utils (unsafeLookup)

initialGeometry :: Geometry
initialGeometry = {
  width: 15.0, 
  depth: 15.0, 
  radius: 8.0, 
  center: {x: 15.0/2.0, y: 1.0}
}

defaultSpecs :: TvSpecs
defaultSpecs = {
  screenSize: 60.0, 
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
  geometry: Core.forScreenSize initialGeometry defaultSpecs,
  tvSpecs: defaultSpecs,
  form: {
    roomWidth: {id: Width, value: 12, error: Nothing},
    roomDepth: {id: Depth, value: 16, error: Nothing},
    channels: {id: Channels, value: "2.0", error: Nothing, options: ["2.0", "2.1", "5.1"]}
  }
}


reducer :: ApplicationState -> Action -> ApplicationState 
reducer state action = case action of 
  UpdateField id value -> Core.repositionSprites $ Core.updateField state id value 
  MouseDown pos -> Core.handleMouseDown state pos 
  MouseUp pos -> Core.updateSprites (\s -> s{isBeingDragged=false, image=s.images.normal}) state 
  MouseMove pos -> 
    let res = Core.handleMouseMove state pos  
        -- _ = spy "Normal?" $ (unsafeLookup Chair res.sprites).image == (unsafeLookup Chair res.sprites).images.normal
    in  res 
  _ -> state 




application :: Component Unit 
application = do 
  envRef <- spy "again?" $ Refs.new Nothing -- {canvas: Nothing, ctx: Nothing}
  reducer' <- mkReducer reducer 
  component "Reducer" \_ -> React.do
    state /\ dispatch <- useReducer initialState{sprites=Core.translateSprites (Core.layoutSprites initialState.sprites initialState.geometry) {x: 0.0, y: 1.0}} reducer'
    -- state /\ dispatch <- useReducer initialState{sprites=Core.translateSprites (Core.layoutSprites initialState.sprites initialState.geometry) {x:0.0, y:1.0}} reducer'
    -- state /\ dispatch <- useReducer initialState reducer'

    -- lookup the canvas element in the DOM and store it 
    -- in our mutable Ref. 
    useEffect "once" $ unsafePartial do 
      maybeEnv <- CanvasSupport.getCanvasEnvironment "canvas"
      -- for some reason, this only seems to actually write when
      -- I put it on its own line and do <-. Trying pure $ Refs.write (...) results 
      -- in the ref never being modified 
      _ <- Refs.write maybeEnv envRef
      -- ditto for this. It only appears to actually run with the <- magic
      _ <- case maybeEnv of  
        Just context -> CanvasRenderer.render context.ctx state
        Nothing -> pure unit 
      _ <- case maybeEnv of  
        Just context -> CanvasRenderer.render context.ctx state
        Nothing -> pure unit 
      pure (pure unit)

    

    useLayoutEffect state $ unsafePartial do 
      env <- Refs.read envRef 
      -- let _ = spy "useEffectStartNormal?" $ (unsafeLookup Chair state.sprites).image == (unsafeLookup Chair state.sprites).images.normal
      -- Ok. Now evident that I don't know how things get executed. *Hours* lost on this line 
      -- because when the <- is not there, it seemed to execute with an n-1 
      -- view of the state, rather than the actual current state, which leads to 
      -- absolutely bonkers behavior (like all hover behaviors being inverted)
      -- finally traced it back to this like. Baffling.. 
      _ <- case env of 
        Just context -> CanvasRenderer.render context.ctx state
        Nothing -> pure unit 
      pure (pure unit)

    pure $ fragment [
      R.div {children: [
        R.div_ [
          mainCanvas {dispatch},
          controls {dispatch, fields: state.form}
        ],
        report 
      ]}
    ]