module Application where

import Prelude

import CanvasSupport (Environment)
import CanvasSupport as CanvasSupport
import Components (controls, homeTheaterForm, hud, readout, report)
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
import InitialState (initialState)
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


reducer :: ApplicationState -> Action -> ApplicationState 
reducer state action = case action of 
  UpdateField id value -> Core.updateField state id value 
  MouseDown pos -> Core.handleMouseDown state pos 
  MouseUp pos -> Core.handleMouseUp state 
  MouseMove pos -> Core.handleMouseMove state pos
  ToggleZoom -> Core.toggleZoomLevel state  
  ChangeListenerSlider rawValue -> Core.handleSliderChange state rawValue 
  ChangeTranslateSlider rawValue -> Core.handleTranslateSlider state rawValue 
  _ -> state 


-- | Entry point to our main React app. 
-- | 
-- | For the most part, it's an html form and a Cavnas element. The bulk 
-- | of the logic lives in the Reducer, and the the bulk of the interesting 
-- | stuff inside of the movement handlers. 
application :: Component Unit 
application = do 
  envRef <- spy "again?" $ Refs.new Nothing 
  reducer' <- mkReducer reducer 

  component "Reducer" \_ -> React.do
    state /\ dispatch <- useReducer (Core.layoutFromConfig initialState) reducer'
    -- lookup the canvas element in the DOM and store it 
    -- in our mutable Ref. 
    -- I don't know how else to make this work... 
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
      -- Ok. Now evident that I don't know how things get executed. *Hours* lost on this line 
      -- because when the <- is not there, it seemed to execute with an n-1 
      -- view of the state, rather than the actual current state, which leads to 
      -- absolutely bonkers behavior (like all hover behaviors being inverted)
      -- finally traced it back to this. Baffling.. 
      _ <- case env of 
        Just context -> CanvasRenderer.render context.ctx state
        Nothing -> pure unit 
      pure (pure unit)

    pure $ fragment [
      R.div {children: [
        R.div_ [
          R.h1_ [R.text "Home Theater Calculator"],
          mainCanvas {dispatch},
          readout state dispatch,
          controls {dispatch, fields: state.form}
        ],
        report 
      ]}
    ]
