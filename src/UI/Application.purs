module Application where

import Prelude

import B64 (decode)
import BlogText (blogText)
import CanvasSupport (Environment)
import CanvasSupport as CanvasSupport
import Components (controls, homeTheaterForm, hud, blogBody)
import Control.Monad.Trans.Class (lift)
import Core as Core
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Nullable (Nullable, notNull, null)
import Data.Number.Format (fixed, toStringWith)
import Data.String (Pattern(..), Replacement(..), replace, replaceAll)
import Data.Tuple (Tuple(..))
import Debug (spy)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Ref as Refs
import Graphics.Canvas (CanvasElement, Context2D)
import Graphics.Canvas as Canvas
import InitialState (initialState)
import MarkdownIt (renderString)
import ParseInt (parseInt)
import Partial.Unsafe (unsafePartial)
import React.Basic.DOM (css)
import React.Basic.DOM as R
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (SyntheticEvent, handler)
import React.Basic.Hooks (Component, JSX, Reducer(..), Reducer, Ref, component, fragment, mkReducer, useEffect, useLayoutEffect, useReducer, useRef, (/\))
import React.Basic.Hooks as React
import Record (get, set)
import Reflections (collectInteractionPoints, leftReflections)
import Renderer as CanvasRenderer
import SimulationCanvas (mainCanvas)
import Sprites as Sprites
import Type.Proxy (Proxy(..))
import Types (Action(..), AnchorPosition(..), AspectRatio, AudioChannels(..), FormField, FormID(..), Geometry, Mode(..), NumericField, SelectField, SpriteID(..), TextField, TvSpecs, ApplicationState)
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
  Init -> state 
  _ -> state 


-- | Entry point to our main React app. 
-- | 
-- | For the most part, it's an html form and a Cavnas element. The bulk 
-- | of the logic lives in the Reducer, and the the bulk of the interesting 
-- | stuff inside of the movement handlers. 
application :: Component Unit 
application = do 
  envRef <- Refs.new Nothing 
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
        Just context -> do 
          CanvasRenderer.render context.ctx state
        Nothing -> pure unit 
      pure (pure unit)

    useEffect state $ unsafePartial do 
      env <- Refs.read envRef 
      -- Ok. Now evident that I don't know how things get executed. *Hours* lost on this line 
      -- because when the <- is not there, it seemed to execute with an n-1 
      -- view of the state, rather than the actual current state, which leads to 
      -- absolutely bonkers behavior (like all hover behaviors being inverted)
      -- finally traced it back to this. Baffling.. 
      _ <- case env of 
        Just context -> do 
          CanvasRenderer.render context.ctx state
        Nothing -> pure unit 
      pure (pure unit)

    pure $ fragment [
      R.div {children: [
        R.div_ [
          R.h1_ [R.text if state.form.mode.value == HomeTheater then "Home Theater Calculator" else "Audio Studio Calculator"],
          mainCanvas {dispatch},
          controls state dispatch
        ],
        blogBody $ replacePlaceholders state $ decode blogText
      ]}
    ]


-- | all the blog content is in markdown, which is written elsewhere 
-- | and then plopped into this project as a converted blob of b64 
-- | encoded HTML. The dynamic bits of text have to be replaced... 
-- | so we do! Literally every render! Not the most efficient thing 
-- | in the world, but it saves my fingies copy/pasting a buncha 
-- | stuff. Performance be damned!  
replacePlaceholders :: ApplicationState -> String -> String 
replacePlaceholders state template = finalOutput
  where 
  stats = Core.homeTheaterStats state 
  reflections = leftReflections $ collectInteractionPoints state.sprites state.geometry
  first = reflections.firstReflection.reflection.y
  second = reflections.secondReflection.reflection.y
  rear = reflections.thirdReflection.reflection.x
  width = state.geometry.width
  toFixedReplacement :: Number -> Replacement 
  toFixedReplacement x = Replacement $ toStringWith (fixed 2) x
  finalOutput = replaceAll (Pattern "##fov##") (toFixedReplacement stats.fov) template
    # replaceAll (Pattern "##distanceFromTv##") (toFixedReplacement stats.distanceFromTv)
    # replaceAll (Pattern "##room-width##") (toFixedReplacement width)
    # replaceAll (Pattern "##half-width##") (toFixedReplacement (width / 2.0))
    # replaceAll (Pattern "##speakerDistance##") (toFixedReplacement stats.speakerDistance)
    # replaceAll (Pattern "##first##") (toFixedReplacement first)
    # replaceAll (Pattern "##second##") (toFixedReplacement second)
    # replaceAll (Pattern "##rear##") (toFixedReplacement rear)


