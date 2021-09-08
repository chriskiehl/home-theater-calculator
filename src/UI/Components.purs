module Components where 

import Prelude

import Constants (tileWidth)
import Core as Core
import Data.Maybe (Maybe(..), isJust)
import Data.Number.Format (fixed, toStringWith)
import Debug (spy)
import Effect (Effect)
import Effect.Uncurried (EffectFn1)
import React.Basic.DOM (css)
import React.Basic.DOM as R
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (SyntheticEvent, handler, handler_)
import React.Basic.Hooks (Component, JSX, component)
import Reflections (collectInteractionPoints, leftReflections)
import Types (Action(..), ApplicationState, FormField, FormFields, FormID, Mode(..), NumericField, SelectField, SpriteMap(..), TextField)
import Utils (getValue)
import Web.Event.EventTarget (dispatchEvent)


controls :: FormProps -> JSX 
controls = homeTheaterForm
    
report :: JSX 
report = do 
  R.text "Buncha stats and stuff here"

-- R.text (("Distance: "<> (toStringWith (fixed 2) $ stats.distanceFromTv / 12.0) <> "\""))

readout :: ApplicationState -> (Action -> Effect Unit) -> JSX 
readout state dispatch = R.div {children: [
    R.div_ [R.text ("FOV: " <> (toStringWith (fixed 2) stats.fov) <> "°  " <> "[" <> show stats.presence <> "]")],
    R.div_ [
      R.text "Distance to TV:",
      R.input {
        type: "range", 
        min: "0.0", 
        max: (show stats.maximumListeningDistance), 
        step: "0.001", 
        value: (toStringWith (fixed 2) $ stats.distanceFromTv),
        onChange: handler getValue $ \value -> dispatch $ ChangeListenerSlider value 
      },
      R.text $ (toStringWith (fixed 2) $ stats.distanceFromTv) <> "\""
    ],
    R.div_ [
      R.text "Distance from wall: ",
      -- R.text (show stats.maxDisplacement),
      R.input {
        type: "range", 
        min: "0.1", 
        max: (toStringWith (fixed 1) stats.maxDisplacement), 
        step: "0.001", 
        value: toStringWith (fixed 2) (stats.frontsDistanceFromWall), 
        onChange: handler getValue $ \value -> dispatch $ ChangeTranslateSlider value 
      },
      R.text $ toStringWith (fixed 2) (stats.frontsDistanceFromWall) <> "\""
    ],
    R.div_ [R.text ("Speaker Angle Distance: " <> show (stats.speakerDistance))],
    R.div_ [R.text ("1st Reflection: " <> (toStringWith (fixed 2) ((firstReflection.reflection.y ))))],
    R.div_ [R.text ("2nd Reflection: " <> (toStringWith (fixed 2) ((secondReflection.reflection.y))))],
    R.div_ [R.text ("Rear Reflection: " <> (toStringWith (fixed 2) ((thirdReflection.reflection.x ))))]
  ]}
  where 
  stats = Core.homeTheaterStats state
  (SpriteMap sprites) = state.sprites 
  {firstReflection, secondReflection, thirdReflection} = leftReflections $ collectInteractionPoints state.sprites state.geometry


{-
Canvas.fillText ctx   20.0 200.0
Canvas.fillText ctx   20.0 220.0
Canvas.fillText ctx ("2nd Reflection: " <> (toStringWith (fixed 2) ((secondReflection.reflection.y))))  20.0 245.0
Canvas.fillText ctx ("3rd Reflection: " <> (toStringWith (fixed 2) ((thirdReflection.reflection.x ))))  20.0 280.0

let ggg = ((tileWidth :*: {x: 0.0, y: 0.0}) :**: isoTransform) :+: state.worldOrigin
let con = C.localToIso {x: 0.0, y: 0.0} state.worldOrigin
Canvas.fillText ctx ("local: (" <> (show state.cursor.localPosition.x) <> ", " <> (show state.cursor.localPosition.y) <> ")")  600.0 30.0
Canvas.fillText ctx ("iso: (" <> (show state.cursor.isoPosition.x) <> ", " <> (show state.cursor.isoPosition.y) <> ")")  600.0 60.0
  Canvas.fillText ctx ("iso: (" <> (show ggg.x) <> ", " <> (show ggg.y) <> ")")  600.0 90.0


-}

errorClass :: Maybe String -> String 
errorClass (Just _) = "error"
errorClass (_     ) = "" 


hud :: {dispatch :: (Action -> Effect Unit), state :: ApplicationState} -> JSX 
hud {dispatch, state} = R.div {
  className: "",
  style: css {position: "absolute", width: "100px", height: "100px", backgroundColor: "red"}
}





type FormProps = {
  dispatch :: (Action -> Effect Unit),
  fields :: FormFields
}


homeTheaterForm :: FormProps -> JSX
homeTheaterForm {dispatch, fields} = do 
  let onClickHandler :: (FormFields -> FormID) -> (String -> String) -> EffectFn1 SyntheticEvent Unit 
      onClickHandler field val = 
        handler getValue $ \e -> 
          dispatch $ UpdateField (field fields) $ val e 
  let isTheaterMode = fields.mode.value == HomeTheater

  R.form_ [
    R.div_ [
      R.div_ (fields.mode.options # map \option -> 
        R.label_ [
          R.input {
            type: "radio", 
            name: "mode", 
            value: option, 
            checked: option == (show fields.mode.value),
            onClick: onClickHandler (_.mode.id) (const option)
          },
          R.text option
        ]
      )
    ],
    onlyWhen isTheaterMode $
      R.div_ [ 
        R.label_ [ 
          R.text "Channels",
          R.select {
            onChange: onClickHandler (_.channels.id) identity,
            children: (makeOption fields.channels) <$> fields.channels.options
          }
      ]],
    R.div_ [
      R.div {style: (css {display: "inline"}), children: [
        R.text "Room Dimensions (feet)",
        R.input {
          type: "number",  
          value: show fields.roomWidth.value, 
          className: errorClass fields.roomWidth.error,
          onChange: onClickHandler (_.roomWidth.id) identity,
          style: (css {width: 80})
        },
        R.span_ [R.text "×"],
        R.input {
          type: "number", 
          value: show fields.roomDepth.value, 
          onChange: onClickHandler (_.roomDepth.id) identity,
          style: (css {width: 80})}
      ]}
    ],
    onlyWhen isTheaterMode $ 
      R.div_ [
        R.div {style: (css {display: "inline"}), children: [
          R.text "Screen Size (diagonal): ",
          R.input {
            type: "number",  
            value: show fields.screenSize.value, 
            className: errorClass fields.screenSize.error,
            onChange: onClickHandler (_.screenSize.id) identity,
            style: (css {width: 80})
          }
        ]},
    R.div_ [
      R.label_ [ 
        R.text "Aspect Ratio",
        R.select {
          onChange: onClickHandler (_.aspectRatio.id) identity,
          children: (makeOption fields.aspectRatio) <$> fields.aspectRatio.options
        }
      ]
    ]
  ]
  ]



onlyWhen :: Boolean -> JSX -> JSX 
onlyWhen pred jsx = if pred then jsx else R.div_ []

makeOption :: SelectField -> String -> JSX 
makeOption props option = do 
  R.option {
    value: option, 
    selected: option == props.value, 
    children: [R.text option]}
