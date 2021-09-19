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
import React.Basic.Events (SyntheticEvent, EventHandler, handler, handler_)
import React.Basic.Hooks (Component, JSX, component)
import Reflections (collectInteractionPoints, leftReflections)
import Types (Action(..), ApplicationState, DispatchFn, FormField, FormFields, FormID, Mode(..), NumericField, SelectField, SpriteMap(..), TextField, TypedSelectField)
import Utils (getValue)
import Web.Event.EventTarget (dispatchEvent)


controls :: FormProps -> JSX 
controls = homeTheaterForm
    
report :: JSX 
report = do 
  R.text ""


pill :: String -> String -> JSX 
pill label flavor = R.div {
  className: "pill " <> flavor, 
  children: [R.text label]
}

slider :: {min :: String, max :: String, value :: String, onChange :: EventHandler} -> JSX 
slider props = R.div {
  className: "slider",
  children: [
    R.div {className: "flex", children: [
      R.input {
        type: "range", 
        style: css {flex: "1 1 auto"},
        min: props.min, 
        max: props.max, 
        step: "0.001", 
        value: props.value,
        onChange: props.onChange  
      },
      R.div_ [R.text props.value]
    ]}
  ]
}

type ButtonGroupProps = {
  leftButton :: String 
}

dispatchUpdate :: (Action -> Effect Unit) -> FormID -> (String -> String) -> EffectFn1 SyntheticEvent Unit 
dispatchUpdate dispatch formId val = 
  handler getValue $ \e -> 
    dispatch $ UpdateField formId $ val e 


btnGroup :: forall a . (Show a) => (Eq a) => {field :: TypedSelectField a, className :: String, dispatch :: DispatchFn} -> JSX 
btnGroup {field, dispatch, className} = 
  R.div {className: "btn-group", children: field.options # map \option -> 
    R.button {
      type: "button", 
      className: "btn " <> className <> " " <> if option == field.value then "selected" else "", 
      children: [R.text (show option)],
      onClick: dispatchUpdate dispatch field.id (const (show option))
    }
  }


readout :: ApplicationState -> (Action -> Effect Unit) -> JSX 
readout state dispatch = R.div {children: [
    R.div_ [
      R.div {style: css {margin: "10px 0"}, children: [
        R.div {className: "text-center", children: [R.text "Designing"]}, 
        btnGroup {field: state.form.mode, dispatch, className: ""}
      ]},
      onlyWhen isTheaterMode $
        R.div {className: "flex group-padding", children: [
          R.div {className: "flex-auto", children: [
            R.div {className: "text-center", children: [R.text "Channels"]}, 
            btnGroup {field: state.form.channels, dispatch, className: "btn-sm"}
          ]},
          R.div {style: css {flex: "0 0 16px"}},
          R.div {className: "flex-auto", children: [
            R.div {className: "text-center", children: [R.text "Aspect Ratio"]}, 
            btnGroup {field: state.form.aspectRatio, dispatch, className: "btn-sm"}
          ]}
        ]},
      onlyWhen isTheaterMode $
        R.div {className: "flex group-padding", children: [
          R.div {className: "flex-auto", children: [
            R.div {className: "text-center", children: [R.text "Screen Diagonal"]}, 
            R.div {className: "", children: [
              R.input {
                type: "number",  
                minLength: 0,
                pattern: "[0-9]*",
                value: show state.form.screenSize.value, 
                className: "form-control" <> errorClass state.form.screenSize.error,
                onChange: onClickHandler (_.screenSize.id) identity,
                style: (css {width: "calc(100% - 24px)"})
                }
              ]}
            ]},
          R.div {style: css {flex: "0 0 16px"}},
          R.div {className: "flex-auto", children: [
            R.div {className: "text-center", children: [R.text "Field of View"]}, 
            R.input {
                type: "tel",  
                minLength: 0,
                pattern: "[0-9]*",
                readOnly: true,
                value: ((toStringWith (fixed 2) stats.fov) <> "°  " <> "[" <> show stats.presence <> "]"), 
                className: "form-control " <> errorClass state.form.screenSize.error,
                onChange: onClickHandler (_.screenSize.id) identity,
                style: (css {flex: "1 1 auto", width: "calc(100% - 24px)"})
              }
          ]}
        ]},
      R.div_ [
        R.div {className: "text-center", children: [R.text "Room Dimensions (feet)"]}, 
        R.div {className: "flex", children: [
          R.input {
            type: "number",  
            value: show state.form.roomWidth.value, 
            className: "form-control flex-auto " <> errorClass state.form.roomWidth.error,
            onChange: onClickHandler (_.roomWidth.id) identity,
            style: (css {width: 80})
          },
          R.div {className: "text-center", style: css {flex: "0 0 16px"}, children: [R.text "×"]},
          R.input {
            type: "number", 
            value: show state.form.roomDepth.value, 
            className: "form-control flex-auto " <> errorClass state.form.roomDepth.error,
            onChange: onClickHandler (_.roomDepth.id) identity,
            style: (css {width: 80})}
          ]}
      ],
      R.div {
        className: "controls group-padding",
        children: [
          R.div_ [
            R.div {className: "group-label text-center", children: [R.text "Layout Controls"]}
          ],
          R.div {className: "slider-group", children: [
          R.div_ [R.text if isTheaterMode then "Distance from TV" else "Distance from phantom center"],
          slider {
            min: "0.0", 
            max: (toStringWith (fixed 2) stats.maximumListeningDistance), 
            value: (toStringWith (fixed 2) $ stats.distanceFromTv),
            onChange: handler getValue $ \value -> dispatch $ ChangeListenerSlider value 
            }
          ]},
          -- R.div_ [R.text ("FOV: " <> (toStringWith (fixed 2) stats.fov) <> "°  " <> "[" <> show stats.presence <> "]")],
          R.div {className: "slider-group", children: [
            R.div_ [R.text "Speaker distance from rear wall:"],
            slider {
              min: "0.1", 
              max: (toStringWith (fixed 1) stats.maxDisplacement), 
              value: toStringWith (fixed 2) (stats.frontsDistanceFromWall), 
              onChange: handler getValue $ \value -> dispatch $ ChangeTranslateSlider value 
            }
        ]}
      ]},
      R.div_ [
        R.div {className: "group-label text-center", children: [R.text "Reflections"]}
      ],
      R.div {className: "flex", children: [
        R.div {className: "flex-auto", children: [
          R.div {className: "text-center", children: [R.text "1st"]}, 
          R.div {className: "", children: [
            R.input {
              value: toStringWith (fixed 2) (firstReflection.reflection.y) <> "\"", 
              className: "form-control text-center",
              readOnly: true,
              style: (css {width: "calc(100% - 24px)"})
              }
            ]}
          ]},
        R.div {style: css {flex: "0 0 16px"}},
        R.div {className: "flex-auto", children: [
          R.div {className: "text-center", children: [R.text "2nd"]}, 
          R.div {className: "", children: [
            R.input {
              value: toStringWith (fixed 2) (secondReflection.reflection.y) <> "\"", 
              className: "form-control text-center",
              readOnly: true,
              style: (css {width: "calc(100% - 24px)"})
              }
            ]}
          ]},
        R.div {style: css {flex: "0 0 16px"}},
        R.div {className: "flex-auto", children: [
          R.div {className: "text-center", children: [R.text "Rear"]}, 
          R.div {className: "", children: [
            R.input {
              value: toStringWith (fixed 2) (thirdReflection.reflection.x) <> "\"", 
              className: "form-control text-center",
              readOnly: true,
              style: (css {width: "calc(100% - 24px)"})
              }
            ]}
          ]}
      ]}
    ]
  ]}
  where 
  stats = Core.homeTheaterStats state
  (SpriteMap sprites) = state.sprites 
  {firstReflection, secondReflection, thirdReflection} = leftReflections $ collectInteractionPoints state.sprites state.geometry
  onClickHandler :: (FormFields -> FormID) -> (String -> String) -> EffectFn1 SyntheticEvent Unit 
  onClickHandler field val = 
    handler getValue $ \e -> 
      dispatch $ UpdateField (field state.form) $ val e 
  isTheaterMode = state.form.mode.value == HomeTheater




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
      -- R.div_ (fields.mode.options # map \option -> 
      --   R.label_ [
      --     R.input {
      --       type: "radio", 
      --       name: "mode", 
      --       value: option, 
      --       checked: option == (show fields.mode.value),
      --       onClick: onClickHandler (_.mode.id) (const option)
      --     },
      --     R.text option
      --   ]
      -- )
    ],
    onlyWhen isTheaterMode $
      R.div_ [
        -- R.text "Channels: ",
        -- R.div_ (fields.channels.options # map \option -> 
        -- R.label_ [
        --   R.input {
        --     type: "radio", 
        --     name: "channels", 
        --     value: option, 
        --     checked: show option == (spy "val:" (show fields.channels.value)),
        --     onClick: onClickHandler (_.channels.id) (const option)
        --   },
        --   R.text option
        -- ])
      ]
      -- R.div_ [ 
      --   R.label_ [ 
      --     R.text "Channels",
      --     R.select {
      --       onChange: onClickHandler (_.channels.id) identity,
      --       children: (makeOption fields.channels) <$> fields.channels.options
      --     }
      -- ]],
    
    
  ]



onlyWhen :: Boolean -> JSX -> JSX 
onlyWhen pred jsx = if pred then jsx else R.div_ []


makeOption :: SelectField -> String -> JSX 
makeOption props option = do 
  R.option {
    value: option, 
    selected: option == props.value, 
    children: [R.text option]}
