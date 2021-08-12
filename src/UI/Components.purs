module Components where 

import Prelude

import Data.Maybe (Maybe(..), isJust)
import Debug (spy)
import Effect (Effect)
import Effect.Uncurried (EffectFn1)
import React.Basic.DOM (css)
import React.Basic.DOM as R
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (SyntheticEvent, handler, handler_)
import React.Basic.Hooks (Component, JSX, component)
import Types (Action(..), FormField, FormFields, FormID, NumericField, SelectField, TextField)
import Utils (getValue)
import Web.Event.EventTarget (dispatchEvent)


controls :: FormProps -> JSX 
controls = homeTheaterForm
    
report :: JSX 
report = do 
  R.text "Buncha stats and stuff here"


errorClass :: Maybe String -> String 
errorClass (Just _) = "error"
errorClass (_     ) = "" 

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
  R.form_ [
    R.div_ [
      R.div_ (fields.mode.options # map \option -> 
        R.label_ [
          R.input {
            type: "radio", 
            name: "mode", 
            value: option, 
            checked: option == fields.mode.value,
            onClick: onClickHandler (_.mode.id) (const option)
          },
          R.text option
        ]
      )
    ],
    R.div_ [ 
      R.label_ [ 
        R.text "Channels",
        R.select {
          onChange: onClickHandler (_.channels.id) identity,
          children: (makeOption fields.channels) <$> fields.channels.options
        }
    ],
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
      ]}
    ],
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


makeOption :: SelectField -> String -> JSX 
makeOption props option = do 
  R.option {
    value: option, 
    selected: option == props.value, 
    children: [R.text option]}
