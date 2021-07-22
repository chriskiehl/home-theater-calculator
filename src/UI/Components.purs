module Components where 

import Prelude

import Data.Maybe (isJust)
import Debug (spy)
import Effect (Effect)
import React.Basic.DOM (css)
import React.Basic.DOM as R
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (SyntheticEvent, handler, handler_)
import React.Basic.Hooks (Component, JSX, component)
import Types (Action(..), FormField, FormFields, NumericField, SelectField, TextField)
import Utils (getValue)
import Web.Event.EventTarget (dispatchEvent)


controls :: FormProps -> JSX 
controls = homeTheaterForm
    
report :: JSX 
report = do 
  R.text "Buncha stats and stuff here"


type FormProps = {
  dispatch :: (Action -> Effect Unit),
  fields :: FormFields
}

homeTheaterForm :: FormProps -> JSX
homeTheaterForm {dispatch, fields} = do 
  R.form_ [
    R.div_ [
      R.div_ (fields.mode.options # map \option -> 
        R.label_ [
          R.input {
            type: "radio", 
            name: "mode", 
            value: option, 
            checked: option == fields.mode.value,
            onClick: handler getValue (\e -> dispatch (UpdateField fields.mode.id option))
          },
          R.text option
        ]
      )
    ],
    R.div_ [ 
      R.label_ [ 
        R.text "Channels",
        R.select {
          onChange: handler getValue (\e -> dispatch (UpdateField fields.channels.id e)),
          children: (makeOption fields.channels) <$> fields.channels.options
        }
    ],
    R.div_ [
      R.div {style: (css {display: "inline"}), children: [
        R.text "Room Dimensions (feet)",
        R.input {
          type: "number",  
          value: show fields.roomWidth.value, 
          className: if isJust fields.roomWidth.error then "error" else "",
          onChange: handler getValue (\e -> dispatch (UpdateField fields.roomWidth.id e)),
          style: (css {width: 80})
        },
        R.span_ [R.text "×"],
        R.input {
          type: "number", 
          value: show fields.roomDepth.value, 
          onChange: handler getValue (\e -> dispatch (UpdateField fields.roomDepth.id e)),
          style: (css {width: 80})}
      ]}
    ],

    R.div_ [
      R.div {style: (css {display: "inline"}), children: [
        R.text "Screen Size (diagonal): ",
        R.input {
          type: "number",  
          value: show fields.screenSize.value, 
          className: if isJust fields.screenSize.error then "error" else "",
          onChange: handler getValue (\e -> dispatch (UpdateField fields.screenSize.id e)),
          style: (css {width: 80})
        }
      ]}
    ],
    R.div_ [
      R.label_ [ 
        R.text "Aspect Ratio",
        R.select {
          onChange: handler getValue (\e -> dispatch (UpdateField fields.aspectRatio.id e)),
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
