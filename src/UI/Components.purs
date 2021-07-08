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
  R.text "Report here!"


type FormProps = {
  dispatch :: (Action -> Effect Unit),
  fields :: FormFields
}

homeTheaterForm :: FormProps -> JSX
homeTheaterForm {dispatch, fields} = do 
  R.form_ [
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
    ]
  ]
]


makeOption :: SelectField -> String -> JSX 
makeOption props option = do 
  R.option {
    value: option, 
    selected: option == props.value, 
    children: [R.text option]}
