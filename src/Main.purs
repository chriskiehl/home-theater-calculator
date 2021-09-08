module Main where

import Prelude
import Types

import Application (application)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Partial.Unsafe (unsafePartial)
import React.Basic.DOM (render)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)



main :: Effect Unit
main = do
  log "🍝"
  let _ = (map toNonElementParentNode $ document =<< window)
  container <- getElementById "article" =<< (map toNonElementParentNode $ document =<< window)
  case container of
    Nothing -> throw "Container element not found."
    Just element  -> do 
      app <- application
      render (app unit) element
