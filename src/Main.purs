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
main = unsafePartial do
  -- Just env <- CanvasSupport.getCanvasEnvironment "canvas"
  -- stateRef <- (Refs.new initialState)  
  log "🍝"
  -- render env.ctx stateRef 
  let _ = (map toNonElementParentNode $ document =<< window)
  container <- getElementById "app" =<< (map toNonElementParentNode $ document =<< window)
  pure unit 
  case container of
    Nothing -> throw "Container element not found."
    Just element  -> do 
      pure unit 
      app <- application
      render (app unit) element
