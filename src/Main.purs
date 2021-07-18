module Main where

import Prelude
import Types

import Application (application)
import CanvasSupport as CanvasSupport
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Effect.Ref as Refs
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
  -- log "🍝"
  -- render env.ctx stateRef 
  container <- getElementById "app" =<< (map toNonElementParentNode $ document =<< window)
  case container of
    Nothing -> throw "Container element not found."
    Just element  -> do 
      pure unit 
      app <- application
      render (app unit) element
