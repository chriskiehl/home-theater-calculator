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


initialState :: GameState
initialState = {
  invalidWidth: false,
  invalidDepth: false,
  selectedMode: HomeTheater, 
  selectedChannels: Channels2_0,
  sprites: Map.fromFoldable [
    -- Tuple LeftFront  twoBlockSprite{id=LeftFront, pos={x: 0.0, y: 0.0}},
    -- Tuple RightFront  twoStackSprite{id=RightFront, pos={x: 0.0, y: 0.0}},
    -- Tuple TV tvSprite{id=TV, pos={x: 3.0/2.0, y: 0.0}},
    -- Tuple Chair blockSprite{id=Chair, pos={x:5.0/2.0 - blockSprite.size.x / 2.0, y: 6.9}},
    -- Tuple LeftRear twoStackSprite{id=LeftRear, pos={x: 0.0, y: 0.0}},
    -- Tuple RightRear twoStackSprite{id=RightRear, pos={x: 0.0, y: 0.0}}
  ],
  geometry: {
    width: 12.0, 
    depth: 15.0, 
    center: {x:5.0/2.0, y: 6.9},
    radius: 6.0
  }
}

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
