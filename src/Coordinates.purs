-- | utils and constants for converting back and forth 
-- | between Cartesian and isometric coordinate systems. 
module Coordinates where

import Prelude

import Constants (tileWidth)
import Types (LocalPosition, Position, WorldPosition, IsometricPosition)
import Vector (Matrix2D, Vector, (:**:), (:*:), (:+:), (:-:))

isoTransform :: Matrix2D
isoTransform = {a1:  1.0, a2: 0.5, 
                b1: -1.0, b2: 0.5}

cartTransform :: Matrix2D
cartTransform = {a1: 0.5, a2: -0.5, 
                 b1: 1.0, b2:  1.0}             


toIso :: WorldPosition -> Position -> Number -> IsometricPosition 
toIso v worldOrigin zoom = (((tileWidth * zoom) :*: (v)) :**: isoTransform) :+: worldOrigin 


localToIso :: LocalPosition -> LocalPosition -> Number -> WorldPosition
localToIso p worldOrigin zoom = (1.0/(tileWidth * zoom) ) :*: ((p :-: worldOrigin) :**: cartTransform)

worldIsoToLocal :: Position -> Position -> Number -> Position 
worldIsoToLocal p worldOrigin zoom = (tileWidth * zoom) :*: ((p :-: worldOrigin) :**: cartTransform)