-- | utils and constants for converting back and forth 
-- | between Cartesian and isometric coordinate systems. 
module Coordinates where

import Prelude

import Types (LocalPosition, Position)
import Vector (Matrix2D, Vector, (:**:), (:*:), (:+:), (:-:))

isoTransform :: Matrix2D
isoTransform = {a1:  1.0, a2: 0.5, 
                b1: -1.0, b2: 0.5}

cartTransform :: Matrix2D
cartTransform = {a1: 0.5, a2: -0.5, 
                 b1: 1.0, b2:  1.0}             


toIso :: Vector -> Vector 
toIso v = ((16.0 :*: (v)) :**: isoTransform) :+: {x: 448.0, y: 250.0} 


localToIso :: LocalPosition -> Position
localToIso p = (1.0/16.0) :*: ((p :-: {x: 448.0, y: 250.0}) :**: cartTransform)
