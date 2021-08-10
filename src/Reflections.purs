-- Tools for finding primary reflection points along the walls 
-- for the source emitters (points b & c) and destination  
-- receiver (point f). 
--
--                    !!README!!
--
-- All functions use the following view of the world in order 
-- to compute reflections: 
--
--   a     b       c      d
--   o     o       o      o
--   |      \     /       |
--   |       \   /        |
--   |        \ /         |
-- e o         o          o g
--   |         f          |
--   |                    |
--   +-----o---o---o------+
--         h   i   j
-- 
-- Reflections points are determined by drawing two additional right triangles 
-- between the source and dest points and finding where their hypotenuses intersect. 
-- For example, to find the first reflection betwen b and f, we make two add two triangles: 
--    1. a, b, e 
--    2. a, f, e 
-- where (b,e) and (f, e) intersect determines where alkong the wall the reflection from b, f 
-- needs to be placed. 
-- 
-- 
-- All reflections are computed the same way, just different permutations of various trigles. 
-- There's probably a much, much smarter math-y way of accomplishing this, but this was this 
-- only way I could figure out how to do it. 
module Reflections where

import Prelude

import Types (Geometry, Position, PrimaryReflections, SpriteID(..), SpriteMap(..), WallInteractionPoints, Sprite)
import Utils (unsafeLookup)


-- | Finds the x,y cooridinates where two lines (as formed by the pairs of inputs) intersect. 
lineIntersection :: Position -> Position -> Position -> Position -> Position 
lineIntersection a1 a2 b1 b2 = {x: xIntersection, y: yIntersection}
  where 
  slopeCA = (a2.y - a1.y) / (a2.x - a1.x)
  interceptCA = a2.y - (slopeCA * a2.x)

  slopeDB = (b2.y - b1.y) / (b2.x - b1.x) 
  interceptEB = b2.y - (slopeDB * b2.x)

  xIntersection = (interceptEB - interceptCA) / (slopeCA - slopeDB)
  yIntersection = (slopeCA * xIntersection) + interceptCA


-- | Constructs the primary refelctions for the front left speaker
leftReflections :: WallInteractionPoints -> PrimaryReflections
leftReflections {a, b, c, d, e, f, g, h, i, j} = {firstReflection, secondReflection, thirdReflection}
  where 
  firstIntersection = lineIntersection a f e b  
  secondIntersection = lineIntersection b g f d 
  thirdIntersection = lineIntersection b i h f 

  firstReflection = {source: b, reflection: {x: a.x, y: firstIntersection.y}, dest: f}
  secondReflection = {source: b, reflection: {x: d.x, y: secondIntersection.y}, dest: f}
  thirdReflection = {source: b, reflection: {x: thirdIntersection.x, y: i.y}, dest: f}


-- | Constructs the primary refelctions for the front right speaker
rightReflections :: WallInteractionPoints -> PrimaryReflections
rightReflections {a, b, c, d, e, f, g, h, i, j} = {firstReflection, secondReflection, thirdReflection}
  where 
  firstIntersection = lineIntersection c g f d   
  secondIntersection = lineIntersection e c a f 
  thirdIntersection = lineIntersection c i j f 

  firstReflection = {source: c, reflection: {x: d.x, y: firstIntersection.y}, dest: f}
  secondReflection = {source: c, reflection: {x: a.x, y: secondIntersection.y}, dest: f}
  thirdReflection = {source: c, reflection: {x: thirdIntersection.x, y: i.y}, dest: f}



-- Collects notable interaction points along the walls in 
-- relation to the current speaker / listener positions. 
-- See: module doc for full explanation. 
--
--   a     b       c      d
--   o     o       o      o
--   |      \     /       |
--   |       \   /        |
--   |        \ /         |
-- e o         o          o g
--   |         f          |
--   |                    |
--   +-----o---o---o------+
--         h   i   j
collectInteractionPoints :: SpriteMap Sprite -> Geometry -> WallInteractionPoints
collectInteractionPoints (SpriteMap sprites) {width, depth} = {a, b, c, d, e, f, g, h, i, j}
  where 
  f = sprites.chair.pos
  b = sprites.leftFront.pos 
  c = sprites.rightFront.pos 
  a = {x: 0.0, y: b.y}
  d = {x: width, y: b.y}
  e = {x: 0.0, y: f.y}
  g = {x: width, y: f.y}
  h = {x: b.x, y: depth}
  i = {x: f.x, y: depth}
  j = {x: c.x, y: depth}






