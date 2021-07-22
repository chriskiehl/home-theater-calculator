-- | Vector type and basic operations
module Vector where 

import Prelude hiding (div, add)

import DegreeMath (Degree, cos, sin)
import Math as Math


type Scalar = Number

type Vector = {x :: Number, y :: Number} 

type Vector3 = {x :: Number, y :: Number, z :: Number} 

type Matrix2D = {
    a1 :: Number, a2 :: Number, 
    b1 :: Number, b2 :: Number
}


add :: Vector -> Vector -> Vector 
add a b = {x: a.x + b.x, y: a.y + b.y}

minus :: Vector -> Vector -> Vector 
minus a b = {x: a.x - b.x, y: a.y - b.y}

div :: Scalar -> Vector -> Vector
div a v = {x: v.x / a, y: v.y / a}

scale :: Scalar -> Vector -> Vector
scale a v = {x: v.x * a, y: v.y * a}

rotate :: Vector -> Degree -> Vector 
rotate v angle = v :**: a 
  where 
  a = {a1: cos angle,    a2: sin angle,
       b1: -(sin angle), b2: cos angle}

mag :: Vector -> Scalar 
mag v = Math.sqrt ((v.x * v.x) + (v.y * v.y))

norm :: Vector -> Vector 
norm v = if not (m == 0.0) then div m v else v
    where m = mag v


-- | Return the vector with the largest magnitude
max :: Vector -> Vector -> Vector 
max v u = if (mag v) > (mag u) then v else u


-- | Return the vector with the largest magnitude
min :: Vector -> Vector -> Vector 
min v u = if (mag v) < (mag u) then v else u


dist :: Vector -> Vector -> Number 
dist v1 v2 = Math.sqrt $ (Math.pow (v2.x - v1.x) 2.0) + (Math.pow (v2.y - v1.y) 2.0)



vectorProduct :: Vector -> Matrix2D -> Vector 
vectorProduct v a = {x: v.x * a.a1 + v.y * a.b1, y: v.x * a.a2 + v.y * a.b2}




multiply :: Matrix2D -> Matrix2D -> Matrix2D
multiply a b = {a1: a.a1 * b.a1 + a.a2 * b.b1, 
                a2: a.a1 * b.a2 + a.a2 * b.b2,
                b1: a.b1 * b.a1 + a.b2 * b.b1,
                b2: a.a1 * b.a2 + a.a2 * b.b2}




infixr 5 add as :+:
infix 5 minus as :-:
infixr 5 scale as :*:
infixr 5 vectorProduct as :**:                