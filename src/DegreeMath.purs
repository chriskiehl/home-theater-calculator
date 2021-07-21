module DegreeMath where 

import Prelude

import Math as Math
import Types (Degree)


toRadians :: Degree -> Number 
toRadians x = x * (Math.pi / 180.0)

toDegrees :: Number -> Degree 
toDegrees rad = rad * (180.0 / Math.pi)

sin :: Degree -> Number 
sin = Math.sin <<< toRadians

cos :: Degree -> Number 
cos = Math.cos <<< toRadians 

tan :: Degree -> Number 
tan = Math.tan <<< toRadians

atan :: Degree -> Number 
atan = toDegrees <<< Math.atan