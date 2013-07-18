{- |
Module      :  Point2D
Description :  Definition and functions of a 2D vector with x and y properties.
Copyright   :  (c) Jeff Smits
License     :  GPL-3.0

Maintainer  :  jeff.smits@gmail.com
Stability   :  experimental
Portability :  portable

 | ---------------------------------------------------------------------- |
 | This program is free software: you can redistribute it and/or modify   |
 | it under the terms of the GNU General Public License as published by   |
 | the Free Software Foundation, either version 3 of the License, or      |
 | (at your option) any later version.                                    |
 |                                                                        |
 | This program is distributed in the hope that it will be useful,        |
 | but WITHOUT ANY WARRANTY; without even the implied warranty of         |
 | MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          |
 | GNU General Public License for more details.                           |
 |                                                                        |
 | You should have received a copy of the GNU General Public License      |
 | along with this program.  If not, see <http://www.gnu.org/licenses/>.  |
 | ---------------------------------------------------------------------- |
-}

module Point2D where

type Point2D = {x : Float, y : Float}

-- old constructor
point2D : Float -> Float -> Point2D
point2D x y = { x = x, y = y }

cartesian : (Float, Float) -> Point2D
cartesian (x, y) = { x = x, y = y }

polar : (Float, Float) -> Point2D
polar (r, phi) = { x = r * cos phi, y = r * sin phi }

toCartesian : Point2D -> (Float, Float)
toCarthesian {x,y} = (x,y)

toPolar : Point2D -> (Float, Float)
toPolar p = let (r,u) = breakDown p
            in  (r,atan2 u.x u.y)

zero : Point2D
zero = from1D 0

-------------------------
-- list-like functions --
-------------------------

-- map a function over both points
map : (Float -> Float) -> Point2D -> Point2D
map f {x,y} = { x = f x, y = f y }

-- fold the two in a function
fold : (Float -> Float -> Float) -> Point2D -> Float
fold f {x,y} = f x y

-- zip two together like they're lists
zipWith : (Float -> Float -> Float) -> Point2D -> Point2D -> Point2D
zipWith f a b = { x = f a.x b.x, y = f a.y b.y }

----------------------------
-- element-wise operators --
----------------------------

-- element-wise addition
e_pls : Point2D -> Point2D -> Point2D
e_pls = zipWith (\a b -> a + b)

-- element-wise substraction
e_min : Point2D -> Point2D -> Point2D
e_min = zipWith (\a b -> a - b)

-- element-wise multiplication
e_mul : Point2D -> Point2D -> Point2D
e_mul = zipWith (\a b -> a * b)

-- element-wise division
e_div : Point2D -> Point2D -> Point2D
e_div = zipWith (\a b -> a / b)

-------------------------------------------------------
-- element-wise operators with a scalar on the right --
-------------------------------------------------------

-- addition with a scalar on the right
pls : Point2D -> Float -> Point2D
pls p s = e_pls p (from1D s)

-- substraction with a scalar on the right
min : Point2D -> Float -> Point2D
min p s = e_min p (from1D s)

-- multiplication with a scalar on the right
mul : Point2D -> Float -> Point2D
mul p s = e_mul p (from1D s)

-- division with a scalar on the right
div : Point2D -> Float -> Point2D
div p s = e_div p (from1D s)

----------------------
-- vector functions --
----------------------

-- dot product
dot : Point2D -> Point2D -> Float
dot a b = fold (\a b -> a+b) <| e_mul a b

-- magnitude
magn : Point2D -> Float
magn p = sqrt <| dot p p

-- create a vector with this value for both dimensions
from1D : Float -> Point2D
from1D a = point2D a a

-- calculate the unit vector given the magnitude of that vector
unit' : Point2D -> Float -> Point2D
unit' = div

-- unit vector (normalized vector)
unit : Point2D -> Point2D
unit p = unit' p <| magn p

-- get both the magnitude and the unit vector
breakDown : Point2D -> (Float, Point2D)
breakDown p = let m = magn p
              in (m, unit' p m)
