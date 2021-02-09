{- |
Module: Shape.hs
-}

module Shape where

data Point = Point { x :: Double, y :: Double } deriving (Show)

 -- Rectangles

data Rectangle = Rectangle { p1 :: Point, p2 :: Point } deriving (Show)
--p1 contains coordinate of the rectangle's center while p2 contains length and breadth
-- Circles

data Circle = Circle { m :: Point, r :: Double } deriving (Show)
--m contains center of circle and r contains radius
-- Triangles

data Triangle = Triangle { a :: Point, b :: Point, c :: Point } deriving (Show)
--a contains cooridnate,b contains length of one side and base, c contains height and other side

--data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)
--10.3 (a)
class Area ax where --type ax is an instance of the class Area if
    area::ax->Double

instance Area Rectangle where--object of type class Area
    area (Rectangle _ (Point x y))=(x*y)
	
instance Area Circle where--object of type class Area
    area (Circle _ r) =(3.14)*r*r
	
instance Area Triangle where--object of type class Area
    area (Triangle _ (Point a b)(Point c d))=(0.5)*d*b

--10.3 (b)

class BoundingBox ax where --extending typeclasses
    bbox::ax->Rectangle

instance BoundingBox Rectangle where--object of type class BoundingBox
    bbox (Rectangle (Point a b) (Point x y))=Rectangle (Point a b) (Point x y) --x*y

instance BoundingBox Circle where--object of type class BoundingBox
    bbox (Circle (Point a b) r) = Rectangle (Point a b) (Point (2*r) (2*r))--(2*r)*(2*r) --rectangle sorrounding the circle is a square, distance from center of circle to squares one side is radius so length of square= 2*r

instance BoundingBox Triangle where--object of type class BoundingBox
    bbox (Triangle (Point f e) (Point a b)(Point c d)) = Rectangle (Point f e) (Point d b)--d*b