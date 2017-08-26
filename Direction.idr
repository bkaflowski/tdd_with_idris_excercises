data Direction = North
               | East
               | South
               | West

turnClockwise : Direction -> Direction
turnClockwise North = East
turnClockwise East = South
turnClockwise South = West
turnClockwise West = North

||| Represents shapes
data Shape = ||| A triangle, with its base length and height
             Triangle Double Double
           | ||| A rectangle, with its length and height
             Rectangle Double Double
           | ||| A circle, with its radius
             Circle Double

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius

data Picture = Primitive Shape
             | Combine Picture Picture
             | Rotate Double Picture
             | Translate Double Double Picture

rectangle : Picture
rectangle = Primitive (Rectangle 20 10)

circle : Picture
circle = Primitive (Circle 5)

triangle : Picture
triangle = Primitive (Triangle 10 10)

testPicture : Picture
testPicture = Combine (Translate 5 5 rectangle)
              (Combine (Translate 35 5 circle)
              (Translate 15 25 triangle))

%name Shape shape, shape1, shape2
%name Picture pic, pic1, pic2

pictureArea : Picture -> Double
pictureArea (Primitive shape) = area shape
pictureArea (Combine pic pic1) = pictureArea pic + pictureArea pic1
pictureArea (Rotate x pic) = pictureArea pic
pictureArea (Translate x y pic) = pictureArea pic

data Biggest = NoTriangle | Size Double

computeTriangle : (shape : Shape) -> Biggest
computeTriangle (Triangle x y) = Size (0.5 * x * y)
computeTriangle (Rectangle x y) = NoTriangle
computeTriangle (Circle x) = NoTriangle

findBigger : Biggest -> Biggest -> Biggest
findBigger NoTriangle NoTriangle = NoTriangle
findBigger NoTriangle (Size x) = (Size x)
findBigger (Size x) NoTriangle = (Size x)
findBigger (Size x) (Size y) = if x >= y then (Size x)
                                         else (Size y)

biggestTriangle : Picture -> Biggest
biggestTriangle (Primitive shape) = computeTriangle shape
biggestTriangle (Combine pic pic1) = findBigger (biggestTriangle pic) (biggestTriangle pic1)
biggestTriangle (Rotate x pic) = biggestTriangle pic
biggestTriangle (Translate x y pic) = biggestTriangle pic

triangle1 : Picture
triangle1 = Primitive (Triangle 50 50)

triangle2 : Picture
triangle2 = Primitive (Triangle 10 10)

triangle3 : Picture
triangle3 = Primitive (Triangle 1 1)

testPicture2 : Picture
testPicture2 = Combine (Translate 5 5 triangle1)
              (Combine (Translate 35 5 triangle2)
              (Translate 15 25 triangle3))
