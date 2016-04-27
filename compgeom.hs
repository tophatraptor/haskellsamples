-- A point is a point in the xy plane, represented by x and y coordinates
-- E.g. (Point 0.0 0.0) is the origin, (Point (-1) (1)) is in the top left
-- quadrant.

module Lab2 (
Point(Point),
LineSegment(LineSegment),
Path(Line, VerticalLine, Parabola),
Shape (Triangle, Quadrilateral, Circle),
BoundingBox(BoundingBox),
getx,
gety,
toLine,
slope,
lslope,
lint,
within,
boundShape,
evalParab,
intersects,
pathintersect,
intersectsBB,
mightIntersectShape
) where

data Point = Point Double Double
    deriving (Show, Eq)

-- A line segment is a straight line of finite length, defined by its
-- two end points.   E.g. (LineSegment (Point 0 0) (Point 1 1)) is a
-- line segment from the origin to the coordinate (1, 1)
data LineSegment = LineSegment Point Point
    deriving (Show, Eq)


-- A Path is a 2D path in the xy-plane.  The idea is that Path can be
-- extended to support straight lines, curves, and arbitrary paths,
-- but currently there is only one data constructor for Path: Line.
data Path =
-- Line represents an infinite straight line defined by its slope a
-- and its y intercept b, ie. by the equation y = ax + b
    Line Double Double --takes inputs a&b for the equation y = ax + b
    | VerticalLine Double --takes the x-intercept of the vertical line
    | Parabola Double Double Double --takes a, b, and c for the parabola form of ax^2+bx+c
    deriving (Show, Eq)

data Shape =
  Triangle Point Point Point  -- Takes 3 vertices
  | Quadrilateral Point Point Point Point --Takes 4 vertices
  | Circle Point Double -- Takes coordinates of center and a value for radius
  deriving (Show, Eq)

data BoundingBox = BoundingBox Point Point --lower left & upper right corner
  deriving (Show, Eq)
--Actual functions below here

getx::Point->Double --Helper function that retrieves the x coordinate of a point
getx (Point a b) = a
gety::Point->Double --Helper function that retrieves the y coordinate of a point
gety (Point a b) = b

xlist::[Point]->[Double]
xlist xs = map getx xs
ylist::[Point]->[Double]
ylist ys = map gety ys
--Helper function that returns a list of all the x values from a list of points

toLine::LineSegment->Path
toLine (LineSegment (Point x1 y1) (Point x2 y2))
    | (x1 /= x2) = Line m ((-m)*x2+y2)
    | otherwise = VerticalLine x1
    where m = slope (Point x1 y1) (Point x2 y2)

slope:: Point->Point->Double
slope (Point x1 y1) (Point x2 y2) = (y2-y1)/(x2-x1)
--Returns the slope of two points
lslope::Path->Double
lslope (Line a b) = a
--Returns the slope of the line it's passed
lslope (VerticalLine a) = read "Infinity" :: Double
lint::Path->Double
lint (Line a b) = b
lint (VerticalLine b) = b
--Returns the x intercept

within::LineSegment->Point->Bool
within (LineSegment (Point a b) (Point c d)) (Point e f)
  | (e<=maximum[a,c] && e>=minimum[a,c]) && (f<=maximum[b,d] && f>=minimum[b,d]) = True
  | otherwise = False
--Determines whether or not a point lies along a line segment
boundShape::Shape->BoundingBox
boundShape (Triangle a b c) = BoundingBox (Point (minimum $ xlist [a,b,c]) (minimum $ ylist [a,b,c])) (Point (maximum $ xlist [a,b,c]) (maximum $ ylist [a,b,c]))

boundShape (Quadrilateral a b c d) = BoundingBox (Point (minimum $ xlist [a,b,c,d]) (minimum $ ylist [a,b,c,d])) (Point (maximum $ xlist [a,b,c,d]) (maximum $ ylist [a,b,c,d]))

boundShape (Circle c r) = BoundingBox (Point (getx(c)-r) (gety(c)-r)) (Point (getx(c)+r) (gety(c)+r))

evalParab::Path->Double->Point
evalParab (Parabola a b c) x = Point x (a*x*x+b*x+c)
--Returns the coordinates of a designated x value on a parabola. Used to make the Parabola intersects function less horrifying to look at.
intersects:: Path->LineSegment->Bool
intersects (VerticalLine a1) (LineSegment (Point x1 y1) (Point x2 y2))
  | x1<=a1 && a1<=x2 = True
  | otherwise = False
intersects (Line a b) ls
  | lslope secondline == a && lint secondline /= b = False
  | lslope secondline == a && lint secondline == b = True
  | within ls (pathintersect (Line a b) secondline) = True
  |otherwise = False
  where secondline = toLine ls

intersects (Parabola a b c) (LineSegment (Point x1 y1) (Point x2 y2))
  | x1==x2 = within(LineSegment (Point x1 y1) (Point x2 y2)) (evalParab (Parabola a b c) x1) --the case in which the parabola intercepts a vertical line segment
  | ((b-(lslope lin))^2-4*a*(c-(lint lin))) >= 0 =within (LineSegment (Point x1 y1) (Point x2 y2))  (evalParab (Parabola a b c) (((-b+lslope lin)+sqrt((b-(lslope lin))^2-4*a*(c-(lint lin))))/(2*a))) || within (LineSegment (Point x1 y1) (Point x2 y2))  (evalParab (Parabola a b c) (((-b+lslope lin)-sqrt((b-(lslope lin))^2-4*a*(c-(lint lin))))/(2*a)))
  --this is the expression: x = (-b + sqrt(b^2-4ac))/(2a)
  | otherwise = False
  where lin = toLine(LineSegment (Point x1 y1) (Point x2 y2))

pathintersect::Path->Path->Point
pathintersect (Line a b)   (Line c d) = Point ((d-b)/(a-c)) (a*((d-b)/(a-c))+b)
pathintersect (Line a b) (VerticalLine d) = Point d (d*a+b)
pathintersect (VerticalLine d) (Line a b) = Point d (d*a+b)
--setting y = ax + b & y = cx + d equal to each other and solving for x & y yields those respective equations
intersectsBB::BoundingBox->Path->Bool
intersectsBB (BoundingBox (Point a b) (Point c d)) (Line m z)
  | intersects l (LineSegment (Point a b) (Point a d)) = True
  | intersects l (LineSegment (Point a b) (Point c b)) = True
  | intersects l (LineSegment (Point c d) (Point a d)) = True
  | intersects l (LineSegment (Point c d) (Point c b)) = True
  | otherwise = False
  where l = Line m z

intersectsBB (BoundingBox (Point a b) (Point c d)) (Parabola x y z)
  | intersects p (LineSegment (Point a b) (Point a d)) = True
  | intersects p (LineSegment (Point a b) (Point c b)) = True
  | intersects p (LineSegment (Point c d) (Point a d)) = True
  | intersects p (LineSegment (Point c d) (Point c b)) = True
  | otherwise = False
  where p = Parabola x y z

intersectsBB (BoundingBox (Point a b) (Point c d)) (VerticalLine x1) = (a<=x1 && x1<=c)

mightIntersectShape::Shape->Path->Bool
mightIntersectShape s p = intersectsBB (boundShape s) p
