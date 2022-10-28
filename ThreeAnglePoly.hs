{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE RecordWildCards #-}
module ThreeAnglePoly where

import Control.Applicative
import Data.Foldable (toList)
import Data.List (sort)
import Text.Printf

data Triple a = Triple { t1, t2, t3 :: a }
  deriving (Show, Eq, Ord, Functor, Foldable)

instance Applicative Triple where
  pure a = Triple a a a
  (<*>) (Triple ft1 ft2 ft3) (Triple t1 t2 t3)
    = Triple (ft1 t1) (ft2 t2) (ft3 t3)

instance Num a => Num (Triple a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  abs = fmap abs
  negate = fmap negate
  signum = fmap signum
  fromInteger = pure . fromInteger

type Point = Triple
pattern Point { x, y, z } = Triple x y z
{-# COMPLETE Point #-}

type Triangle = Triple
pattern Triangle { ab, bc, ca } = Triple ab bc ca
{-# COMPLETE Triangle #-}

sortTriangle :: Ord a => a -> a -> a -> Triangle a
sortTriangle x y z =
  let [ab,bc,ca] = reverse $ sort [x,y,z]
  in
  Triangle {..}

area :: (Num a, Floating a) => Triangle a -> a
area Triangle {..} =
  let s = (ab + bc + ca) / 2
  in
  sqrt $ s * (s - ab) * (s - bc) * (s - ca)

height :: (Num a, Floating a) => Triangle a -> a
height triangle = area triangle * 2 / ab triangle

pythagorasArm :: (Num a, Floating a) => a -> a -> a
pythagorasArm hyp x = sqrt $ hyp ** 2 - x ** 2

cOffsetFromCenter :: (Num a, Floating a) => Triangle a -> a
cOffsetFromCenter triangle@Triangle {..} = pythagorasArm bc (height triangle) - ab / 2

allAbout :: (Num a, Floating a) => Triangle a -> (a,a,a)
allAbout triangle = (area triangle, height triangle, cOffsetFromCenter triangle)

pyramidCrossSection :: (Ord a, Num a, Floating a) => Triangle a -> a -> Triangle a
pyramidCrossSection tri r =
  let cToPeakProjection = pythagorasArm r (cOffsetFromCenter tri)
      abToPeakProjection = height (Triangle (ab tri) r r)
      abToCProjection = height tri
  in
  Triangle abToCProjection cToPeakProjection abToPeakProjection

data Pyramid a = Pyramid { a, b, c, h :: a }
  deriving (Show, Eq, Ord, Functor)

pyramidPoints :: (Ord a, Num a, Floating a) => Triangle a -> a -> Pyramid (Point a)
pyramidPoints tri r =
  let a = Point 0 0 0
      b = Point 0 (ab tri) 0
      c = Point (height tri) (ab tri / 2 + cOffsetFromCenter tri) 0
      section = pyramidCrossSection tri r
      h = Point (pythagorasArm (ca section) (height section)) (ab tri / 2) (height section)
  in
  Pyramid {..}

relativeToH :: (Num a, Floating a) => Pyramid (Point a) -> Pyramid (Point a)
relativeToH pyramid = fmap (subtract (h pyramid)) pyramid

ngonShortDiagonal :: (Num a, Floating a) => Int -> a
ngonShortDiagonal n = 2 * sin ((fromIntegral n - 2) * pi / fromIntegral n / 2)

specToTriangle :: (Ord a, Num a, Floating a) => (Int, Int, Int) -> Triangle a
specToTriangle (n1, n2, n3) = sortTriangle (ngonShortDiagonal n1) (ngonShortDiagonal n2) (ngonShortDiagonal n3)

toOpenSCAD :: forall a. (Floating a, PrintfArg a) => Pyramid (Point a) -> String
toOpenSCAD pyramid =
  openSCADPrelude ++
  unlines
    [ "scale(60) difference() {"
    , " union() {"
    , "  difference() {"
    , "   sphere(0.2, $fn=16);"
    , "   translate([0,0,-1]) cube(2, center=true, $fn=16);"
    , "  }"
    , "  translate([0,0,-0.05]) cylinder(0.05, 0.2, 0.2, $fn=16);"
    , " }"
    , line a h (6.1 / 60)
    , line b h (6.1 / 60)
    , line c h (6.1 / 60)
    , "}"
    ]
  where
    Pyramid {..} = (* Point 1 1 (-1)) <$> relativeToH pyramid

    point :: Point a -> String
    point Point {..} = printf "[%g,%g,%g]" x y z

    line :: Point a -> Point a -> a -> String
    line pt0 pt1 dia = printf " line(%s, %s, diameter=%g);" (point pt0) (point pt1) dia

    openSCADPrelude :: String
    openSCADPrelude = "\
    \// Find the unitary vector with direction v. Fails if v=[0,0,0].           \n\
    \function unit(v) = norm(v)>0 ? v/norm(v) : undef;                          \n\
    \// Find the transpose of a rectangular matrix                              \n\
    \function transpose(m) = // m is any rectangular matrix of objects          \n\
    \  [ for(j=[0:len(m[0])-1]) [ for(i=[0:len(m)-1]) m[i][j] ] ];              \n\
    \// The identity matrix with dimension n                                    \n\
    \function identity(n) = [for(i=[0:n-1]) [for(j=[0:n-1]) i==j ? 1 : 0] ];    \n\
    \                                                                           \n\
    \// computes the rotation with minimum angle that brings a to b             \n\
    \// the code fails if a and b are opposed to each other                     \n\
    \function rotate_from_to(a,b) =                                             \n\
    \    let( axis = unit(cross(a,b)) )                                         \n\
    \    axis*axis >= 0.99 ?                                                    \n\
    \        transpose([unit(b), axis, cross(axis, unit(b))]) *                 \n\
    \            [unit(a), axis, cross(axis, unit(a))] :                        \n\
    \        identity(3);                                                       \n\
    \                                                                           \n\
    \module line(p0, p1, diameter=1) {                                          \n\
    \    v = p1-p0;                                                             \n\
    \    translate(p0)                                                          \n\
    \        // rotate the cylinder so its z axis is brought to direction v     \n\
    \        multmatrix(rotate_from_to([0,0,1],v))                              \n\
    \            cylinder(d=diameter, h=norm(v), $fn=16);                       \n\
    \}                                                                          \n\
    \"
