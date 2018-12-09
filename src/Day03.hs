module Day03 where

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List

data Point = Point Int Int deriving (Show, Eq, Ord)
data Claim = Claim Int Point Point deriving (Show, Eq)

getId :: String -> Int
getId w = read (tail w) :: Int

readT :: (String, String) -> (Int, Int)
readT (s1, s2) = (read s1, read (tail s2))

getCoord :: String -> Point
getCoord w = (uncurry Point) $ readT (span (/= ',') (init w))

getArea :: String -> Point
getArea w = (uncurry Point) $ readT (span (/= 'x') w)

getClaim :: String -> Claim
getClaim s =
    Claim (getId (head ws)) (getCoord (ws !! 2)) (getArea (ws !! 3))
    where ws = words s

claims :: String -> [Claim]
claims = map getClaim . lines

pointInClaim :: Point -> Claim -> Bool
pointInClaim (Point x1 y1) (Claim _ (Point x2 y2) (Point w2 h2)) =
  x1 >= x2 && x1 < x2 + w2 &&
  y1 >= y2 && y1 < y2 + h2

pointsOfClaim :: Claim -> [Point]
pointsOfClaim (Claim _ (Point x y) (Point w h)) =
  [ Point a b | a <- [x..x+w-1], b <- [y..y+h-1] ]

contours :: Claim -> [Point]
contours (Claim _ (Point x y) (Point w h)) =
  [ Point a b | a <- [x, xw], b <- [y..yh] ] ++
  [ Point a b | a <- [x..xw], b <- [y, yh] ]
  where (xw, yh) = (pred x + w, pred y + h)

overlap :: Claim -> Claim -> Bool
overlap claim1 claim2 =
  any (flip pointInClaim claim1) (contours claim2) ||
  any (flip pointInClaim claim2) (contours claim1)

f :: Set Point -> [Claim] -> Int
f seen [] = Set.size seen
f seen (x:xs) = f (Set.union seen points) xs
  where
    points = Set.fromList [ point | point <- pointsOfClaim x,
                            point `Set.notMember` seen,
                            any (pointInClaim point) xs ]

g :: [Claim] -> Int
g xs = case find (\x -> not $ any (overlap x) (delete x xs)) xs of
  Just (Claim i _ _) -> i
  otherwise -> error "No free claim found"

day03a :: String -> Int
day03a = f Set.empty . claims

day03b :: String -> Int
day03b = g . claims
