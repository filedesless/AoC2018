module Day03 where

data Point = Point Int Int deriving (Show, Eq)
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

corners :: Claim -> [Point]
corners (Claim _ (Point x y) (Point w h)) =
  [Point x y, Point xw y, Point x yh, Point xw yh]
  where (xw, yh) = (pred x + w, pred y + h)

pointInClaim :: Claim -> Point -> Bool
pointInClaim (Claim _ (Point x1 y1) (Point w1 h1)) (Point x2 y2) =
  x2 >= x1 && x2 < x1 + w1 &&
  y2 >= y1 && y2 < y1 + h1

overlap :: Claim -> Claim -> Bool
overlap claim1 claim2 =
  any (pointInClaim claim1) (corners claim2) ||
  any (pointInClaim claim2) (corners claim1)

day03a :: String -> Int
day03a = error "Not implemented yet"

day03b :: String -> String
day03b = error "Not implemented yet"
