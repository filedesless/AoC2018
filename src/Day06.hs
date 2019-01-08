module Day06 where

import Data.List
import Data.List.Split( splitOn )
import Data.Function( on )

data Point = Pt Int Int deriving Eq
data Location = Loc { locChar :: Char, locPt :: Point } deriving Eq

dist :: Point -> Point -> Int
dist (Pt a b) (Pt c d) = ((+) `on` abs) (a - c) (b - d)

grid :: [Location] -> Point -> Char
grid locs (Pt x y)
  | Just loc <- find ((== Pt x y) . locPt) locs = locChar loc
  | length (filter ((== snd mdistance) . snd) distances) > 1 = '.'
  | otherwise = locChar $ fst mdistance
  where
    mdistance = minimumBy (compare `on` snd) distances
    distances = map (\(Loc c pt) -> (Loc c pt, dist pt (Pt x y))) locs

day06a :: String -> Int
day06a s = maximum $ map (\c -> length $ filter (== c) fullGrid) finites
  where
    fullGrid = map (grid locs) [ Pt x y | (x, y) <- numGrid ]
    numGrid = [ (x, y) | y <- [0..uy], x <- [0..ux] ]
    finites = charset \\ infinites
    infinites = map (grid locs)
      [ Pt x y | (x, y) <- numGrid, x <= 0 || x >= ux || y <= 0 || y >= uy ]
    (ux, uy) = (fst $ maximumBy (compare `on` fst) tups, snd $ maximumBy (compare `on` snd) tups)
    charset = map locChar locs
    locs = [ Loc c (Pt x y) | (c, (x, y)) <- zip (['a'..'z'] ++ ['A'..'Z']) tups ]
    tups = [ (read x :: Int, read y :: Int) | (x:y:rest) <- map (splitOn ", " ) $ lines s ]


day06b :: String -> Int
day06b = undefined
