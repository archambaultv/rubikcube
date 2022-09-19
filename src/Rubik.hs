module Rubik
    ( 
        Color(..),
        RubikCube(..),
        RubikFace,
        getFaces,
        solvedCube
    ) where

import Data.List (intercalate)

-- A rubik cube is made of 6 faces, each one has 9 tiles. Each tile can have one
-- of the six possible colors.
data Color = White | Red | Blue | Yellow | Orange | Green
    deriving (Eq, Ord)

instance Show Color where
    show White = "W"
    show Red = "R"
    show Blue = "B"
    show Yellow = "Y"
    show Orange = "O"
    show Green = "G"

-- So that makes 54 tiles in total. But not all possible combination of colors
-- are possible so we do not have 54^6 possible combinations. In fact the center
-- tiles of each face are always in the same relative position so we can take
-- advantage of that to define a coordinate system that is independant of how
-- someone holds the rubik cude.

-- We simple define a rubik cube has the list of all 54 faces. The first 9 tiles
-- are is the white face, the second the red face, third blue face and so on.
data RubikCube = RubikCube [Color] -- A list of length 54
                deriving (Eq)

type RubikFace = [Color] -- A list of length 9

getFaces :: RubikCube -> (RubikFace, RubikFace, RubikFace, RubikFace, RubikFace, RubikFace)
getFaces (RubikCube xs) = toTuple $ go xs
    where go [] = []
          go ys = take 9 ys : go (drop 9 ys)

          toTuple [a,b,c,d,e,f] = (a,b,c,d,e,f)
          toTuple _ = error "RubikCube wrong list size"

-- Prints a rubik cube as a 2D projection
-- as if white is the front face
--         0 1 2
--         3 R 5
--         6 7 8
-- 
-- 0 1 2   0 1 2   0 1 2    
-- 3 B 5   3 W 5   3 G 5
-- 6 7 8   6 7 8   6 7 8
-- 
--         0 1 2
--         3 O 5
--         6 7 8
-- 
--         0 1 2
--         3 Y 5
--         6 7 8

printRubikCube :: RubikCube -> String
printRubikCube xs =
    let (w,r,b,y,o,g) = getFaces xs
        top :: [String]
        top =  rubikFaceMatrix r
        center :: [String]
        center = concatMatrix 
               $ map rubikFaceMatrix [b, w, g]
        bottom1 :: [String]
        bottom1 = rubikFaceMatrix o
        bottom2 :: [String]
        bottom2 = rubikFaceMatrix y
    in
        intercalate "\n\n" 
        $ map (intercalate "\n") 
        [pad top, center, pad bottom1, pad bottom2]
    where
        rubikFaceMatrix :: RubikFace -> [String]
        rubikFaceMatrix u =
            let colors = map show u
                firstRow = take 3 colors
                secondRow = take 3 $ drop 3 colors
                thirdRow = drop 6 colors
            in map (intercalate " ") $ [firstRow, secondRow, thirdRow]

        concatMatrix :: [[String]] -> [String]
        concatMatrix u  = [intercalate "  " $ map head u,
                           intercalate "  " $ map (head . tail) u,
                           intercalate "  " $ map last u]

        pad :: [String] -> [String]
        pad u = map ("       " ++) u

instance Show RubikCube where
    show rc = printRubikCube rc

-- The initial cube is well sorted
solvedCube :: RubikCube
solvedCube = RubikCube 
            $ concatMap (\c -> replicate 9 c) [White, Red, Blue, Yellow, Orange, Green]


-- Swap two element of the list, the list must be long enough
swap :: Int -> Int -> [a] -> [a]
swap x y ls | x > y = swap y x ls
swap x y ls | x == y = ls
swap x y ls =
    let (start, ls1) = splitAt x ls 
        x' = head ls1
        (middle, ls2) = splitAt (y - x - 1) (tail ls1)
        y' = head ls2
    in start ++ [y'] ++ middle ++ [x'] ++ tail ls2 

-- Permute many elements, the first taking the place of the second and so forth
-- until the last takes the place of the first
permute :: [Int] -> [a] -> [a]
permute [] ls = ls
permute (x : xs) ls = foldl swap' ls xs
    where
    swap' :: [a] -> Int -> [a]
    swap' ls b = swap x b ls

-- move :: Move -> RubikCube -> RubikCube
-- move FCw (RubikCube xs) = RubikCube $
--      foldl permute' xs [[1,3,9,7],[2,6,8,4],[]]
--     where
--     permute' :: [Color] -> [Int] -> [Color]
--     permute' ls xs = permute xs ls
