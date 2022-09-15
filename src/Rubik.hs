module Rubik
    ( 
    ) where

import Data.List (intercalate, splitAt)

-- A rubik cube is made of 6 faces, each one has 9 tiles. Each tile can have one
-- of the six possible colors.
data Color = Red | Blue | Yellow | Green | Orange | White
    deriving (Eq)

instance Show Color where
    show Red = "R"
    show Blue = "B"
    show Yellow = "Y"
    show Green = "G"
    show Orange = "O"
    show White = "W"

-- We name the faces from the point of view of the person solving the cube,
-- looking at it
data Face = Front | Back | FRight | FLeft | Up | Down
    deriving (Show, Eq)

-- The easiest representation of a rubik cube is a list of length 54, for the 54
-- tiles.
data RubikCube = RubikCube [Color]

-- We can assign each face a slot on the RubikCube list. The up face is the
-- first 9th color. Then the front face, down, back, left and right. By
-- definition we will consider the first element of the Front list to be the
-- upper left corner of this face. 
face :: Face -> RubikCube -> [Color]
face Front (RubikCube xs) = take 9 xs
face Down (RubikCube xs) = take 9 $ drop 9 xs
face Back (RubikCube xs) = take 9 $ drop 18 xs
face Up (RubikCube xs) = take 9 $ drop 27 xs
face FLeft (RubikCube xs) = take 9 $ drop 36 xs
face FRight (RubikCube xs) = drop 45 xs

instance Show RubikCube where
    show c = intercalate "\n"
             ["U : " ++ (show $ face Up c),
              "F : " ++ (show $ face Front c),
              "D : " ++ (show $ face Down c),
              "B : " ++ (show $ face Back c),
              "L : " ++ (show $ face FLeft c),
              "R : " ++ (show $ face FRight c)]


-- The initial cube is well sorted
defaultCube :: RubikCube
defaultCube = RubikCube 
            $ concatMap (\c -> replicate 9 c) [Red, White, Orange, Yellow, Blue, Green]

-- We can make moves to shuffle the rubik cube. This is actually a permutation
-- of the list of colors. There is 3 axis of rotation (clockwise -
-- anticlockwise, right - left, up - down) and each axis can move in two
-- directions. There is three "columns" that can move for each axis, so 18 moves
-- in total.
data Move = FCw  -- Front face, clock wise
          | FAcw -- Front face, anti clock wise
          | MCw  -- Middle face, clock wise
          | MCc  -- Middle face, anti clock wise
          | BCw  -- Back face, clock wise
          | BCc  -- Back face, anit clock wise

          | RF   -- Right face, forward
          | RB   -- Right face, backward
          | MF   -- Middle face, forward
          | MB   -- Middle face, backward
          | LF   -- Left face, forward
          | LB   -- Left face, backward

          | UR   -- Up face, to the right
          | UL   -- Up face, to the left
          | MR   -- Middle face, to the right
          | ML   -- Middle face, to the left
          | DR   -- Down face, to the right
          | DL   -- Down face, to the left

-- Now we can define how the moves affect the rubik cube. This will also define
-- the numbering for each face, considering that the first element of the Front
-- face list is the upper left corner.

-- Since moves are perumutation, it will be easier to define them if we have
-- some basic functions to handle permutation

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
