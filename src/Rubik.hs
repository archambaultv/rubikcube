module Rubik
    ( 
        Color(..),
        RubikFace,
        faceToList,
        RubikCube(..),
        solvedCube,
        Rotation(..),
        Move(..),
        move,
        moves,
        repl
    ) where

import Data.List (intercalate)
import System.IO (stdout, hFlush)

-- A rubik cube is made of 6 faces, each one has 9 tiles. Each tile can have one
-- of the six possible colors.
data Color = White | Red | Blue | Yellow | Orange | Green
    deriving (Eq)

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

-- We simple define a rubik cube has a list of all 54 faces. The first 9 tiles
-- are is the white face, the second the red face, third blue face and so on.
-- We use tuples 

-- A face has 9 tiles
type RubikFace = (Color, Color, Color,
                  Color, Color, Color,
                  Color, Color, Color)

faceToList :: RubikFace -> [Color]
faceToList (a,b,c,d,e,f,g,h,i) = [a,b,c,d,e,f,g,h,i]

data RubikCube = RubikCube {
    whiteFace :: RubikFace,
    redFace :: RubikFace,
    blueFace :: RubikFace,
    yellowFace :: RubikFace,
    orangeFace :: RubikFace,
    greenFace :: RubikFace
    }
    deriving (Eq)


-- getFaces :: RubikCube -> (RubikFace, RubikFace, RubikFace, RubikFace, RubikFace, RubikFace)
-- getFaces (RubikCube xs) = toTuple $ go xs
--     where go [] = []
--           go ys = take 9 ys : go (drop 9 ys)

--           toTuple [a,b,c,d,e,f] = (a,b,c,d,e,f)
--           toTuple _ = error "RubikCube wrong list size"

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
printRubikCube (RubikCube w r b y o g) =
    let top :: [String]
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
            let colors = map show $ faceToList u
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
             (mkFace White) 
             (mkFace Red)
             (mkFace Blue) 
             (mkFace Yellow) 
             (mkFace Orange) 
             (mkFace Green)

    where mkFace :: Color -> RubikFace
          mkFace c = (c,c,c,c,c,c,c,c,c)

-- Moves. There is 3 axis and 3 columns for each axis to move. Each column can
-- move clockwise or counterclockwise in relation to the axis. So 18 moves in
-- total. But there is only 6 basics moves, the 12 others are a combination of
-- those 6. So we first define the 6 basics moves and then from them other 12.
-- Each move is equivalent to rotating the appropriate face (bases on the color)
-- clockwise when view the face up front


-- Rotate clockwise a RubikFace
rotateFaceCWise :: RubikFace -> RubikFace
rotateFaceCWise (f1,f2,f3,f4,f5,f6,f7,f8,f9) = 
            --    ^  ^  ^  ^  ^  ^  ^  ^  ^
                (f7,f4,f1,f8,f5,f2,f9,f6,f3)

-- Rotate anti clockwise a RubikFace
rotateFaceAntiCWise :: RubikFace -> RubikFace
rotateFaceAntiCWise = rotateFaceCWise . rotateFaceCWise . rotateFaceCWise

-- Rotate a RubikFace half a turn
rotateHalfTurn :: RubikFace -> RubikFace
rotateHalfTurn = rotateFaceCWise . rotateFaceCWise

-- Rotates the four neighbours of the white face clockwise
rotateWhiteSides :: (RubikFace, RubikFace, RubikFace, RubikFace)
                 -> (RubikFace, RubikFace, RubikFace, RubikFace)
rotateWhiteSides ((r0,r1,r2,r3,r4,r5,r6,r7,r8),
                  (g0,g1,g2,g3,g4,g5,g6,g7,g8),
                  (o0,o1,o2,o3,o4,o5,o6,o7,o8),
                  (b0,b1,b2,b3,b4,b5,b6,b7,b8)) =
                 
                 ((r0,r1,r2,r3,r4,r5,b8,b5,b2),
                  (r6,g1,g2,r7,g4,g5,r8,g7,g8),
                  (g6,g3,g0,o3,o4,o5,o6,o7,o8),
                  (b0,b1,o0,b3,b4,o1,b6,b7,o2))

-- Apply a basic move to a rubik cube. Rotating clockwise the face with same
-- color as the first parameter
basicMove :: Color -> RubikCube -> RubikCube
basicMove White (RubikCube w r b y o g) =
    let w' = rotateFaceCWise w
        (r',g',o',b') = rotateWhiteSides (r,g,o,b)
    in RubikCube w' r' b' y o' g'
basicMove Red (RubikCube w r b y o g) =
    let r' = rotateFaceCWise r
        -- When computing the neighbours, we can pretend that red is the front
        -- face (at the place of white) and reuse rotateWhiteSides. But blue and
        -- green are rotated to take into account that the cube rotated
        (y',g',w',b') = rotateWhiteSides (y,rotateFaceAntiCWise g,w,rotateFaceCWise b)
    in RubikCube w' r' (rotateFaceAntiCWise b') y' o (rotateFaceCWise g')
basicMove Blue (RubikCube w r b y o g) =
    let b' = rotateFaceCWise b
        -- We use the same trick as for MRed
        (r',w',o',y') = rotateWhiteSides (rotateFaceAntiCWise r,w,rotateFaceCWise o, rotateHalfTurn y)
    in RubikCube w' (rotateFaceCWise r') b' (rotateHalfTurn y') (rotateFaceAntiCWise o') g
basicMove Yellow (RubikCube w r b y o g) = 
    let y' = rotateFaceCWise y
        -- We use the same trick as for MRed
        (r',b',o',g') = rotateWhiteSides (rotateHalfTurn r, b,rotateHalfTurn o, g)
    in RubikCube w (rotateHalfTurn r')  b' y' (rotateHalfTurn o') g'
basicMove Orange (RubikCube w r b y o g) = 
    let o' = rotateFaceCWise o
        -- We use the same trick as for MRed
        (w',g',y',b') = rotateWhiteSides (w,rotateFaceCWise g,y, rotateFaceAntiCWise b)
    in RubikCube w' r (rotateFaceCWise b') y' o' (rotateFaceAntiCWise g')
basicMove Green (RubikCube w r b y o g) = 
    let g' = rotateFaceCWise g
        -- We use the same trick as for MRed
        (r',y',o',w') = rotateWhiteSides (rotateFaceCWise r,rotateHalfTurn y,rotateFaceAntiCWise o,w)
    in RubikCube w' (rotateFaceAntiCWise r') b (rotateHalfTurn y') (rotateFaceCWise o') g'

-- A rotation can be clockwise or anticlockwise
data Rotation = CWise | AntiCWise
    deriving (Eq)

instance Show Rotation where
    show CWise = "CW"
    show AntiCWise = "ACW"

instance Read Rotation where
    readsPrec _ ('C' :'W' : xs)  = [(CWise, xs)]
    readsPrec _ ('A' : 'C' :'W' : xs) = [(AntiCWise, xs)]
    readsPrec _ _ = []

-- We can move each face clock wise or anti clock wise
data Move = Move Color Rotation
    deriving (Eq)

instance Show Move where
    show (Move c CWise) = show c
    show (Move c AntiCWise) = show c ++ "'"

readMove :: String -> Maybe (Move, String)
readMove ('W' : '\'' : xs)  = Just (Move White AntiCWise, xs)
readMove ('W' : xs)  = Just (Move White CWise, xs)
readMove ('R' : '\'' : xs)  = Just (Move Red AntiCWise, xs)
readMove ('R' : xs)  = Just (Move Red CWise, xs)
readMove ('B' : '\'' : xs)  = Just (Move Blue AntiCWise, xs)
readMove ('B' : xs)  = Just (Move Blue CWise, xs)
readMove ('Y' : '\'' : xs)  = Just (Move Yellow AntiCWise, xs)
readMove ('Y' : xs)  = Just (Move Yellow CWise, xs)
readMove ('O' : '\'' : xs)  = Just (Move Orange AntiCWise, xs)
readMove ('O' : xs)  = Just (Move Orange CWise, xs)
readMove ('G' : '\'' : xs)  = Just (Move Green AntiCWise, xs)
readMove ('G' : xs)  = Just (Move Green CWise, xs)
readMove _ = Nothing

readMoves :: String -> Maybe [Move]
readMoves s = do
    (m, x) <- readMove s
    fmap (m :) (if x == [] then return [] else readMoves x)

instance Read Move where
    readsPrec _ s = 
        case readMove s of
            Just (m, s') -> [(m, s')]
            Nothing -> []

-- Similar to a basic move, but can rotate anti clockwise
move :: Move -> RubikCube -> RubikCube
move (Move c CWise) rc = basicMove c rc
move (Move c AntiCWise) rc = m $ m $ m rc
    where m = basicMove c

-- Apply a series of move together
moves :: [Move] -> RubikCube -> RubikCube
moves ms rc = foldl (flip move) rc ms

repl :: IO ()
repl = myREPL (ReplState solvedCube [])

data ReplState = ReplState {cube :: RubikCube, vars :: [(String, [Move])]}

-- A small repl to play with the rubik cube
myREPL :: ReplState -> IO ()
myREPL st = do
    input <- read_
    eval_ input

    where read_ :: IO String
          read_ = putStr "Moves> " >> hFlush stdout >> getLine

          eval_ :: String -> IO ()
          eval_ ":quit" = return ()
          eval_ ":q" = eval_ ":quit"
          eval_ ":restart" = myREPL st{cube = solvedCube}
          eval_ ":r" = eval_ ":restart"
          eval_ s | take 4 s == "let " = addVar (drop 4 s)
          eval_ s =
            case lookup s (vars st) of
                Just mv -> mvCube mv
                Nothing ->
                    case readMoves s of
                        Nothing -> putStrLn "Unable to parse" >> myREPL st
                        Just mv -> 
                            let rc = moves mv (cube st)
                            in putStrLn (show rc) >> myREPL (st{cube = rc})

          addVar :: String -> IO ()
          addVar s =
              let (name, s') = span (/= ' ') s
                  xs = dropWhile (== ' ') $ drop 1 $ dropWhile (== ' ') s'
              in case readMoves xs of
                  Nothing -> putStrLn "Unable to parse" >> myREPL st
                  Just mv -> myREPL st{vars = (name, mv) : vars st}

          mvCube :: [Move] -> IO ()
          mvCube mv = let rc = moves mv (cube st)
                      in putStrLn (show rc) >> myREPL (st{cube = rc})