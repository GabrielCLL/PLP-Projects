module Util where

import Graphics.Gloss
import System.Random


-- --------------------------------------------------- -- 

-- Coordinate of something
type Coordinate = (Float, Float)

-- --------------------------------------------------- -- 

-- Inputs
data Directions =  UP | DOWN | LEFT | RIGHT deriving (Eq)
data Decisions =  ACCEPT | PAUSE | EXIT deriving (Eq)
nameGame = "ForCobrinha" :: String

-- --------------------------------------------------- -- 

-- Sizes

-- SizeWindow
data SizeGrid =
    SizeGrid{ gridWidth  :: Float
            , gridHeight :: Float
            }

-- SizePixel
data SizePixel =
    SizePixel{ pixelWidth  :: Float
             , pixelHeight :: Float
             }

 -- Default Sizes

sizeGridDefault =
    SizeGrid {gridWidth = 720, gridHeight = 480}
sizeGridSnake   =
    SizeGrid {gridWidth = 432, gridHeight = 288}
sizeGridHangman =
    SizeGrid {gridWidth = 432, gridHeight = 96}

sizePixelDefault =
    SizePixel {pixelWidth = 9, pixelHeight = 6}
pixelSnake =
    SizePixel {pixelWidth  = 18, pixelHeight = 12}
pixelFood =
    SizePixel {pixelWidth  = 18, pixelHeight = 12}
-- --------------------------------------------------- -- 

-- Board
data Board =
    Board { sizeGrid        :: SizeGrid
          , translateXPos   :: Float
          , translateYPos   :: Float
          , boardColor      :: Color
          }

-- Default Board

boardDefault =
    Board { sizeGrid        = sizeGridDefault
          , translateXPos   = 0
          , translateYPos   = 0
          , boardColor      = dark (dark green)
          }

-- --------------------------------------------------- -- 

-- View

data View =
    View { pixelView   :: SizePixel
         , gridView    :: SizeGrid
         , colorList   :: [Color]
         }

-- --------------------------------------------------- -- 

-- Game Board

data GameBoardBackGround =
    GameBoardBackGround {sBoardBack   :: Picture
                        ,hBoardBack   :: Picture
                        }
data GameBoard =
    GameBoard { sBoard   :: Board
              , hBoard   :: Board
              }

-- --------------------------------------------------- -- 

-- Functions:

-- -- Backgroud

-- -- -- Make tha picture of the backgroung
createBackground :: Board -> Picture
createBackground o  = translate (translateXPos o) (translateYPos o) fig
    where
        width  = gridWidth (sizeGrid o)
        height = gridHeight (sizeGrid o)
        fig    = color (boardColor o) (rectangleSolid width height)

-- -- Random Number

-- -- -- Generator for a  RandomNumber
getRandomNumber :: StdGen -> Int -> Int
getRandomNumber gen limit = x
    where
        (x, g') = randomR (0, limit -1) gen

getRandomNumber' :: StdGen -> Float -> Float
getRandomNumber' gen limit = x
    where
        (x, g') = randomR (0, limit -1) gen

-- -- -- New seed
makeNewSeed :: StdGen -> Float -> StdGen
makeNewSeed gen limit = g'
    where
        (x, g') = randomR (0, limit -1) gen

-- --------------------------------------------------- -- 

-- Mapping Char

-- -- Char seletion
selectCharater :: Char -> Char
selectCharater char 
    | char == 'i' = 'a'
    | char == 'e' = 's'
    | char == 'a' = 'd'
    | char == 'o' = 'f'
    | char == 'u' = 'g'
    | char == 'm' = 'h'
    | char == 'd' = 'j'
    | char == 's' = 'k'
    | char == 'r' = 'l'
    | char == 'y' = 'z'
    | char == 'j' = 'c'
    | char == 'b' = 'v'
    | char == 'k' = 'b'
    | char == 'q' = 'n'
    | char == 'v' = 'm'
    | char == '/' = 'q'
    | char == ',' = 'w'
    | char == '.' = 'e'
    | char == 'h' = 'r'
    | char == 'x' = 't'
    | char == 'w' = 'y'
    | char == 'l' = 'u'
    | char == 't' = 'i'
    | char == 'c' = 'o'
    | char == 'p' = 'p'
    | otherwise = '#'

-- --------------------------------------------------- -- 

