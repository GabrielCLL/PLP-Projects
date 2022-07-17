module Draw where

import Snake
import Graphics.Gloss
import State
import Boards
import Food
import Data.Maybe
import Util
import Hangman


snakePicture :: Picture
snakePicture = drawSnake viewSnake initialSnake

{- 
    Objetives: 
                - Move the begin of the center of grid to the Upper Left 
                - Translate to -60 (position of grid of snake game in the window)
-}
translator :: View -> Coordinate -> Picture -> Picture
translator view (x,y) = Translate x1 y1
  where
    grid    = gridView view
    gw      = gridWidth grid
    gh      = gridHeight grid
    pixel   = pixelView view
    pw      = pixelWidth pixel
    ph      = pixelHeight pixel
    x1      =  x*pw - gw / 2 + pw / 2
    y1      = gh / 2  - ph / 2 - (y * ph) -60


drawRect :: View -> Coordinate -> Picture
drawRect  view (x, y) = translator view (x,y) (rectangleSolid width height)
  where
    pixel  = pixelView view
    width  = pixelWidth pixel
    height = pixelHeight pixel

drawSnake :: View -> Snake -> Picture
drawSnake view snake = Pictures (h : t)
  where
    headColor = head (colorList view)
    bodyColor = last (colorList view)
    h         = Color headColor $ drawRect view (snakeHeadPos snake)
    t         = map (Color bodyColor . drawRect view) (snakeBodyPos snake)

drawFood :: View -> Maybe Food -> Picture
drawFood view food = if isJust food
                     then Pictures [fig]
                     else Pictures [blank]
  where
    colorFood = head (colorList view)
    fig =  Color colorFood $ drawRect view (foodPosition (fromJust food))

drawState :: State -> Picture
drawState state
  | getOver state = Pictures [background, sP, hangman, fP, drawGameOver, score]
  | getWin state = Pictures [background, sP, hangman, fP, drawGameWin, score]
  | not (control state) = Pictures [background, sP, hangman, fP, drawEnterLetter,score]
  | otherwise = Pictures [background, sP, hangman, fP, score]
  where
      s = getSnake state
      f = getFood state
      background = drawForCobrinha
      sP = drawSnake viewSnake s
      fP = drawFood viewFood  f
      score = drawScore state
      hangman = translate (-200) (150) $ renderHangman (getHangman state)


drawEnterLetter :: Picture
drawEnterLetter = pictures
        [color black  (translate (-135) (92) (scale 0.2 0.2 (text "Please enter a letter")))]

drawScore :: State -> Picture
drawScore state = Pictures [fig1, fig2]
  where
    fig1 = color black  (translate (240) (180) (scale 0.2 0.2 (text "Score:")))
    fig2 = color black  (translate (240) (140) (scale 0.2 0.2 (text (show (score state)))))

-- draw the background of the game
drawForCobrinha :: Picture
drawForCobrinha = pictures [sBoardBack gameBoardBackground, hBoardBack gameBoardBackground]

-- Draw the Win e Lose 

-- Game Over
drawGameOver :: Picture
drawGameOver = pictures
        [color red  (translate (-110) (-0)(scale 0.3 0.3 (text "Game Over")))]

-- Game Win
drawGameWin :: Picture
drawGameWin = pictures
        [color black  (translate (-110) (-0)(scale 0.3 0.3 (text "Game Win")))]
        