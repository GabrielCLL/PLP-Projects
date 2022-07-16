module State 
        ( State (..)
        , initialState
        , setSnake
        , setHangman
        , setDirection
        , setFood
        , setPaused
        , setGameOver
        , setGameWin
        , setSeed
        ) where

import Snake
import Hangman
import Graphics.Gloss.Interface.Pure.Game
import System.Exit
import Util
import Boards
import Food
import Data.Maybe
import System.Random (StdGen)


data State =
     State{ getSnake     :: Snake
          , getDirection :: Directions  
          , getHangman   :: Hangman
          , getFood      :: Maybe Food
          , getPaused    :: Bool
          , getBoard     :: [Board]
          , getOver      :: Bool
          , getWin       :: Bool
          , seed         :: StdGen
          }

-- Initial State of Game
-- Snake part -- TODO Hangman part
initialState :: StdGen -> Hangman -> State 
initialState gen hangman = State initialSnake RIGHT hangman Nothing False [sBoard gameBoard, hBoard gameBoard] False False gen

{- 
initialState :: Maybe Food -> State
initialState food = State initialSnake RIGHT food False [sBoard gameBoard, hBoard gameBoard] False
-}

setSnake :: Snake -> State -> State
setSnake snake state = state {getSnake = snake}

setHangman :: Hangman -> State -> State
setHangman hangman state = state {getHangman = hangman}

setFood :: Maybe Food -> State -> State
setFood food state = state {getFood = food}

setDirection :: Directions -> State -> State
setDirection dir state = state {getDirection = dir}

setPaused :: Bool -> State -> State
setPaused paused state = state {getPaused = paused}

setGameOver :: Bool -> State -> State
setGameOver over state = state {getOver = over}

setGameWin :: Bool -> State -> State
setGameWin win state = state {getWin = win}

setSeed :: StdGen -> State -> State
setSeed newSeed state = state {seed = newSeed}

