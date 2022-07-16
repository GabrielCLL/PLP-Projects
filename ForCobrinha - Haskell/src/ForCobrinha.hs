module ForCobrinha where

import Graphics.Gloss
import Draw
import Snake
import State
import Window
import Controllers
import System.Random
import Data.Maybe
import Hangman
import Food



-- Game Step and Functions

-- Game Step
gameStep :: Float -> State -> State
gameStep _ state
  | getPaused  state = state
  | itsEatingFood (doStep state) = eatFood $ growSnake state
 -- | itsNotValid (getSnake state) = setGameOver True state
  | checkGameWin (getHangman state) = setGameWin True state
  | otherwise = newState
  where
      doStep =   makeFood . doMoveSnake
      oldNumLetters = discoveredLettersLength (discoveredLetters (challenge (getHangman (doStep state))))
      hangman = updateHangman (getHangman (doStep state))
      newNumLetters = discoveredLettersLength (discoveredLetters (challenge hangman))

      guessed = guessedLetter oldNumLetters newNumLetters -- Retorna True se acertou a letra, False caso contrário

      newState = setHangman hangman (doStep state)


-- --------------------------------------------------- -- 

-- Functions:

guessedLetter :: Int -> Int -> Bool
guessedLetter oldNumLetters newNumLetters = newNumLetters > oldNumLetters

-- --------------------------------------------------- -- 

-- Functions:

-- Snake Functions

-- Do the move
doMoveSnake :: State -> State
doMoveSnake state = setSnake newSnake state
    where
        oldSnake = getSnake state
        diretion = getDirection state
        newSnake = move (setDirectionHead oldSnake diretion)

-- A new food here
makeFood :: State -> State
makeFood state = if isNothing(getFood state)
                then if overlapping
                     then makeFood state
                     else setFood (Just food) $ setSeed newSeed state
                else state
    where
        tuple = coordinateFood (seed state)
        newSeed = snd tuple
        food = moveFood (initialFood {foodPosition =  fst tuple})
        overlapping = overllaping (getSnake state) (foodPosition food)

eatFood :: State -> State
eatFood state = if isJust (getFood state) && eat
                then setFood Nothing state
                else state
    where
        food   = getFood state
        snake  = getSnake state
        eat    = eating snake (fromJust food)

-- Getting Bigger
growSnake :: State -> State
growSnake state = setSnake newSnake state
    where
        snake = getSnake state
        food = fromJust (getFood state)
        newSnake = growning snake food

-- Eating to Grow
itsEatingFood :: State -> Bool
itsEatingFood state
    | eating snake food        = True
    | otherwise                = False
    where
        snake = getSnake state
        food  = fromJust (getFood state)

-- --------------------------------------------------- -- 

-- Settings
fps = 10 :: Int

-- --------------------------------------------------- -- 

-- GameTest

game :: IO ()
game = do
    seed <- newStdGen
    allWords <- getWords
    let hangman = initialStateHangman seed allWords
    play displayWindow (windowBackgroundColor Nothing) fps (initialState seed hangman) drawState handleKey gameStep
