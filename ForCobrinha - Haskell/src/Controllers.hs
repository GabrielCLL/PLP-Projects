{-# LANGUAGE PatternSynonyms #-}

module Controllers where

import Graphics.Gloss.Interface.Pure.Game
import Data.Maybe
import State
import Util
import System.Exit
import Snake
import Data.Char
import Hangman


-- Pattern for Key Event
pattern KeyHeld k <- EventKey k Down _ _ -- x y
pattern SKeyHeld k <- KeyHeld (SpecialKey k)
pattern CKeyHeld k <- KeyHeld (Char k)

-- Glossary Diretions for a given key
keyToDiretion :: Key -> Maybe Directions
keyToDiretion (SpecialKey KeyUp)  = Just UP
keyToDiretion (SpecialKey KeyLeft) = Just LEFT
keyToDiretion (SpecialKey KeyDown) = Just DOWN
keyToDiretion (SpecialKey KeyRight) = Just RIGHT
keyToDiretion _ = Nothing

-- Glossary Decision for a given key
keyToDecision :: Key -> Maybe Decisions
keyToDecision (SpecialKey KeyEsc) = Just EXIT
keyToDecision (SpecialKey KeySpace) = Just PAUSE
keyToDecision (SpecialKey KeyEnter) = Just ACCEPT
keyToDecision _ = Nothing

-- Handle the pressing key
handleKey :: Event -> State -> State
handleKey (CKeyHeld k) state = verifyHangmanDecision k state 
handleKey (KeyHeld k) state
    -- | k == SpecialKey KeyEsc = exitSuccess
    | isNothing (keyToDiretion k) =  maybe id verifyDecision (keyToDecision k) state
    | otherwise = maybe id verifyDirSnake (keyToDiretion k) state
handleKey _ state = state

verifyDirSnake :: Directions -> State -> State
verifyDirSnake diretion state = if direction (getSnake  state) == diretion
                                then state
                                else setDirection diretion state


verifyDecision :: Decisions -> State -> State
verifyDecision decision state
    | decision == PAUSE = setPaused (not $ getPaused state) state
    | otherwise = state -- provisório   


verifyHangmanDecision :: Char -> State -> State
verifyHangmanDecision letterKicked state = 
    setHangman newHangman state
    where newHangman = hangmanInput letterKicked (getHangman state)
  
