module Food
       (Food(..)
       , viewFood
       , coordinateFood --initialFood--generateFood
       , moveFood
       , initialFood
       ) where



import System.Random
import Util
import Graphics.Gloss

-- --------------------------------------------------- -- 

-- Food
newtype Food = Food {foodPosition :: Coordinate}

-- A food
initialFood = Food (0,0)

-- --------------------------------------------------- -- 

-- A view for the food
viewFood =
    View { pixelView = pixelFood
         , gridView  = sizeGridSnake
         , colorList = [red]
         }

-- --------------------------------------------------- -- 

-- Functions

-- -- Move
moveFood :: Food -> Food
moveFood food = food {foodPosition = coordinate}
       where
              coordinate = foodPosition food

-- -- Calculate the coodinates for the food
coordinateFood :: StdGen -> (Coordinate, StdGen)
coordinateFood gen  = (coordinate, gen2)
       where
              view = viewFood
              grid       = gridView view
              pixel      = pixelView view
              withd      = (gridWidth grid / pixelWidth pixel) - 1 
              height     = (gridHeight grid / pixelHeight pixel) - 1 
              x          = getRandomNumber' gen withd
              gen1       = makeNewSeed gen withd
              y          = getRandomNumber' gen1 height
              gen2       = makeNewSeed gen1 height
       
              coordinate = (fromIntegral (round x), fromIntegral (round y))

-- --------------------------------------------------- -- 