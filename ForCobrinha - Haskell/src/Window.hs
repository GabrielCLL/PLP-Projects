module Window where

import Graphics.Gloss
import Data.Maybe
import Util



-- Config of Window
data ConfigWindow  =
    ConfigWindow  {windowWidth :: Int,
                  windowHeight :: Int,
                  offset :: (Int, Int),
                  configFullScreen :: Bool,
                  windowColor :: Color
                  }

-- Config Default 720 x 480
defaultConfigWindow =
    ConfigWindow {windowWidth = 720,
                 windowHeight = 480,
                 offset = (10,10),
                 configFullScreen = True,
                 windowColor = dark white
                 }

windowSize :: ConfigWindow -> (Int,Int)
windowSize s = (windowWidth s, windowHeight s)

windowOffset :: ConfigWindow -> (Int, Int)
windowOffset = offset

windowBackgroundColor :: Maybe ConfigWindow -> Color
windowBackgroundColor (Just o) = windowColor o
windowBackgroundColor Nothing  = windowColor  defaultConfigWindow

-- WindowMaker 
displayWindow :: Display
displayWindow = InWindow nameGame (windowSize defaultConfigWindow) (windowOffset defaultConfigWindow)


