module Plotter.Plotter where

import qualified Data.Aeson.Extended as A
import Data.Colour
import Data.Default.Class
import Data.Time
import qualified GHC.Generics as G
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Easy
import System.FilePath

newtype Config = Config {cTempStore :: FilePath} deriving (Show, G.Generic)

instance A.FromJSON Config where
  parseJSON = A.genericParseJSON A.customOptions

newtype Handle = Handle {hConfig :: Config}

withHandle :: Config -> (Handle -> IO ()) -> IO ()
withHandle conf f = f $ Handle {hConfig = conf}

plotStats :: (PlotValue x, BarsPlotValue y) => Handle -> String -> [(x, y)] -> FilePath -> IO ()
plotStats h axisName records fileName = toFile def path $ do
  layout_y_axis . laxis_title .= "Water amount"
  layout_x_axis . laxis_title .= axisName
  plot (line "" [records])
  plot (points "" records)
  plot $ plotBars <$> bars [""] (map (\(x, y) -> (x, [y])) records)
  where
    path = (cTempStore . hConfig) h </> fileName

plotDayStats :: (BarsPlotValue y, PlotValue x) => Handle -> [(x, y)] -> FilePath -> IO ()
plotDayStats h = plotStats h "Time (Hour)"

plotMonthStats :: (BarsPlotValue y, PlotValue x) => Handle -> [(x, y)] -> FilePath -> IO ()
plotMonthStats h = plotStats h "Time (Day)"