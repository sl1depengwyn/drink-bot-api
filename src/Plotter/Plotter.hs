module Plotter.Plotter where

import qualified Data.Aeson.Extended as A
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Colour
import Data.Default.Class
import Data.Time
import qualified GHC.Generics as G
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Easy
import System.Directory.Extended
import System.FilePath

newtype Config = Config {cTempStore :: FilePath} deriving (Show, G.Generic)

instance A.FromJSON Config where
  parseJSON = A.genericParseJSON A.customOptions

newtype Handle = Handle {hConfig :: Config}

withHandle :: Config -> (Handle -> IO ()) -> IO ()
withHandle conf f = f $ Handle {hConfig = conf}

mkEC :: (PlotValue x, BarsPlotValue y) => String -> [(x, y)] -> EC (Layout x y) ()
mkEC xAxisName records = do
  layout_y_axis . laxis_title .= "Water amount"
  layout_x_axis . laxis_title .= xAxisName
  plot (line "" [records])
  plot (points "" records)
  plot $ plotBars <$> bars [""] (map (\(x, y) -> (x, [y])) records)

mkPath :: FilePath -> UTCTime -> FilePath
mkPath path time = path </> (formatTime defaultTimeLocale "%H%M%S" time <> ".png")

plotStats :: (PlotValue x, BarsPlotValue y) => Handle -> String -> [(x, y)] -> IO ByteString
plotStats h xAxisName records = do
  let tpath = cTempStore (hConfig h)
  path <- mkPath tpath <$> getCurrentTime
  toFile def path (mkEC xAxisName records)
  image <- BS.readFile path
  removeIfExists path
  pure image

plotDayStats :: (BarsPlotValue y, PlotValue x) => Handle -> [(x, y)] -> IO ByteString
plotDayStats h = plotStats h "Time (Hour)"

plotMonthStats :: (BarsPlotValue y, PlotValue x) => Handle -> [(x, y)] -> IO ByteString
plotMonthStats h = plotStats h "Time (Day)"
