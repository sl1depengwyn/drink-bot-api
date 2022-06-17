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
import qualified Graphics.Rendering.Cairo as C
import Data.ByteString (ByteString)

newtype Config = Config {cTempStore :: FilePath} deriving (Show, G.Generic)

instance A.FromJSON Config where
  parseJSON = A.genericParseJSON A.customOptions

newtype Handle = Handle {hConfig :: Config}

withHandle :: Config -> (Handle -> IO ()) -> IO ()
withHandle conf f = f $ Handle {hConfig = conf}

plotStats :: (PlotValue x, BarsPlotValue y) => Handle -> String -> [(x, y)] -> IO ByteString
plotStats h xAxisName records = getPNG def $ do
  layout_y_axis . laxis_title .= "Water amount"
  layout_x_axis . laxis_title .= xAxisName
  plot (line "" [records])
  plot (points "" records)
  plot $ plotBars <$> bars [""] (map (\(x, y) -> (x, [y])) records)

plotDayStats :: (BarsPlotValue y, PlotValue x) => Handle -> [(x, y)] -> IO ByteString
plotDayStats h = plotStats h "Time (Hour)"

plotMonthStats :: (BarsPlotValue y, PlotValue x) => Handle -> [(x, y)] -> IO ByteString
plotMonthStats h = plotStats h "Time (Day)"

getPNG :: (PlotValue x, BarsPlotValue y) => FileOptions -> EC (Layout x y) () -> IO ByteString
getPNG fo ec = C.withImageSurface C.FormatARGB32 width height (\result -> do
      pf <- C.renderWith result $ runBackend (defaultEnv bitmapAlignmentFns) cr
      C.imageSurfaceGetData result)
  where
    (width,height) = _fo_size fo
    cr = render r (fromIntegral width, fromIntegral height)
    r = toRenderable (execEC ec)