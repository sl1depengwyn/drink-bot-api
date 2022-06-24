module Plotter.Plotter where

import Codec.Picture.Png
import Codec.Picture.Types
import qualified Data.Aeson.Extended as A
import Data.Array.Base
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Colour
import Data.Default.Class
import Data.Time
import qualified Data.Vector.Storable as V
import Data.Word (Word32, Word8)
import qualified GHC.Generics as G
import qualified Graphics.Rendering.Cairo as C
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
mkPath path time = path </> (formatTime defaultTimeLocale "%H%M%S%q" time <> ".png")

plotStats :: (PlotValue x, BarsPlotValue y) => Handle -> String -> [(x, y)] -> IO LBS.ByteString
plotStats h xAxisName records = do
  let (width, height) = _fo_size def
      ec = mkEC xAxisName records
      cr = render (layoutToRenderable (execEC ec)) (fromIntegral width, fromIntegral height)
  img <-
    C.withImageSurface C.FormatARGB32 width height $ \result -> do
      C.renderWith result $ runBackend (defaultEnv bitmapAlignmentFns) cr
      stride <- C.imageSurfaceGetStride result
      surfaceData <- C.imageSurfaceGetPixels result
      withImage width height $ \x y -> do
        w <- surfaceData `readArray` (y * (stride `div` 4) + x)
        let [alpha, red, green, blue] = LBS.unpack (toLazyByteString (word32BE w))
        pure (PixelRGBA8 red green blue alpha)
  pure (encodePng img)

plotDayStats :: (BarsPlotValue y, PlotValue x) => Handle -> [(x, y)] -> IO LBS.ByteString
plotDayStats h = plotStats h "Time (Hour)"

plotMonthStats :: (BarsPlotValue y, PlotValue x) => Handle -> [(x, y)] -> IO LBS.ByteString
plotMonthStats h = plotStats h "Time (Day)"
