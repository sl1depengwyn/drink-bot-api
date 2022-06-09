module Logger where

import qualified Data.Aeson.Extended as A
import           Data.Char           (toUpper)
import           Data.Maybe          (fromMaybe)
import qualified Data.Text           as T
import           Data.Time.Extended
import qualified GHC.Generics        as G
import           Prelude             hiding (error, log)

data Verbosity
  = Debug
  | Info
  | Warning
  | Error
  deriving (Eq, Ord)

instance Show Verbosity where
  show Debug   = "DEBUG"
  show Info    = "INFO"
  show Warning = "WARNING"
  show Error   = "ERROR"

instance A.FromJSON Verbosity where
  parseJSON =
    A.withText "FromJSON Logger.Verbosity" $ \t ->
      case t of
        "debug"   -> pure Debug
        "info"    -> pure Info
        "warning" -> pure Warning
        "error"   -> pure Error
        _         -> fail $ "Unknown verbosity: " ++ T.unpack t

data Config =
  Config
    { cPath      :: Maybe String
    , cVerbosity :: Maybe Verbosity
    }
  deriving (Show, G.Generic)

instance A.FromJSON Config where
  parseJSON = A.genericParseJSON A.customOptions

newtype Handle =
  Handle
    { hConfig :: Config
    }

withHandle :: Config -> (Handle -> IO a) -> IO a
withHandle config f = f $ Handle {hConfig = config}

pushLogStrLn :: Maybe FilePath -> Verbosity -> String -> IO ()
pushLogStrLn (Just path) v logMsg =
  nowFormatted >>= (\time -> appendFile path (mconcat ["[", time, "] ", "[", show v, "]", ": ", logMsg, "\n"]))
pushLogStrLn Nothing v logMsg = nowFormatted >>= (\time -> putStrLn $ mconcat ["[", time, "] ", "[", show v, "]", ": ", logMsg])

log :: Handle -> Verbosity -> String -> IO ()
log (Handle config) v x
  | v >= verbosity = pushLogStrLn mbPath v x
  | otherwise = return ()
  where
    verbosity = fromMaybe Debug (cVerbosity config)
    mbPath = cPath config

debug, info, warning, error :: Handle -> String -> IO ()
debug h = log h Debug

info h = log h Info

warning h = log h Warning

error h = log h Error
