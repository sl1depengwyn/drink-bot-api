module Server.Main where

import qualified Data.Aeson.Extended as A
import qualified Data.Yaml as Yaml
import qualified Database.Database as Database
import qualified GHC.Generics as G
import qualified Logger
import qualified Plotter.Plotter as Plotter
import qualified Server
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)

-- run = undefined

-- startApp :: IO ()
-- startApp = run 8080 app

data Config = Config
  { cDatabase :: Database.Config,
    cServer :: Server.Config,
    cLogger :: Logger.Config,
    cPlotter :: Plotter.Config
  }
  deriving (Show, G.Generic)

instance A.FromJSON Config where
  parseJSON = A.genericParseJSON A.customOptions

main :: IO ()
main = do
  args <- getArgs
  case args of
    [configPath] -> run configPath
    _ -> run "config.yaml"

run :: FilePath -> IO ()
run path = do
  errOrConfig <- Yaml.decodeFileEither path
  conf <- either (fail . show) pure errOrConfig
  let db = cDatabase conf
  let server = cServer conf
  let logger = cLogger conf
  let plotter = cPlotter conf
  Logger.withHandle
    logger
    ( \hLogger ->
        Plotter.withHandle
          plotter
          ( \hPlotter ->
              Database.withHandle
                db
                hLogger
                ( \hDb -> Server.withHandle server hDb hPlotter hLogger Server.startApp
                )
          )
    )
