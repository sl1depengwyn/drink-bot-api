{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.Extended as A
import Data.Aeson.TH
import Data.ByteString as BS
import Data.ByteString.Lazy (fromStrict)
import qualified Data.ByteString.Lazy as BSL
import Data.Data (Typeable)
import Data.Fixed
import Data.Functor.Identity (Identity)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Extended
import qualified Database.Database as DB
import Database.Schema.V001
import GHC.Generics (Generic)
import qualified Logger
import Network.Wai
import Network.Wai.Handler.Warp
import qualified Plotter.Plotter as Plotter
import Servant
import Servant.API.ContentTypes
import Control.Monad.Reader



data Config = Config {cPort :: Int, cToken :: Text} deriving (Show, Generic)

instance FromJSON Config where
  parseJSON = A.genericParseJSON A.customOptions

data Handle = Handle
  { hConfig :: Config,
    hDatabase :: DB.Handle,
    hPlotter :: Plotter.Handle,
    hLogger :: Logger.Handle
  }

type AppM = ReaderT Handle Handler

withHandle ::
  Config ->
  DB.Handle ->
  Plotter.Handle ->
  Logger.Handle ->
  (Handle -> IO ()) ->
  IO ()
withHandle c db plot logger f = f Handle {hConfig = c, hDatabase = db, hPlotter = plot, hLogger = logger}

data WithCT = WithCT {header :: BS.ByteString, content :: BS.ByteString}

instance AllCTRender '[IMAGE] WithCT where
  handleAcceptH _ _ (WithCT h c) = Just (fromStrict h, fromStrict c)

data IMAGE deriving (Typeable)

instance MimeRender IMAGE BS.ByteString where
  mimeRender _ content = fromStrict content

instance Accept IMAGE where
  contentType _ = ""

type ImageApi = Capture "image_id" Int :> Get '[IMAGE] WithCT

type API = "users" :> Get '[JSON] [User]

type API2 = "delete" :> DeleteNoContent

type ServerApi =
  "addUser" :> ReqBody '[JSON] User :> Post '[JSON] User
    :<|> "editRecord" :> QueryParam "userId" Int :> QueryParam "messageId" Int :> QueryParam "amount" Int :> Put '[JSON] Record
    :<|> "addRecord" :> ReqBody '[JSON] Record :> Post '[JSON] Record
    :<|> "getTodayTotal" :> QueryParam "userId" Int :> Get '[JSON] Int
    :<|> "getTodayStats" :> QueryParam "userId" Int :> Get '[IMAGE] WithCT
    :<|> "getMonthStats" :> QueryParam "userId" Int :> Get '[IMAGE] WithCT


--handleAddUsers :: User -> ServerT ServerApi AppM
handleAddUsers :: User -> AppM User
handleAddUsers = return
  

handleEdit :: Maybe Int -> Maybe Int -> Maybe Int -> AppM Record
handleEdit uId mId amount = return (Record (UserId 1) 1 1 (mkUTCTime (1, 1, 1) (1, 1, 1)))

handleAdd :: Record -> AppM Record
handleAdd = return

handleTodayTotal :: Maybe Int -> AppM Int
handleTodayTotal uId = return 123

handleTodayStats :: Maybe Int -> AppM WithCT
handleTodayStats _ = do
  file <- liftIO $ BSL.readFile "test.png"
  return $ WithCT "image/png" (BSL.toStrict file)

handleMonthStats :: Maybe Int -> AppM WithCT
handleMonthStats = undefined

api :: Proxy ServerApi
api = Proxy

app :: Handle -> Application
app h = serve api (hoistServer api (`runReaderT` h) server)

server :: ServerT ServerApi AppM
server = handleAddUsers :<|> handleEdit :<|> handleAdd :<|> handleTodayTotal :<|> handleTodayStats :<|> handleMonthStats

startApp :: Handle -> IO ()
startApp h = run port (app h)
  where
    port = cPort (hConfig h)

instance ToJSON User where
  toJSON = A.genericToJSON A.customOptions

instance ToJSON (PrimaryKey UserT Identity) where
  toJSON = A.genericToJSON A.customOptions

instance ToJSON (PrimaryKey RecordT Identity) where
  toJSON = A.genericToJSON A.customOptions

instance ToJSON Record where
  toJSON = A.genericToJSON A.customOptions

instance FromJSON User where
  parseJSON = A.genericParseJSON A.customOptions

instance FromJSON (PrimaryKey UserT Identity) where
  parseJSON = A.genericParseJSON A.customOptions

instance FromJSON (PrimaryKey RecordT Identity) where
  parseJSON = A.genericParseJSON A.customOptions

instance FromJSON Record where
  parseJSON = A.genericParseJSON A.customOptions