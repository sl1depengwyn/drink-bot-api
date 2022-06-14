{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Data.Aeson.Extended as A
import Data.Aeson.TH
import Data.ByteString as BS
import Data.ByteString.Lazy (fromStrict)
import Data.ByteString.Lazy as BSL
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
import Servant
import Servant.API.ContentTypes
import Control.Monad
import Control.Monad.IO.Class (liftIO)

data Config = Config {cPort :: Int, cToken :: Text} deriving (Generic)

instance FromJSON Config where
  parseJSON = A.genericParseJSON A.customOptions

data Handle = Handler {hConfig :: Config, hDatabase :: DB.Handle, hLogger :: Logger.Handle}

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

app :: Application
app = serve (Proxy @ServerApi) server

server :: Server ServerApi
server = handleUsers :<|> handleEdit :<|> handleAdd :<|> handleTodayTotal :<|> handleTodayStats :<|> handleMonthStats

handleUsers :: User -> Handler User
handleUsers = return

handleEdit :: (Maybe Int -> Maybe Int -> Maybe Int -> Handler Record)
handleEdit uId mId amount = return (Record (UserId 1) 1 1 (mkUTCTime (1, 1, 1) (1, 1, 1)))

handleAdd :: (Record -> Handler Record)
handleAdd = return

handleTodayTotal :: (Maybe Int -> Handler Int)
handleTodayTotal uId = return 123

handleTodayStats :: (Maybe Int -> Handler WithCT)
handleTodayStats _ = do
  file <- liftIO $ BSL.readFile "test.png"
  return $ WithCT "image/png" (toStrict file)

handleMonthStats :: (Maybe Int -> Handler WithCT)
handleMonthStats = undefined

users :: [User]
users =
  [ User 1 (Just 2),
    User 2 Nothing
  ]

startApp :: IO ()
startApp = run 8080 app

startApp' :: Handle -> IO ()
startApp' h = run 8080 app
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