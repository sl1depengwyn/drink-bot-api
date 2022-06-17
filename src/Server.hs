{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
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
import Lens.Micro
import qualified Logger
import Network.Wai
import Network.Wai.Handler.Warp
import qualified Plotter.Plotter as Plotter
import Servant
import Servant.API.ContentTypes

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
  "addUser" :> ReqBody '[JSON] User :> Post '[JSON] NoContent
    :<|> "editRecord" :> QueryParam "userId" Int :> QueryParam "messageId" Int :> QueryParam "amount" Int :> Put '[JSON] Bool
    :<|> "addRecord" :> ReqBody '[JSON] Record :> Post '[JSON] Bool
    :<|> "getTodayTotal" :> QueryParam "userId" Int :> Get '[JSON] (Maybe Int)
    :<|> "getTodayStats" :> QueryParam "userId" Int :> Get '[IMAGE] WithCT
    :<|> "getMonthStats" :> QueryParam "userId" Int :> Get '[IMAGE] WithCT

askLogger :: AppM Logger.Handle
askLogger = asks hLogger

askDbh :: AppM DB.Handle
askDbh = asks hDatabase

askPlotter :: AppM Plotter.Handle
askPlotter = asks hPlotter

handleAddUsers :: User -> AppM NoContent
handleAddUsers u = do
  dbh <- askDbh
  liftIO (DB.addUser dbh (u ^. userId))
  pure NoContent

handleEdit :: Maybe Int -> Maybe Int -> Maybe Int -> AppM Bool
handleEdit uId mId amount = do
  dbh <- askDbh
  case (uId, mId, amount) of
    (Just uId', Just mId', Just amount') -> do
      liftIO (DB.updateRecord dbh uId' mId' amount')
      pure True
    (_, _, _) -> pure False

handleAdd :: Record -> AppM Bool
handleAdd r = do
  dbh <- askDbh
  liftIO (DB.addRecord dbh (r ^. recordUId) (r ^. recordMId) (r ^. recordAmount) (r ^. recordTStamp))
  pure True

handleTodayTotal :: Maybe Int -> AppM (Maybe Int)
handleTodayTotal uId = do
  case uId of
    (Just uId') -> do
      dbh <- askDbh
      amount <- liftIO (DB.getSumTodaysAmount dbh uId')
      case amount of
        (Just v) -> pure v
        _ -> pure Nothing
    _ -> pure Nothing

handleTodayStats :: Maybe Int -> AppM WithCT
handleTodayStats uId = do
  case uId of
    (Just uId') -> do
      dbh <- askDbh
      plotterh <- askPlotter
      stats <- liftIO (DB.getTodaysRecordsStats dbh uId')
      image <- liftIO (Plotter.plotDayStats plotterh stats)
      return (WithCT "image/png" image)
    _ -> pure (WithCT "" "")

handleMonthStats :: Maybe Int -> AppM WithCT
handleMonthStats uId = do
  case uId of
    (Just uId') -> do
      dbh <- askDbh
      plotterh <- askPlotter
      stats <- liftIO (DB.getMonthRecordsStats dbh uId')
      image <- liftIO (Plotter.plotMonthStats plotterh stats)
      return (WithCT "image/png" image)
    _ -> pure (WithCT "" "")

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