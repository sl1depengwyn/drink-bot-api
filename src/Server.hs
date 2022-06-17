{-# LANGUAGE DataKinds #-}
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
import qualified Data.Text.Encoding as Text
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
    :<|> "addButton" :> ReqBody '[JSON] Button :> Post '[JSON] Bool
    :<|> "removeButton" :> ReqBody '[JSON] Button :> Delete '[JSON] NoContent
    :<|> "getButtons" :> QueryParam "userId" Int :> Get '[JSON] [Int]

type ProtectedApi = BasicAuth "Dring Enough API" () :> ServerApi

checkBasicAuth :: Handle -> BasicAuthCheck ()
checkBasicAuth h = BasicAuthCheck $ \basicAuthData ->
  let password = Text.decodeUtf8 (basicAuthPassword basicAuthData)
   in if password == cToken (hConfig h)
        then pure (Authorized ())
        else pure BadPassword

askLogger :: AppM Logger.Handle
askLogger = asks hLogger

askDbh :: AppM DB.Handle
askDbh = asks hDatabase

askPlotter :: AppM Plotter.Handle
askPlotter = asks hPlotter

handleAddUser :: User -> AppM NoContent
handleAddUser u = do
  dbh <- askDbh
  liftIO (DB.addUser dbh (u ^. userId))
  pure NoContent

handleEditRecord :: Maybe Int -> Maybe Int -> Maybe Int -> AppM Bool
handleEditRecord uId mId amount = do
  dbh <- askDbh
  case (uId, mId, amount) of
    (Just uId', Just mId', Just amount') -> do
      liftIO (DB.updateRecord dbh uId' mId' amount')
      pure True
    (_, _, _) -> pure False

handleAddRecord :: Record -> AppM Bool
handleAddRecord r = do
  dbh <- askDbh
  liftIO (DB.addRecord dbh (r ^. recordUId) (r ^. recordMId) (r ^. recordAmount) (r ^. recordTStamp))
  pure True

handleGetTodayTotal :: Maybe Int -> AppM (Maybe Int)
handleGetTodayTotal uId = do
  case uId of
    (Just uId') -> do
      dbh <- askDbh
      amount <- liftIO (DB.getSumTodaysAmount dbh uId')
      case amount of
        (Just v) -> pure v
        _ -> pure Nothing
    _ -> pure Nothing

handleGetTodayStats :: Maybe Int -> AppM WithCT
handleGetTodayStats uId = do
  case uId of
    (Just uId') -> do
      dbh <- askDbh
      plotterh <- askPlotter
      stats <- liftIO (DB.getTodaysRecordsStats dbh uId')
      image <- liftIO (Plotter.plotDayStats plotterh stats)
      return (WithCT "image/png" image)
    _ -> pure (WithCT "" "")

handleGetMonthStats :: Maybe Int -> AppM WithCT
handleGetMonthStats uId =
  case uId of
    (Just uId') -> do
      dbh <- askDbh
      plotterh <- askPlotter
      stats <- liftIO (DB.getMonthRecordsStats dbh uId')
      image <- liftIO (Plotter.plotMonthStats plotterh stats)
      return (WithCT "image/png" image)
    _ -> pure (WithCT "" "")

handleAddButton :: Button -> AppM Bool
handleAddButton button = do
  dbh <- askDbh
  liftIO (DB.addButton dbh (button ^. buttonUId) (button ^. buttonAmount))
  pure True

handleRemoveButton :: Button -> AppM NoContent
handleRemoveButton button = do
  dbh <- askDbh
  liftIO (DB.removeButton dbh (button ^. buttonUId) (button ^. buttonAmount))
  pure NoContent

handleGetButtons :: Maybe Int -> AppM [Int]
handleGetButtons uId =
  case uId of
    (Just uId') -> do
      dbh <- askDbh
      liftIO (DB.getButtons dbh uId')
    _ -> pure []

api :: Proxy ProtectedApi
api = Proxy

app :: Handle -> Application
app h = serveWithContext api ctx (hoistServerWithContext api ctx' (`runReaderT` h) server)
  where
    ctx' :: Proxy '[BasicAuthCheck ()]
    ctx' = Proxy

    ctx = checkBasicAuth h :. EmptyContext

server :: ServerT ProtectedApi AppM
server _ =
  handleAddUser
    :<|> handleEditRecord
    :<|> handleAddRecord
    :<|> handleGetTodayTotal
    :<|> handleGetTodayStats
    :<|> handleGetMonthStats
    :<|> handleAddButton
    :<|> handleRemoveButton
    :<|> handleGetButtons

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

instance ToJSON Button where
  toJSON = A.genericToJSON A.customOptions

instance FromJSON User where
  parseJSON = A.genericParseJSON A.customOptions

instance FromJSON (PrimaryKey UserT Identity) where
  parseJSON = A.genericParseJSON A.customOptions

instance FromJSON (PrimaryKey RecordT Identity) where
  parseJSON = A.genericParseJSON A.customOptions

instance FromJSON Record where
  parseJSON = A.genericParseJSON A.customOptions

instance FromJSON Button where
  parseJSON = A.genericParseJSON A.customOptions