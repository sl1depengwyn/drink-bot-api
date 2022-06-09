{-# LANGUAGE ImpredicativeTypes #-}

module Database.Schema.V001 where

import Data.Int
import Data.Time
import Database.Beam
import Database.Beam.Backend
import Database.Beam.Migrate
import Database.Beam.Postgres
import Lens.Micro hiding (to)

data UserT f = User
  { _userId :: Columnar f Int32,
    _lastMessageId :: Columnar f (Maybe Int32)
  }
  deriving (Generic, Beamable)

type User = UserT Identity

deriving instance Show User

deriving instance Eq User

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f Int32)
    deriving (Generic, Beamable)
  primaryKey = UserId . _userId

deriving instance Show (PrimaryKey UserT Identity)

deriving instance Eq (PrimaryKey UserT Identity)

User (LensFor userId) (LensFor lastMessageId) = tableLenses

data RecordT f = Record
  { _ruserId :: PrimaryKey UserT f,
    _rmessageId :: Columnar f Int32,
    _ramount :: Columnar f Int32,
    _rtimeStamp :: Columnar f UTCTime
  }
  deriving (Generic, Beamable)

type Record = RecordT Identity

deriving instance Show Record

deriving instance Eq Record

instance Table RecordT where
  data PrimaryKey RecordT f
    = RecordId
        (PrimaryKey UserT f)
        (Columnar f Int32)
    deriving (Generic, Beamable)
  primaryKey = RecordId <$> _ruserId <*> _rmessageId

deriving instance Show (PrimaryKey RecordT Identity)

deriving instance Eq (PrimaryKey RecordT Identity)

Record
  (UserId (LensFor recordUId))
  (LensFor recordMId)
  (LensFor recordAmount)
  (LensFor recordTStamp) = tableLenses

data ButtonT f = Button
  { _buserId :: PrimaryKey UserT f,
    _bamount :: Columnar f Int32
  }
  deriving (Generic, Beamable)

type Button = ButtonT Identity

deriving instance Show Button

deriving instance Eq Button

instance Table ButtonT where
  data PrimaryKey ButtonT f
    = ButtonId
        (PrimaryKey UserT f)
        (Columnar f Int32)
    deriving (Generic, Beamable)
  primaryKey = ButtonId <$> _buserId <*> _bamount

deriving instance Show (PrimaryKey ButtonT Identity)

deriving instance Eq (PrimaryKey ButtonT Identity)

Button
  (UserId (LensFor buttonUId))
  (LensFor buttonAmount) = tableLenses

data DrinkDb f = DrinkDb
  { _dUsers :: f (TableEntity UserT),
    _dRecords :: f (TableEntity RecordT),
    _dButtons :: f (TableEntity ButtonT)
  }
  deriving (Generic, Database Postgres)

DrinkDb
  (TableLens drinkUsers)
  (TableLens drinkRecords)
  (TableLens drinkButtons) = dbLenses

utctime :: BeamSqlBackend be => DataType be UTCTime
utctime = DataType (timestampType Nothing True)

migration :: Migration Postgres (CheckedDatabaseSettings Postgres DrinkDb)
migration =
  DrinkDb
    <$> ( createTable "users" $
            User
              { _userId = field "id" int notNull unique,
                _lastMessageId = field "lastmessageid" (maybeType int)
              }
        )
    <*> ( createTable "records" $
            Record
              { _ruserId = UserId $ field "userid" int notNull,
                _rmessageId = field "id" int notNull,
                _ramount = field "amount" int notNull,
                _rtimeStamp = field "date" utctime notNull
              }
        )
    <*> ( createTable "buttons" $
            Button
              { _buserId = UserId $ field "userid" int notNull,
                _bamount = field "amount" int notNull
              }
        )
