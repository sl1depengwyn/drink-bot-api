module Database.Migration
  ( migration,
    allowDestructive,
    module Database.Schema.V001,
  )
where

import Data.Time (UTCTime)
import Database.Beam
import Database.Beam.Backend
import Database.Beam.Migrate
import Database.Beam.Migrate.Simple
import Database.Beam.Postgres
import Database.Schema.V001 hiding (migration)
import qualified Database.Schema.V001 as V001 (migration)

migration :: MigrationSteps Postgres () (CheckedDatabaseSettings Postgres DrinkDb)
migration = migrationStep "V001" (const V001.migration)

allowDestructive :: (Monad m, MonadFail m) => BringUpToDateHooks m
allowDestructive =
  defaultUpToDateHooks
    { runIrreversibleHook = pure True
    }
