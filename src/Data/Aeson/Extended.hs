module Data.Aeson.Extended
  ( module Data.Aeson,
    module Data.Aeson.Extra,
    module Data.Aeson.Types,
    customOptions,
  )
where

import Data.Aeson.Extra
import Data.Aeson
import Data.Aeson.Types
import qualified Database.PostgreSQL.Simple.Newtypes as Data

customOptions :: Options
customOptions = defaultOptions {fieldLabelModifier = camelTo2 '_' . tail, sumEncoding = UntaggedValue}
