module Thunderbuns.Persist.Types where

import Database.Persist
import Database.Persist.Sqlite
import Data.ByteString.D64.UUID
import qualified Data.Text.Encoding as T


instance PersistFieldSql OrderedUUID where
  sqlType = const SqlString

instance PersistField OrderedUUID where
  toPersistValue (OrderedUUID x) = PersistByteString x
  fromPersistValue (PersistByteString x)= Right $ OrderedUUID x
  fromPersistValue (PersistText x)= Right $ OrderedUUID (T.encodeUtf8 x)
  fromPersistValue _ = Left "xxx"

