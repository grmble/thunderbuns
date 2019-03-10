{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Thunderbuns.Persist.Gen where

import Data.ByteString.D64.UUID
import Database.Persist.Sqlite
import Database.Persist.TH
import Thunderbuns.Persist.Types ()
import Thunderbuns.Tlude

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Message
    uuid OrderedUUID
    nick Text
    cmd Text
    channel Text
    msg Text

    UniqueUUID channel uuid
|]
