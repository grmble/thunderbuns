{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Thunderbuns.Persist where

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Thunderbuns.Tlude
import Thunderbuns.Persist.Types
import Data.ByteString.D64.UUID
import Data.Coerce (coerce)
import Data.Maybe (fromJust)
import Data.UUID.V1 (nextUUID)
import UnliftIO (liftIO)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Message
    uuid OrderedUUID
    UniqueUUID uuid
    nick String
    cmd String
    channel String
    msg String
|]

{--
   Because of IRC's Scandinavian origin, the characters {}|^ are
   considered to be the lower case equivalents of the characters []\~,
   respectively. This is a critical issue when determining the
   equivalence of two nicknames or channel names.
--}

blubb :: IO ()
blubb = runSqlite ":memory" $ do
  runMigration migrateAll
  uuid <- orderedUUID . fromJust <$> liftIO nextUUID
  let msg = Message uuid "nick" "cmd" "#haskell" "it's a test"
  _ <- insert msg
  pure ()
