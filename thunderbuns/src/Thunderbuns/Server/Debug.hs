{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Thunderbuns.Server.Debug where

import Control.Lens (view)
import Data.Foldable (for_)
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import qualified Data.Text as T
import Servant
import Thunderbuns.Auth.Types
import Thunderbuns.Logging
import UnliftIO.STM

type LogLevels = M.HashMap T.Text (Maybe Priority)

type DebugAPI
   -- list log levels
   = Get '[ JSON] LogLevels
      -- modify log levels
      :<|> ReqBody '[ JSON] LogLevels :> PostNoContent '[ JSON] NoContent

-- XXX ideally we wouild be streaming ...
-- but i can't figure out how to get hold of a ToStreamGenerator
-- import Servant.API.Stream
-- type DebugAPI = StreamGet NewlineFraming JSON [T.Text] -- list debug levels
debugAPI :: Proxy DebugAPI
debugAPI = Proxy

debugServer ::
     (HasLogger r) => r -> Claims -> Server DebugAPI
debugServer r _ = listLogLevels :<|> modifyLogLevels
  where
    listLogLevels :: Handler LogLevels
    listLogLevels = do
      let namesTV = view (loggerL . loggerNamesL) r
      names <- readTVarIO namesTV
      let priMapTV = view (loggerL . priorityMapL) r
      priMap <- readTVarIO priMapTV
      pure $ foldr (\n -> M.insert n (M.lookup n priMap)) M.empty names
    modifyLogLevels :: LogLevels -> Handler NoContent
    modifyLogLevels lls = do
      let namesTV = view (loggerL . loggerNamesL) r
      let priMapTV = view (loggerL . priorityMapL) r
      atomically $
        for_ (M.toList lls) $ \(n, mpri) -> do
          modifyTVar namesTV (S.insert n)
          modifyTVar priMapTV (M.alter (const mpri) n)
      pure NoContent
