{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Thunderbuns.Server.Debug where

import Control.Lens (view)
import Control.Monad.Reader
import Data.Foldable (for_)
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import qualified Data.Text as T
import Jose.Jwt (JwtClaims)
import Servant
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

debugServerT ::
     (HasLogger h) => JwtClaims -> ServerT DebugAPI (ReaderT h Handler)
debugServerT _ = listLogLevels :<|> modifyLogLevels
  where
    listLogLevels :: (HasLogger h) => ReaderT h Handler LogLevels
    listLogLevels = do
      namesTV <- asks (view $ loggerL . loggerNamesL)
      names <- readTVarIO namesTV
      priMapTV <- asks (view $ loggerL . priorityMapL)
      priMap <- readTVarIO priMapTV
      pure $ foldr (\n -> M.insert n (M.lookup n priMap)) M.empty names
    modifyLogLevels :: (HasLogger h) => LogLevels -> ReaderT h Handler NoContent
    modifyLogLevels lls = do
      namesTV <- asks (view $ loggerL . loggerNamesL)
      priMapTV <- asks (view $ loggerL . priorityMapL)
      for_ (M.toList lls) $ \(n, mpri) ->
        atomically $ do
          modifyTVar namesTV (S.insert n)
          modifyTVar priMapTV (M.alter (const mpri) n)
      pure NoContent
