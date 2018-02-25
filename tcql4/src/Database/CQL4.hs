module Database.CQL4 where

import qualified Data.HashMap.Strict as M
import qualified Data.Serialize.Get as G
import qualified Data.Serialize.Put as P
import qualified Data.Text as T
import qualified Database.CQL4.Internal.Get as CG
import qualified Database.CQL4.Internal.Put as CP
import Database.CQL4.Types
import Data.Foldable

-- | Put the startup message
--
-- This is the first request on a new connection and
-- the server will respond either with `ERROR`
-- `READY` or `AUTHENTICATE`
startup :: P.Put
startup = do
  P.putNested
    (CP.frameHeader RequestFrame [] 0 OpStartup)
    (CP.map CP.string (M.singleton "CQL_VERSION" "3.3.0"))

-- | Put the Options message
--
-- Get server options.  This can be issued before startup.
options :: P.Put
options =
  CP.frameHeader RequestFrame [] 0 OpOptions 0


-- | query without bound parameters
unboundQuery :: Consistency -> T.Text -> Query
unboundQuery cl cql =
  UnboundQuery { query = cql, consistency = cl, skipMetadata = False, pageSize = Nothing
             , pagingState = Nothing, serialConsistency = Nothing
             , defaultTimestamp = Nothing }

executeQuery :: Query -> P.Put
executeQuery q = do
  P.putNested (CP.frameHeader RequestFrame [] 0 OpQuery) $ do
    CP.longString (query q)
    CP.consistency (consistency q)
    CP.queryFlags q
    CP.queryValues q
    for_ (pageSize q) CP.int
    for_ (pagingState q) CP.bytes
    for_ (serialConsistency q) CP.consistency
    for_ (defaultTimestamp q) CP.timestamp
    
    
    
-- | 

-- | Get the next message from the server
message :: G.Get Message
message = do
  h <- CG.frameHeader
  G.isolate (fromIntegral $ frameLength h) $
    case (frameOpCode h) of
      OpError -> errorMessage
      OpReady -> pure ReadyMsg
      OpAuthenticate -> AuthenticateMsg <$> CG.string
      OpSupported -> SupportedMsg <$> CG.map (CG.list CG.string)
      x -> fail ("not implemented: " ++ show x)

show' :: Show a => a -> T.Text
show' = T.pack . show

errorMessage :: G.Get Message
errorMessage = do
  code <- CG.int
  msg <- CG.string
  case code of
    0x0000 -> pure $ ErrorMsg code msg []
    0x000A -> pure $ ErrorMsg code msg []
    0x0100 -> pure $ ErrorMsg code msg []
    0x1000 -> do
      cl <- CG.consistency
      req <- CG.int
      alive <- CG.int
      pure $
        ErrorMsg
          code
          msg
          [("cl", show' cl), ("req", show' req), ("alive", show' alive)]
    0x1001 -> pure $ ErrorMsg code msg []
    0x1002 -> pure $ ErrorMsg code msg []
    0x1003 -> pure $ ErrorMsg code msg []
    0x1100 -> do
      cl <- CG.consistency
      received <- CG.int
      blockFor <- CG.int
      writeType <- CG.string
      pure $
        ErrorMsg
          code
          msg
          [ ("cl", show' cl)
          , ("received", show' received)
          , ("blockFor", show' blockFor)
          , ("writeType", writeType)
          ]
    0x1200 -> do
      cl <- CG.consistency
      received <- CG.int
      blockFor <- CG.int
      dataPresent <- CG.boolean
      pure $
        ErrorMsg
          code
          msg
          [ ("cl", show' cl)
          , ("received", show' received)
          , ("blockFor", show' blockFor)
          , ("dataPresent", show' dataPresent)
          ]
    0x1300 -> do
      cl <- CG.consistency
      received <- CG.int
      blockFor <- CG.int
      numFailures <- CG.int
      dataPresent <- CG.boolean
      pure $
        ErrorMsg
          code
          msg
          [ ("cl", show' cl)
          , ("received", show' received)
          , ("blockFor", show' blockFor)
          , ("numFailures", show' numFailures)
          , ("dataPresent", show' dataPresent)
          ]
    0x1400 -> do
      keyspace <- CG.string
      function <- CG.string
      argTypes <- CG.list CG.string
      pure $
        ErrorMsg
          code
          msg
          [ ("keyspace", keyspace)
          , ("function", function)
          , ("argTypes", show' argTypes)
          ]
    0x1500 -> do
      cl <- CG.consistency
      received <- CG.int
      blockFor <- CG.int
      numFailures <- CG.int
      writeType <- CG.string
      pure $
        ErrorMsg
          code
          msg
          [ ("cl", show' cl)
          , ("received", show' received)
          , ("blockFor", show' blockFor)
          , ("numFailures", show' numFailures)
          , ("writeType", writeType)
          ]
    0x2000 -> pure $ ErrorMsg code msg []
    0x2100 -> pure $ ErrorMsg code msg []
    0x2200 -> pure $ ErrorMsg code msg []
    0x2300 -> pure $ ErrorMsg code msg []
    0x2400 -> do
      keyspace <- CG.string
      table <- CG.string
      pure $ ErrorMsg code msg [("keyspace", keyspace), ("table", table)]
    0x2500 -> pure $ ErrorMsg code msg []
    x -> pure $ ErrorMsg code msg [("unknown code", show' x)]
