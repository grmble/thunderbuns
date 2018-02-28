module Database.CQL4
  ( startup
  , options
  , unboundQuery
  , executeQuery
  , message
  , errorMessage
  ) where

import Data.Foldable
import qualified Data.HashMap.Strict as M
import Data.Int
import qualified Data.Serialize.Get as G
import qualified Data.Serialize.Put as P
import qualified Data.Text as T
import qualified Database.CQL4.Internal.Get as CG
import qualified Database.CQL4.Internal.Put as CP
import Database.CQL4.Types

-- | Put the startup message
--
-- This is the first request on a new connection and
-- the server will respond either with `ERROR`
-- `READY` or `AUTHENTICATE`
startup :: P.Put
startup =
  P.putNested
    (CP.frameHeader RequestFrame [] 0 OpStartup)
    (CP.map CP.string (M.singleton "CQL_VERSION" "3.3.0"))

-- | Put the Options message
--
-- Get server options.  This can be issued before startup.
options :: P.Put
options = CP.frameHeader RequestFrame [] 0 OpOptions 0

-- | query without bound parameters
unboundQuery :: Consistency -> T.Text -> Query
unboundQuery cl cql =
  UnboundQuery
    { query = cql
    , consistency = cl
    , skipMetadata = False
    , pageSize = Nothing
    , pagingState = Nothing
    , serialConsistency = Nothing
    , defaultTimestamp = Nothing
    }

executeQuery :: Query -> P.Put
executeQuery q =
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
    case frameOpCode h of
      OpError -> errorMessage
      OpReady -> pure ReadyMsg
      OpAuthenticate -> AuthenticateMsg <$> CG.string
      OpSupported -> SupportedMsg <$> CG.map (CG.list CG.string)
      x -> fail ("not implemented: " ++ show x)

errorMessage :: G.Get Message
errorMessage = do
  code <- CG.int
  msg <- CG.string
  case code of
    0x0000 -> pure $ ErrorMsg code msg []
    0x000A -> pure $ ErrorMsg code msg []
    0x0100 -> pure $ ErrorMsg code msg []
    0x1000 ->
      _errorMsg code msg [_epConsistency, _epInt "req", _epInt "alive"]
    0x1001 -> pure $ ErrorMsg code msg []
    0x1002 -> pure $ ErrorMsg code msg []
    0x1003 -> pure $ ErrorMsg code msg []
    0x1100 ->
      _errorMsg
        code
        msg
        [_epConsistency, _epInt "received", _epInt "blockFor", _epString "writeType"]
    0x1200 ->
      _errorMsg
        code
        msg
        [_epConsistency, _epInt "received", _epInt "blockFor", _epBool "dataPresent"]
    0x1300 ->
      _errorMsg
        code
        msg
        [ _epConsistency
        , _epInt "received"
        , _epInt "blockFor"
        , _epInt "numFailures"
        , _epBool "dataPresent"
        ]
    0x1400 ->
      _errorMsg
        code
        msg
        [ _epString "keyspace"
        , _epString "function"
        , _ep "argTypes" show' (CG.list CG.string)
        ]
    0x1500 ->
      _errorMsg
        code
        msg
        [ _epConsistency
        , _epInt "received"
        , _epInt "blockFor"
        , _epInt "numFailures"
        , _epString "writeType"
        ]
    0x2000 -> pure $ ErrorMsg code msg []
    0x2100 -> pure $ ErrorMsg code msg []
    0x2200 -> pure $ ErrorMsg code msg []
    0x2300 -> pure $ ErrorMsg code msg []
    0x2400 -> _errorMsg code msg [_epString "keyspace", _epString "table"]
    0x2500 -> pure $ ErrorMsg code msg []
    x -> pure $ ErrorMsg code msg [("unknown code", show' x)]
  where
    show' :: Show a => a -> T.Text
    show' = T.pack . show
    _errorMsg :: Int32 -> T.Text -> [G.Get (T.Text, T.Text)] -> G.Get Message
    _errorMsg code msg ps = ErrorMsg code msg <$> sequence ps
    _ep :: T.Text -> (a -> T.Text) -> G.Get a -> G.Get (T.Text, T.Text)
    _ep n f g = ((n, ) . f) <$> g
    _epConsistency :: G.Get (T.Text, T.Text)
    _epConsistency = _ep "cl" show' CG.consistency
    _epInt :: T.Text -> G.Get (T.Text, T.Text)
    _epInt n = _ep n show' CG.int
    _epString :: T.Text -> G.Get (T.Text, T.Text)
    _epString n = (n, ) <$> CG.string
    _epBool :: T.Text -> G.Get (T.Text, T.Text)
    _epBool n = _ep n show' CG.boolean
