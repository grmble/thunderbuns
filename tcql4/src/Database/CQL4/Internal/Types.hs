{-# LANGUAGE StrictData #-}

{- | CQL Types common for get and put

Note that the values of the Enums are highly magical -
they are the actual codes on the wire.

No rearranging ...
-}
module Database.CQL4.Internal.Types where

import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as M
import Data.Int
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Database.CQL4.Types

-- | The frame header is transmitted before the data of the frame
data FrameHeader = FrameHeader
  { frameVersion :: FrameVersion
  , frameFlags :: [FrameFlag]
  , frameStream :: StreamID
  , frameOpCode :: OpCode
  , frameLength :: Int32 -- ^ Length of the following frame body, max 256MB
  } deriving (Show, Eq)

-- | A stream id identifies the request of a response
--
-- The client must not generate negative stream ids (reserved for events
-- originating from the server)
type StreamID = Int16

-- | The FrameVersion is either 0x04 (Request) or 0x84 (Response)
data FrameVersion
  = RequestFrame
  | ResponseFrame
  deriving (Show, Eq)

-- | The flags are combinined in a single Word8
--
-- The enum gives the bit position - toggle the bit to set the flag
data FrameFlag
  = FlagCompress
  | FlagTrace
  | FlagCustomPayload
  | FlagWarning
  deriving (Show, Eq, Enum)

-- | Frame Op Codes
data OpCode
  = OpError
  | OpStartup
  | OpReady
  | OpAuthenticate
  | OpUnusedV4
  | OpOptions
  | OpSupported
  | OpQuery
  | OpResult
  | OpPrepare
  | OpExecute
  | OpRegister
  | OpEvent
  | OpBatch
  | OpAuthChallenge
  | OpAuthResponse
  | OpAuthSuccess
  deriving (Show, Eq, Enum)

-- | A CQL query
--
-- Note that this does not have query flags - the query flags
-- are determined by the type of the query (and the parameters
-- that are present)
data Query = Query
  { queryText :: T.Text
  , queryConsistency :: Consistency
  , queryValues :: [TypedValue]
  , querySkipMetadata :: Bool
  , queryPageSize :: Maybe Int32
  , queryPagingState :: Maybe B.ByteString
  , querySerialConsistency :: Maybe Consistency
  , queryDefaultTimestamp :: Maybe UTCTime
  } deriving (Show, Eq)

-- | Query flags
--
-- Note that for this enum the enum value is the bit position,
-- the flag is 2^enum 
data QueryFlags
  = QFValues
  | QFSkipMetadata
  | QFPageSize
  | QFPagingState
  | QFSerialConsistency
  | QFDefaultTimestamp
  | QFNamedValues
  deriving (Show, Eq, Enum)

-- | Metadata flags
--
-- again, bit position enum
data MetadataFlag
  = GlobalTableSpec
  | HasMorePages
  | NoMetadata
  deriving (Show, Eq, Enum)

-- | Messages that can be received from the server
--
-- Only messages that can be received by the client are relevant here
data Message
  = ErrorMsg ErrorMsgT
  | ReadyMsg
  | AuthenticateMsg T.Text
  | SupportedMsg (M.HashMap T.Text [T.Text])
  | ResultMsg QueryResult
  deriving (Show, Eq)

data ErrorMsgT = ErrorMsgT
  { errorCode :: Int32
  , errorMsg :: T.Text
  , errorParams :: [(T.Text, T.Text)]
  } deriving (Show, Eq)

-- | QueryResult represents the different result types for a query
data QueryResult
  = QueryResultVoid -- ^ query with empty/no result
  | QueryResultRows ResultRows
  | QueryResultKeyspace T.Text
  | QueryResultPrepared -- XXX implement me
  | QueryResultSchemaChanged SchemaChange
  deriving (Show, Eq)

-- | A schema change result or event
data SchemaChange = SchemaChange
  { changeType :: ChangeType
  , changeObject :: ChangeObject
  } deriving (Show, Eq)

-- | Change type - what happened?
data ChangeType
  = ChangeCreated
  | ChangeUpdated
  | ChangeDropped
  deriving (Enum, Show, Eq)

-- | The changed object in a schema change
--
-- Combines change_target and change_options from the protocl spec.
-- One value is the keyspace, 2 the keyspace and object, if there is a list
-- it's one string for each argument
data ChangeObject
  = ChangeKeyspace T.Text
  | ChangeTable T.Text
                T.Text
  | ChangeType T.Text
               T.Text
  | ChangeFunction T.Text
                   T.Text
                   [T.Text]
  | ChangeAggregate T.Text
                    T.Text
                    [T.Text]
  deriving (Show, Eq)
