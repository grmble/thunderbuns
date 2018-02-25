{- | CQL Types common for get and put

Note that the values of the Enums are highly magical -
they are the actual codes on the wire.

No rearranging ...
-}
module Database.CQL4.Types where

import Data.Int
import qualified Data.Text as T

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

-- | Messages that can be received from the server
--
-- Only messages that can be received by the client are relevant here
data Message
  = ErrorMsg { errorCode :: Int32, errorMsg :: T.Text, errorParams :: [(T.Text, T.Text)] }
  | ReadyMsg
  | AuthenticateMsg T.Text
  deriving (Show, Eq)
