{- | CQL4 Connection Types

It's not in Types because of import circularity - all those internal modules
make the module tree more complicated.

On the bright side, everything is exported.
-}
module Database.CQL4.Connection where

import Control.Concurrent.QSem
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import qualified Data.ByteString as B
import Data.Conduit.Network (AppData)
import qualified Data.HashMap.Strict as M
import Database.CQL4.Exceptions
import Database.CQL4.Internal.Types
import UnliftIO.Concurrent (ThreadId)
import UnliftIO.STM

-- | A Connection is a ReaderT with mutable state + ExceptT
type ConnectionIO = ReaderT Connection (ExceptT CQLException IO)

-- | The data inside a ConnectionIO ReaderT
data Connection = Connection
  { connSocket :: AppData -- ^ conduit app data for the underlying socket
  , connSem :: QSem -- ^ semaphore that guards writing to the socket
  , connStreamID :: TVar StreamID -- ^ counter for generating stream ids
  , connPending :: TVar (M.HashMap StreamID (TMVar Message)) -- ^ map of pending requests
  , connDispatcher :: TMVar ThreadId -- ^ dedicated thread to dispatch server messages
  , connLogger :: Logger -- ^ logging function for requests/responses
  }

-- | A logging function
--
-- Will log the bytestrings on the wire, for debugging
--
-- See hexdumpLogger or (const pure)
type Logger = String -> B.ByteString -> IO B.ByteString
