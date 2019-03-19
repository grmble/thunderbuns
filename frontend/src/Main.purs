module Main where

import Prelude

import Bonsai (debugProgram, noDebug)
import Bonsai.Core (issueCommand)
import Bonsai.DOM (ElementId(..), document, effF, locationHash, window)
import Data.Maybe (Maybe(..))
import Data.Lens (set)
import Data.String as String
import Effect (Effect)
import Thunderfront.Controller (update)
import Thunderfront.Types.Model (activeChannel, emptyModel)
import Thunderfront.Types.Msg (Msg(..))
import Thunderfront.Types.WS as WS
import Thunderfront.View (viewMain)
import Thunderfront.WebSocket (consoleHandler, newWebSocket, onOpen, onClose, onMessage, onError)

main :: Effect Unit
main = do
  -- T.configure T.defaultOptions
  hash <- effF $ window >>= document >>= locationHash
  let model = if String.length hash > 2
              then set activeChannel (pure (WS.Channel (String.drop 2 hash))) emptyModel
              else emptyModel
  prg <- dbgProgram (ElementId "main") update viewMain model window

  ws <- newWebSocket ""
  onOpen (const $ issueCommand prg (pure (WebSocketMsg (Just ws)))) ws
  onClose (const $ issueCommand prg (pure (WebSocketMsg Nothing))) ws
  flip onMessage ws $ \ev -> do
    msg <- effF $ WS.decodeMessageEvent ev
    issueCommand prg (pure $ ResponseMsg msg)
  onError consoleHandler ws

  pure unit
  where
    dbgProgram =
      debugProgram (noDebug
        { timing = true
        -- , events = true
        })
