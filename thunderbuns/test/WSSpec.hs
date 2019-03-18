{-# LANGUAGE DuplicateRecordFields #-}

module WSSpec where

import Data.Maybe (fromJust)
import Data.ByteString.D64.UUID
import System.Log.Bunyan.LogText
import Test.Hspec hiding (example)
import Thunderbuns.Irc.Types
import Thunderbuns.WS.Api
import Thunderbuns.WS.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "GenericMessage vs ChannelMessage" $ do
    it "user/privmsg/channel" $
      toResponse example `shouldSatisfy` isChannelMessage
    it "user/notice/channel" $
      toResponse (example {msgCmd = Cmd "NOTICE"}) `shouldSatisfy`
      isChannelMessage
    it "user/privmsg/multiple channels" $
      toResponse (example {msgArgs = ["#haskell,#scala", "hi"]}) `shouldSatisfy`
      isChannelMessage
    it "from servername -> generic" $
      toResponse (example {msgPrefix = Just "the.server.name"}) `shouldNotSatisfy`
      isChannelMessage
    it "some other command -> generic" $
      toResponse (example {msgCmd = Cmd "LIST"}) `shouldNotSatisfy`
      isChannelMessage
    it "numeric code -> generic" $
      toResponse (example {msgCmd = Response (NumericCode 101)}) `shouldNotSatisfy`
      isChannelMessage
    it "message without prefix" $
      toResponse (example {msgPrefix = Nothing}) `shouldNotSatisfy`
      isChannelMessage
    it "channel does not start with &#+!" $
      toResponse (example {msgArgs = ["yourNick", "a private msg"]}) `shouldNotSatisfy`
      isChannelMessage
  describe "ChannelMessage contents" $ do
    it "msg is extracted from args" $
      let msg = (\ChannelMessage {msg} -> msg) <$> toChannelMessages example
       in msg `shouldBe` ["hello ladies and gentlemen"]
    it "command is as in input" $
      let cmd = (\ChannelMessage {cmd} -> cmd) <$> toChannelMessages example
       in cmd `shouldBe` ["PRIVMSG"]
    it "channels are split on ," $
      let channels =
            (\ChannelMessage {channel} -> channel) <$>
            toChannelMessages (example {msgArgs = ["#haskell,#scala", "hi"]})
       in channels `shouldBe` [Channel "#haskell", Channel "#scala"]
    it "channels must start with #!&+" $
      let channels = (\ChannelMessage {channel} -> channel) <$>
            toChannelMessages (example {msgArgs = ["#a,!b,&c,+d,e", "hi"]})
       in channels `shouldBe` (Channel <$> ["#a", "!b", "&c", "+d"])
    it "from is correctly parsed" $
      let from = (\ChannelMessage {from} -> from) <$> toChannelMessages example
       in from `shouldBe`
          [From {nick = Nick "nick", user = "username", host = "the.hostname"}]
    it "from is correctly stringified" $
      let from = (\ChannelMessage {from} -> from) <$> toChannelMessages example
       in fromToText <$> from `shouldBe` [toText (fromJust $ msgPrefix example)]

toChannelMessages :: Message -> [ChannelMessage]
toChannelMessages = snd . toResponse

toResponse :: Message -> ([GenericMessage], [ChannelMessage])
toResponse = responseFromMessage (OrderedUUID "fakeit")

example :: Message
example =
  Message
    { msgPrefix = Just "nick!username@the.hostname"
    , msgCmd = Cmd "PRIVMSG"
    , msgArgs = ["#haskell", "hello", "ladies", "and gentlemen"]
    }

isChannelMessage :: ([GenericMessage], [ChannelMessage]) -> Bool
isChannelMessage (_, []) = False
isChannelMessage _ = True
