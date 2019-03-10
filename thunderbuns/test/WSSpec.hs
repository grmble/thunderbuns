{-# LANGUAGE DuplicateRecordFields #-}

module WSSpec where

import Test.Hspec hiding (example)
import Thunderbuns.Irc.Types
import Thunderbuns.WS.Handler
import Thunderbuns.WS.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "GenericMessage vs ChannelMessage" $ do
    it "user/privmsg/channel" $
      classify example `shouldSatisfy` isChannelMessage
    it "user/notice/channel" $
      classify (example {msgCmd = Cmd "NOTICE"}) `shouldSatisfy`
      isChannelMessage
    it "user/privmsg/multiple channels" $
      classify (example {msgArgs = ["#haskell,#scala", "hi"]}) `shouldSatisfy`
      isChannelMessage
    it "from servername -> generic" $
      classify (example {msgPrefix = "the.server.name"}) `shouldNotSatisfy`
      isChannelMessage
    it "some other command -> generic" $
      classify (example {msgCmd = Cmd "LIST"}) `shouldNotSatisfy`
      isChannelMessage
    it "numeric code -> generic" $
      classify (example {msgCmd = Response (NumericCode 101)}) `shouldNotSatisfy`
      isChannelMessage
    it "channel does not start with &#+!" $
      classify (example {msgArgs = ["yourNick", "a private msg"]}) `shouldNotSatisfy`
      isChannelMessage
  describe "ChannelMessage contents" $ do
    it "msg is extracted from args" $
      let ChannelMessage {msg} = classify example
       in msg `shouldBe` "hello ladies and gentlemen"
    it "command is as in input" $
      let ChannelMessage {cmd} = classify example
       in cmd `shouldBe` "PRIVMSG"
    it "channels are split on ," $
      let ChannelMessage {channels} =
            classify (example {msgArgs = ["#haskell,#scala", "hi"]})
       in channels `shouldBe` [Channel "#haskell", Channel "#scala"]
    it "channels must start with #!&+" $
      let ChannelMessage {channels} =
            classify (example {msgArgs = ["#a,!b,&c,+d,e", "hi"]})
       in channels `shouldBe` (Channel <$> ["#a", "!b", "&c", "+d"])
    it "from is correctly parsed" $
      let ChannelMessage {from} = classify example
       in from `shouldBe`
          From {nick = Nick "nick", user = "username", host = "the.hostname"}

classify :: Message -> Response
classify = classifyIrcMessage

example :: Message
example =
  Message
    { msgPrefix = "nick!username@the.hostname"
    , msgCmd = Cmd "PRIVMSG"
    , msgArgs = ["#haskell", "hello", "ladies", "and gentlemen"]
    }

isChannelMessage :: Response -> Bool
isChannelMessage ChannelMessage {} = True
isChannelMessage _ = False
