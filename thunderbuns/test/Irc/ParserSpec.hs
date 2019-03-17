module Irc.ParserSpec where

import Data.List (isInfixOf)
import Test.Hspec
import Thunderbuns.Irc.Parser
import Thunderbuns.Irc.Types
import Thunderbuns.Tlude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Parse IRC Command" $ do
    it "PRIVMSG" $
      runParser parseCommand "PRIVMSG #world :hello" `shouldBe`
      Right (Command "PRIVMSG" ["#world", "hello"])
    it "PRIVMSG with CRLF" $
      runParser parseCommand "PRIVMSG #world :hello\r\n" `shouldBe`
      Right (Command "PRIVMSG" ["#world", "hello"])
    it "Commands cant have prefix" $
      runParser parseCommand ":server.name PRIVMSG #world :hello" `shouldSatisfy`
      leftInfixOf "no prefix allowed"
    it "print . parse == id" $
      for_ commandCorpus $ \bs -> do
        let Right cmd = runParser parseCommand bs
        printCommand cmd `shouldBe` bs
  describe "Parse IRC Message" $ do
    it "PRIVMSG" $
      runParser parseMessage ":nick!user@host PRIVMSG #world :hello\r\n" `shouldBe`
      Right
        (Message (Just "nick!user@host") (Cmd "PRIVMSG") ["#world", "hello"])
    it "PRIVMSG without CRLF" $
      runParser parseMessage ":nick!user@host PRIVMSG #world :hello" `shouldSatisfy`
      leftInfixOf "Must be terminated by CRLF"
    it "Message prefix is optional" $
      runParser parseMessage "PRIVMSG #world :hello\r\n" `shouldBe`
      Right (Message Nothing (Cmd "PRIVMSG") ["#world", "hello"])
    it "Message with IPv6 address prefix" $
      runParser
        parseMessage
        ":Blubb!~blubb@1234:123 QUIT :Ping timeout: 264 seconds\r\n" `shouldBe`
      Right
        (Message
           (Just "Blubb!~blubb@1234:123")
           (Cmd "QUIT")
           ["Ping timeout: 264 seconds"])
    it "print . parse == id" $
      for_ messageCorpus $ \bs -> do
        let Right msg = runParser parseMessage bs
        printMessage msg `shouldBe` bs
  describe "IRC LowerCase" $ do
    it "should lowercase normal strings" $ ircLowerCase "YES" `shouldBe` "yes"
    it "should handle utf8 characters" $
      ircLowerCase "J\xc3\x9cRGEN" `shouldBe` "j\xc3\xbcrgen"
    it "should handle the special irc rules" $
      ircLowerCase "[\\]~" `shouldBe` "{|}^"
  where
    leftInfixOf :: String -> Either String a -> Bool
    leftInfixOf s = either (isInfixOf s) (const False)

commandCorpus :: [ByteString]
commandCorpus = ["PRIVMSG #world hello\r\n", "PRIVMSG #world :hello there\r\n"]

messageCorpus :: [ByteString]
messageCorpus =
  [ ":nick!user@host PRIVMSG #world :hello there\r\n"
  , "PRIVMSG #world hello\r\n"
  , ":Blubb!~blubb@1234:123 QUIT :Ping timeout: 264 seconds\r\n"
  ]
