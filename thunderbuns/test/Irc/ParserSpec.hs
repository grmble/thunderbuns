module Irc.ParserSpec where

import Data.Attoparsec.ByteString
import Data.List (isInfixOf)
import Test.Hspec
import Thunderbuns.Irc.Parser
import Thunderbuns.Irc.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Parse IRC Command" $ do
    it "PRIVMSG" $
      parseOnly (parseCommand <* endOfInput) "PRIVMSG #world :hello" `shouldBe`
      Right (Command "PRIVMSG" ["#world", "hello"])
    it "PRIVMSG with CRLF" $
      parseOnly (parseCommand <* endOfInput) "PRIVMSG #world :hello\r\n" `shouldBe`
      Right (Command "PRIVMSG" ["#world", "hello"])
    it "Commands cant have prefix" $
      parseOnly
        (parseCommand <* endOfInput)
        ":server.name PRIVMSG #world :hello" `shouldSatisfy`
      leftInfixOf "no prefix allowed"
  describe "Parse IRC Message" $ do
    it "PRIVMSG" $
      parseOnly
        (parseMessage <* endOfInput)
        ":nick!user@host PRIVMSG #world :hello\r\n" `shouldBe`
      Right
        (Message (Just "nick!user@host") (Cmd "PRIVMSG") ["#world", "hello"])
    it "PRIVMSG without CRLF" $
      parseOnly
        (parseMessage <* endOfInput)
        ":nick!user@host PRIVMSG #world :hello" `shouldSatisfy`
      leftInfixOf "Must be terminated by CRLF"
    it "Message prefix is optional" $
      parseOnly (parseMessage <* endOfInput) "PRIVMSG #world :hello\r\n" `shouldBe`
      Right (Message Nothing (Cmd "PRIVMSG") ["#world", "hello"])
  describe "IRC LowerCase" $ do
    it "should lowercase normal strings" $ ircLowerCase "YES" `shouldBe` "yes"
    it "should handle utf8 characters" $
      ircLowerCase "J\xc3\x9cRGEN" `shouldBe` "j\xc3\xbcrgen"
    it "should handle the special irc rules" $
      ircLowerCase "[\\]~" `shouldBe` "{|}^"
  where
    leftInfixOf :: String -> Either String a -> Bool
    leftInfixOf s = either (isInfixOf s) (const False)
