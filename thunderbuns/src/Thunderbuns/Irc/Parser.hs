module Thunderbuns.Irc.Parser where

import Control.Applicative
import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.ByteString as B
import qualified Data.Word as W
import Thunderbuns.Irc.Types
import Thunderbuns.Tlude

-- | Parse a message from an irc server
--
-- The message is expected as on-the-wire.  it must be
-- terminated by CR LF
parseMessage :: Parser Message
parseMessage = Message <$> parsePrefix <*> parseCmd <*> parseArgs <?> "Message"

-- | Bytestring representation of a parsed IRC message.
--
-- The string will be terminated by CR LF.  This bytestring
-- can be sent to an IRC server.
ircLine :: Message -> ByteString
ircLine (Message pre cmd args) =
  let cmdStr = ircCmd cmd
      lst = (":" <> pre) : cmdStr : args
   in B.intercalate " " (quoteLastArg lst) <> "\r\n"

ircCmd :: Cmd -> ByteString
ircCmd (Response code) = fromString $ show $ fromCode code
ircCmd (Cmd bs) = bs

-- | Parse an irc command
parseCommand :: Parser Command
parseCommand =
  Command <$> token middle <*> parseArgs' <?> "Command"

-- | Bytestring representation of a Command
ircCmdLine :: Command -> ByteString
ircCmdLine (Command cmd args) =
  let lst = cmd : args
   in ircArgs lst <> "\r\n"

ircArgs :: [ByteString] -> ByteString
ircArgs = B.intercalate " " . quoteLastArg

quoteLastArg :: [ByteString] -> [ByteString]
quoteLastArg [] = []
quoteLastArg [x]
  | " " `B.isInfixOf` x = [":" <> x]
  | otherwise = [x]
quoteLastArg (x:xs) = x : quoteLastArg xs

fromCode :: Code -> Int
fromCode (NumericCode x) = x
fromCode RplWelcome = 1
fromCode RplBounce = 5
fromCode RplNickInUse = 433

toCode :: Int -> Code
toCode 1 = RplWelcome
toCode 5 = RplBounce
toCode 433 = RplNickInUse
toCode x = NumericCode x

isErrorCode :: Code -> Bool
isErrorCode c = fromCode c >= 400

parsePrefix :: Parser ByteString
parsePrefix =
  token (string ":" *> takeWhile1 notSpCrLfCl) <?> "Mandatory prefix"

parseCmd :: Parser Cmd
parseCmd =
  token (threeDigitCode <|> fmap Cmd middle) <?>
  "3 digit message code OR irc command"
  where
    threeDigitCode = fmap (Response . toCode . read) (count 3 digit)

parseArgs :: Parser [ByteString]
parseArgs = many (trailing <|> token middle) <* crLf <?> "Args"

parseArgs' :: Parser [ByteString]
parseArgs' =
  many (trailing <|> token middle) <* option () crLf <?> "Args CRLF optional"

token :: Parser a -> Parser a
token p = p <* skipMany (char ' ')

-- | Matches anything but space, cr, lf or :
middle :: Parser ByteString
middle = Atto.takeWhile1 notSpCrLfCl <?> "Middle - at least 1 char, no prefix allowed"

-- | Matches : followed by anything but cr or lf or null
trailing :: Parser ByteString
trailing = char ':' *> Atto.takeWhile notCrLf

notSpCrLfCl :: W.Word8 -> Bool
notSpCrLfCl 0x00 = False
notSpCrLfCl 0x20 = False
notSpCrLfCl 0x0d = False
notSpCrLfCl 0x0a = False
notSpCrLfCl 0x3a = False -- ':'
notSpCrLfCl _ = True

notCrLf :: W.Word8 -> Bool
notCrLf 0x00 = False
notCrLf 0x0d = False
notCrLf 0x0a = False
notCrLf _ = True

crLf :: Parser ()
crLf = string "\r\n" $> () <?> "Must be terminated by CRLF"

skipCrLf :: Parser ()
skipCrLf = skipMany1 crLf

parseLine :: Parser ByteString
parseLine = B.snoc <$> Atto.takeWhile (/= 0x0a) <*> word8 0x0a

c2w :: Char -> W.Word8
c2w = toEnum . fromEnum

char :: Char -> Parser W.Word8
char = word8 . c2w

digit :: Parser Char
digit = fmap (toEnum . fromEnum) (satisfy $ \x -> x >= 0x30 && x <= 0x39)
