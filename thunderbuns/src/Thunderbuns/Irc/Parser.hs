module Thunderbuns.Irc.Parser where

import Control.Applicative
import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.ByteString as B
import qualified Data.Word as W
import Thunderbuns.Irc.Types
import Thunderbuns.Tlude

-- | Run the parser on the complete input.
--
-- It must match up until the end.
runParser :: Parser a -> ByteString -> Either String a
runParser p = parseOnly (p <* endOfInput)

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
printMessage :: Message -> ByteString
printMessage (Message pre cmd args) =
  let cmdStr = printCmd cmd
      lst = maybe [] (\x -> [":" <> x]) pre ++ (cmdStr : args)
   in B.intercalate " " (quoteLastArg lst) <> "\r\n"

printCmd :: Cmd -> ByteString
printCmd (Response code) = fromString $ show $ fromCode code
printCmd (Cmd bs) = bs

-- | Parse an irc command
parseCommand :: Parser Command
parseCommand = Command <$> token middle <*> parseArgs' <?> "Command"

-- | Bytestring representation of a Command
printCommand :: Command -> ByteString
printCommand (Command cmd args) =
  let lst = cmd : args
   in printArgs lst <> "\r\n"

printArgs :: [ByteString] -> ByteString
printArgs = B.intercalate " " . quoteLastArg

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

parsePrefix :: Parser (Maybe ByteString)
parsePrefix = optional (token (string ":" *> middle) <?> "prefix")

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

-- | Matches anything but space, cr, lf or : on the first character, allow : after
middle :: Parser ByteString
middle =
  fst <$>
  (match (void $ satisfy notSpCrLfCl <* Atto.takeWhile notSpCrLf) <?>
   "Middle - no prefix allowed")

-- | Matches : followed by anything but cr or lf or null
trailing :: Parser ByteString
trailing = char ':' *> Atto.takeWhile notCrLf

-- "not inner parts"
notSpCrLfCl :: W.Word8 -> Bool
notSpCrLfCl 0x00 = False
notSpCrLfCl 0x20 = False
notSpCrLfCl 0x0d = False
notSpCrLfCl 0x0a = False
notSpCrLfCl 0x3a = False -- ':'
notSpCrLfCl _ = True

-- "inner parts"
notSpCrLf :: W.Word8 -> Bool
notSpCrLf 0x00 = False
notSpCrLf 0x20 = False
notSpCrLf 0x0d = False
notSpCrLf 0x0a = False
notSpCrLf _ = True

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
