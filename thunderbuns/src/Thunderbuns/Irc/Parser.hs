{-# LANGUAGE TemplateHaskell #-}

module Thunderbuns.Irc.Parser where

import Control.Applicative
import Control.Lens.TH (makeLenses, makePrisms)
import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString as Atto
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Functor
import Data.Monoid ((<>))
import Data.String (fromString)
import qualified Data.Word as W

-- | Well known error codes - only the ones we need ...
data Code
  = NumericCode Int
  | RplWelcome -- ^ sent after successful registration (also RplYourHost, RplCreated, RplMyInfo)
  | RplBounce -- ^ sent to suggest an alternative server
  deriving (Eq, Ord, Show)

data Command
  = Response !Code
  | Command !ByteString
  deriving (Eq, Show)

$(makePrisms ''Command)

data Message = Message
  { _msgPrefix :: !(Maybe ByteString)
  , _msgCmd :: !Command
  , _msgArgs :: ![ByteString]
  } deriving (Eq, Show)

$(makeLenses ''Message)

ircLine :: Message -> ByteString
ircLine (Message pre cmd args) =
  let cmdStr =
        case cmd of
          Response code -> fromString $ show $ fromCode code
          Command bs -> bs
      lst = maybe [] (pure . (<>) ":") pre ++ cmdStr : args
   in B.intercalate " " lst

fromCode :: Code -> Int
fromCode (NumericCode x) = x
fromCode RplWelcome = 1
fromCode RplBounce = 5

toCode :: Int -> Code
toCode 1 = RplWelcome
toCode 5 = RplBounce
toCode x = NumericCode x

isErrorCode :: Code -> Bool
isErrorCode c = fromCode c >= 400

-- | Try to parse message, if we can't parse until next CRLF
parseMessageOrLine :: Parser (Either ByteString Message)
parseMessageOrLine = (Right <$> parseMessage) <|> (Left <$> parseLine)

parseAsByteString :: Parser ByteString
parseAsByteString =
  parseMessageOrLine >>= \case
    Left txt -> pure txt
    Right msg -> pure $ ircLine msg

parseMessage :: Parser Message
parseMessage = Message <$> parsePrefix <*> parseCommand <*> parseArgs

parsePrefix :: Parser (Maybe ByteString)
parsePrefix =
  option Nothing (fmap Just (token (char ':' *> takeWhile1 notSpCrLfCl)))

parseCommand :: Parser Command
parseCommand =
  token (threeDigitCode <|> fmap Command middle) <?>
  "3 digit message code OR irc command"
  where
    threeDigitCode = fmap (Response . toCode . read) (count 3 digit)

parseArgs :: Parser [ByteString]
parseArgs = many (trailing <|> token middle) <* crLf

token :: Parser a -> Parser a
token p = p <* skipMany (char ' ')

-- | Matches anything but space, cr, lf or :
middle :: Parser ByteString
middle = Atto.takeWhile1 notSpCrLfCl

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
crLf = string "\r\n" $> ()

skipCrLf :: Parser ()
skipCrLf = skipMany1 crLf

parseLine :: Parser ByteString
parseLine = Atto.takeWhile (/= 0x0a) <* word8 0x0a

c2w :: Char -> W.Word8
c2w = toEnum . fromEnum

char :: Char -> Parser W.Word8
char = word8 . c2w

digit :: Parser Char
digit = fmap (toEnum . fromEnum) (satisfy $ \x -> x >= 0x30 && x <= 0x39)
