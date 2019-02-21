module Thunderbuns.Irc.Parser where

import Control.Applicative
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
  | RplNickInUse -- ^ the nick is already used
  deriving (Eq, Ord, Show)

data Cmd
  = Response !Code
  | Cmd !ByteString
  deriving (Eq, Show)

data Message = Message
  { msgPrefix :: !(Maybe ByteString)
  , msgCmd :: !Cmd
  , msgArgs :: ![ByteString]
  } deriving (Eq, Show)

ircLine :: Message -> ByteString
ircLine (Message pre cmd args) =
  let cmdStr =
        case cmd of
          Response code -> fromString $ show $ fromCode code
          Cmd bs -> bs
      lst = maybe [] (pure . (<>) ":") pre ++ cmdStr : args
   in B.intercalate " " (quoteLastArg lst) <> "\r\n"

quoteLastArg :: [ByteString] -> [ByteString]
quoteLastArg [] = []
quoteLastArg [x] = [":" <> x]
quoteLastArg (x :xs) = x : quoteLastArg xs

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

parseMessage :: Parser Message
parseMessage = Message <$> parsePrefix <*> parseCmd <*> parseArgs

parsePrefix :: Parser (Maybe ByteString)
parsePrefix =
  option Nothing (fmap Just (token (char ':' *> takeWhile1 notSpCrLfCl)))

parseCmd :: Parser Cmd
parseCmd =
  token (threeDigitCode <|> fmap Cmd middle) <?>
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
