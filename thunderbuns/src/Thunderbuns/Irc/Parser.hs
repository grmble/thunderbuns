{-# LANGUAGE TemplateHaskell #-}

module Thunderbuns.Irc.Parser where

import Control.Applicative
import Control.Lens.TH (makeLenses, makePrisms)
import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as Atto
import Data.Functor
import Data.Text (Text)
import qualified Data.Text as T

-- | Well known error codes - only the ones we need ...
data Code
  = NumericCode Int
  | RplWelcome -- ^ sent after successful registration (also RplYourHost, RplCreated, RplMyInfo)
  | RplBounce -- ^ sent to suggest an alternative server
  deriving (Eq, Ord, Show)

data Command
  = Response Code
  | Command Text
  deriving (Eq, Show)

$(makePrisms ''Command)

data Message = Message
  { _msgPrefix :: Maybe Text
  , _msgCmd :: Command
  , _msgArgs :: [Text]
  } deriving (Eq, Show)

$(makeLenses ''Message)

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
parseMessageOrLine :: Parser (Either T.Text Message)
parseMessageOrLine = (Right <$> parseMessage) <|> (Left <$> parseLine)

parseAsText :: Parser T.Text
parseAsText =
  parseMessageOrLine >>= \case
    Left txt -> pure txt
    Right msg -> pure $ T.pack $ show msg ++ "\n"

parseMessage :: Parser Message
parseMessage = Message <$> parsePrefix <*> parseCommand <*> parseArgs

parsePrefix :: Parser (Maybe Text)
parsePrefix =
  option Nothing (fmap Just (token (char ':' *> takeWhile1 notSpCrLfCl)))

parseCommand :: Parser Command
parseCommand =
  token (threeDigitCode <|> fmap Command middle) <?>
  "3 digit message code OR irc command"
  where
    threeDigitCode = fmap (Response . toCode . read) (count 3 digit)

parseArgs :: Parser [Text]
parseArgs = many (trailing <|> token middle) <* skipCrLf

token :: Parser a -> Parser a
token p = p <* skipMany (char ' ')

-- | Matches anything but space, cr, lf or :
middle :: Parser T.Text
middle = Atto.takeWhile1 notSpCrLfCl

-- | Matches : followed by anything but cr or lf or null
trailing :: Parser T.Text
trailing = char ':' *> Atto.takeWhile notCrLf

notSpCrLfCl :: Char -> Bool
notSpCrLfCl '\x00' = False
notSpCrLfCl ' ' = False
notSpCrLfCl '\r' = False
notSpCrLfCl '\n' = False
notSpCrLfCl ':' = False
notSpCrLfCl _ = True

notCrLf :: Char -> Bool
notCrLf '\x00' = False
notCrLf '\r' = False
notCrLf '\n' = False
notCrLf _ = True

crLf :: Parser ()
crLf = string "\r\n" $> ()

skipCrLf :: Parser ()
skipCrLf = skipMany1 crLf

parseLine :: Parser T.Text
parseLine = Atto.takeWhile (/= '\n') <* skipMany (char '\n')
