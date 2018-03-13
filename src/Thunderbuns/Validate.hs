{-| THunderbuns Validaiton

The core type here is `V` - which is for Vende^H^H^H^H ... it's for Validated.

If it has a V in front, it has been validated, feel free store it in a database,
display it to the user, ...

If it has not been validated: maybe you should.  E.g. newlines can be bad for logging -
if you accept strings with newlines, someone may poison your logging system
(Thunderbuns JSON-based logger is safe though - special charcters are simply escaped).
Less and greater signs can screw up code that builds HTML.  Single quotes are
dangerous for SQL.

Naughty list:

* Less/Greater: fun with html
* Ampersand: xml decl trickeries (in combination with ;
* Single quotes: fun with sql, most likely in combination with ; and -.
* Dots, Backslashes and Slashes: fun with urls or filesystem paths
* Newlines: log poisoning, generally any ASCII control character

The default validator for strings does not allow the naughties, except for
single quote and dots (common in names). Remember to use bind variables in your SQL.


XXX: Product co-monad? Reader?
-}
module Thunderbuns.Validate
  ( V -- no data constructor
  , DefaultValidator(..)
  , ValidationError
  , Validator
  , unV
  , validate
  , validate'
  , enumValidator
  , inRange
  , symbolValidator
  , symbolValidator'
  , textValidator
  ) where

import Control.Applicative.Lift
import Data.Attoparsec.Text
import Data.Char
import qualified Data.List as L
import qualified Data.Text as T

-- | A newtype for a validated value.
newtype V a =
  V a
  deriving (Show, Eq, Ord, Functor)

unV :: V a -> a
unV (V a) = a

-- XXX: ValidationError should be the monoid that accumulates labeled messages
type ValidationError = String

type Validator a = Maybe String -> a -> Errors [ValidationError] a

-- | Manual validation with an explicit validator
validate' :: Maybe String -> Validator a -> a -> Either [ValidationError] (V a)
validate' msg v a = V <$> runErrors (v msg a)

-- | Validation using the default validator for a type.
validate :: DefaultValidator a => a -> Either [ValidationError] (V a)
validate = validate' Nothing defaultValidator

-- | Most basic types are safe, so they have instances
--
-- Anything which has an enum instance has an instance as well.
--
-- What is missing: all string types.  Validate those using the text
-- validators.
--
-- Write an instance for your record types, and you are good to go.
class DefaultValidator a where
  defaultValidator :: Validator a

instance DefaultValidator Integer where
  defaultValidator _ = pure

instance DefaultValidator Int where
  defaultValidator _ = pure

instance DefaultValidator Double where
  defaultValidator _ = pure

instance DefaultValidator Float where
  defaultValidator _ = pure

instance DefaultValidator Bool where
  defaultValidator _ = pure

instance DefaultValidator T.Text where
  defaultValidator = textValidator

mkFailure :: Maybe String -> String -> Errors [ValidationError] a
mkFailure Nothing s = failure [s]
mkFailure (Just m) s = failure [m ++ ": " ++ s]

mkParser :: Maybe String -> Parser a -> Parser a
mkParser msg p =
  case msg of
    Nothing -> p
    Just s -> p <?> s

-- | An enum always validates, it can not have wrong values
enumValidator :: (Enum a, Show a) => Validator a
enumValidator = const pure

-- | A range validation
inRange :: (Enum a, Show a) => a -> a -> Validator a
inRange low hi msg x =
  if low' <= x' && x' <= hi'
    then pure x
    else mkFailure msg ("not in range: " ++ show low ++ " " ++ show hi)
  where
    low' = fromEnum low
    hi' = fromEnum hi
    x' = fromEnum x

-- | Construct a parsing validator for text.
parsingValidator :: Parser T.Text -> Validator T.Text
parsingValidator p msg a = go (parse (mkParser msg p) a)
  where
    go rslt =
      case rslt of
        Fail _ ss s -> failure [L.intercalate ": " (ss ++ [s])]
        Partial cb -> go $ cb ""
        Done i r ->
          if T.null i
            then pure r
            else failure ["input not exhausted: " ++ T.unpack i]

-- | Validates a symbol - must start with letter, folloed by alphanumeric, dots or underscoe
symbolValidator :: Validator T.Text
symbolValidator =
  symbolValidator' "dots or underscores" (\x -> x == '.' || x == '_')

-- | Validates a symbol - must start with letter, folloed by alphanumeric and additional sybmols
symbolValidator' :: String -> (Char -> Bool) -> Validator T.Text
symbolValidator' pmsg p = parsingValidator (fst <$> match symbolParser)
  where
    symbolParser = do
      _ <- letter <?> "must start with letter"
      let msg = "must be alphanumeric or " ++ pmsg
      _ <- Data.Attoparsec.Text.takeWhile (\x -> isAlphaNum x || p x) <?> msg
      endOfInput <?> msg

-- | Default text validator
--
-- Allows letters, digits, underscores, dots, single quotes and space charcters
-- (but only 0x20 space, no tabs, newlines or similiar)
--
-- The text is stripped of leading and trailing spaces, empty is not allowed
-- (use a Maybe Text for that)
textValidator :: Validator T.Text
textValidator str a = parsingValidator (fst <$> match textParser) str stripped
  where
    stripped = T.dropAround (== toEnum 0x20) a
    msg = "letter, digit, underscore, dot, single quote or space"
    charPred x = isAlphaNum x || x == '_' || x == '.' || x == '\'' || x == toEnum 0x20
    charParser = satisfy charPred <?> msg
    greedyParser = Data.Attoparsec.Text.takeWhile charPred <?> msg
    textParser = do
      _ <- charParser
      _ <- greedyParser
      endOfInput <?> msg
