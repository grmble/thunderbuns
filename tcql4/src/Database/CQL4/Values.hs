{- |  CQL4 Values - conversion to and from TypedValue

The module provides functions to convert between TypedValue
(the on-the-wire representation) to haskell types.
-}
module Database.CQL4.Values where

import Control.Monad (when)
import Control.Monad.Except (liftEither, throwError)
import Control.Monad.State (StateT, evalStateT, get, put)
import qualified Data.ByteString as B
import Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.Scientific as Scientific
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Data.Time.Clock (DiffTime, UTCTime)
import qualified Data.UUID as U
import qualified Data.Vector as V
import Database.CQL4.Connection
import Database.CQL4.Exceptions
import Database.CQL4.Types

-- | HasToValue is a type that has a cassandra equivalent
--
-- Note that cassandra is strict about the types it receives -
-- if a table column has type X, the bind parameter must be exactly
-- type X.  E.g. for a 16bit int (Cassandra: smallint), only
-- a 16 bit value will do -  you can't just transmit a 8-bit int.
--
-- So the recommandation is to use the exact equivalent type
-- in your haskell data structure.  E.g. if you use `int`,
-- use Int32 in your data structure.
--
-- Regarding TimeUUIDValue: TimeUUIDValue and UUIDValue both
-- map to UUID.  So HasToValue has to choose one, and UUIDValue
-- was chosen.  If you have a timeduuid column, you have to use
-- the data constructor TimeUUIDValue instead of toValue.
class HasToValue a where
  toValue :: a -> TypedValue

instance HasToValue T.Text where
  toValue = TextValue

instance HasToValue Integer where
  toValue = VarintValue

instance HasToValue Int64 where
  toValue = LongValue

instance HasToValue Int32 where
  toValue = IntValue

instance HasToValue Int16 where
  toValue = SmallintValue

instance HasToValue Int8 where
  toValue = TinyintValue

instance HasToValue Float where
  toValue = FloatValue

instance HasToValue Double where
  toValue = DoubleValue

instance HasToValue Scientific.Scientific where
  toValue = DecimalValue

instance HasToValue Bool where
  toValue = BoolValue

instance HasToValue B.ByteString where
  toValue = BlobValue

instance HasToValue U.UUID where
  toValue = UUIDValue

instance HasToValue Day where
  toValue = DateValue

instance HasToValue UTCTime where
  toValue = TimestampValue

instance HasToValue DiffTime where
  toValue = TimeValue

instance HasToValue IPAddress where
  toValue = InetValue

-- | HasFromValue extracts a haskell type from a cassandra TypedValue
--
-- This may fail - e.g. the TypedValue could be a Double, but you want
-- a UUID. Or it's an Int32, and you want a Int64.  The design decision
-- here was to be strict because Cassndra is strict as well when receiving
-- values.
class HasFromValue a where
  fromValue :: TypedValue -> Either CQLException a

instance HasFromValue T.Text where
  fromValue (TextValue t) = Right t
  fromValue x = Left $ fromValueException "Text" x

instance HasFromValue Integer where
  fromValue (VarintValue v) = Right v
  fromValue x = Left $ fromValueException "Integer" x

instance HasFromValue Int64 where
  fromValue (LongValue v) = Right (fromIntegral v)
  fromValue x = Left $ fromValueException "Int" x

instance HasFromValue Int32 where
  fromValue (IntValue v) = Right (fromIntegral v)
  fromValue x = Left $ fromValueException "Int32" x

instance HasFromValue Int16 where
  fromValue (SmallintValue v) = Right (fromIntegral v)
  fromValue x = Left $ fromValueException "Int16" x

instance HasFromValue Int8 where
  fromValue (TinyintValue v) = Right (fromIntegral v)
  fromValue x = Left $ fromValueException "Int8" x

instance HasFromValue Float where
  fromValue (FloatValue v) = Right v
  fromValue x = Left $ fromValueException "Float" x

instance HasFromValue Double where
  fromValue (DoubleValue d) = Right d
  fromValue x = Left $ fromValueException "Double" x

instance HasFromValue Scientific.Scientific where
  fromValue (DecimalValue sc) = Right sc
  fromValue x = Left $ fromValueException "Scientific" x

instance HasFromValue Bool where
  fromValue (BoolValue b) = Right b
  fromValue x = Left $ fromValueException "Bool" x

instance HasFromValue B.ByteString where
  fromValue (BlobValue bs) = Right bs
  fromValue x = Left $ fromValueException "ByteString" x

instance HasFromValue U.UUID where
  fromValue (UUIDValue uu) = Right uu
  fromValue x = Left $ fromValueException "UUID" x

instance HasFromValue Day where
  fromValue (DateValue d) = Right d
  fromValue x = Left $ fromValueException "Day" x

instance HasFromValue UTCTime where
  fromValue (TimestampValue ts) = Right ts
  fromValue x = Left $ fromValueException "UTCTime" x

instance HasFromValue DiffTime where
  fromValue (TimeValue t) = Right t
  fromValue x = Left $ fromValueException "DiffTime" x

instance HasFromValue IPAddress where
  fromValue (InetValue ip) = Right ip
  fromValue x = Left $ fromValueException "IPAddress" x

-- | Helper monad to extract values from a result set
--
-- It maintains a current slice of the rows values,
-- allowing you to take one value after the other using
-- `extract`.
type ValueExtractorIO = StateT (V.Vector TypedValue) ConnectionIO

-- | Extract the next value from the result row
extract :: HasFromValue a => ValueExtractorIO a
extract = do
  vs <- get
  when (V.null vs) $ throwError $ messageException "fvIO.empty"
  a <- liftEither $ fromValue (V.head vs)
  put (V.slice 1 (V.length vs - 1) vs)
  pure a

-- | Extract the values from a row.
extractRow :: ValueExtractorIO a -> V.Vector TypedValue -> ConnectionIO a
extractRow = evalStateT

-- | Error if the result is not exactly one row
--
-- extractRow should be called using traverse/for over the result set.
-- Extract single row asserts that the result set consists of exactly
-- one row, then calls extractRow on it.
extractSingleRow ::
     ValueExtractorIO a -> [V.Vector TypedValue] -> ConnectionIO a
extractSingleRow m [v] = extractRow m v
extractSingleRow _ [] = throwError $ messageException "extractSingleRow.empty"
extractSingleRow _ _ =
  throwError $ messageException "extractSingleRow.moreThanOneRow"
