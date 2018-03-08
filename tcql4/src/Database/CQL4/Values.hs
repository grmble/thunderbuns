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
import Data.Time.Clock (UTCTime)
import qualified Data.UUID as U
import qualified Data.Vector as V
import Database.CQL4.Exceptions
import Database.CQL4.Protocol
import Database.CQL4.Types

-- | IsCQLValue is a type that has a Cassandra equivalent
--
-- Note that cassandra is strict about the types it receives -
-- if a table column has type X, the bind parameter must be exactly
-- type X.  E.g. for a 16bit int (Cassandra: smallint), only
-- a 16 bit value will do -  you can't just transmit a 8-bit int.
--
-- So the recommandation is to use the exact equivalent type
-- in your haskell data structure.  E.g. if you use `int`,
-- use Int32 in your data structure.
class IsCQLValue a where
  -- | Construct a typed value for the haskell type a
  toValue :: a -> TypedValue
  -- | Extract a haskell type for the typed value.
  fromValue :: TypedValue -> Either CQLException a

instance IsCQLValue T.Text where
  toValue = TextValue
  fromValue (TextValue t) = Right t
  fromValue x = Left $ fromValueException "Text" x

instance IsCQLValue Integer where
  toValue = VarintValue
  fromValue (VarintValue v) = Right v
  fromValue x = Left $ fromValueException "Integer" x

instance IsCQLValue Int64 where
  toValue = LongValue
  fromValue (LongValue v) = Right (fromIntegral v)
  fromValue x = Left $ fromValueException "Int" x

instance IsCQLValue Int32 where
  toValue = IntValue . fromIntegral
  fromValue (IntValue v) = Right (fromIntegral v)
  fromValue x = Left $ fromValueException "Int32" x

instance IsCQLValue Int16 where
  toValue = SmallintValue . fromIntegral
  fromValue (SmallintValue v) = Right (fromIntegral v)
  fromValue x = Left $ fromValueException "Int16" x

instance IsCQLValue Int8 where
  toValue = TinyintValue . fromIntegral
  fromValue (TinyintValue v) = Right (fromIntegral v)
  fromValue x = Left $ fromValueException "Int8" x

instance IsCQLValue Float where
  toValue = FloatValue
  fromValue (FloatValue v) = Right v
  fromValue x = Left $ fromValueException "Float" x

instance IsCQLValue Double where
  toValue = DoubleValue
  fromValue (DoubleValue d) = Right d
  fromValue x = Left $ fromValueException "Double" x

instance IsCQLValue Scientific.Scientific where
  toValue = DecimalValue
  fromValue (DecimalValue sc) = Right sc
  fromValue x = Left $ fromValueException "Scientific" x

instance IsCQLValue Bool where
  toValue = BoolValue
  fromValue (BoolValue b) = Right b
  fromValue x = Left $ fromValueException "Bool" x

instance IsCQLValue B.ByteString where
  toValue = BlobValue
  fromValue (BlobValue bs) = Right bs
  fromValue x = Left $ fromValueException "ByteString" x

instance IsCQLValue U.UUID where
  toValue = UUIDValue
  fromValue (UUIDValue uu) = Right uu
  fromValue x = Left $ fromValueException "UUID" x

instance IsCQLValue Day where
  toValue = DateValue
  fromValue (DateValue d) = Right d
  fromValue x = Left $ fromValueException "Day" x

instance IsCQLValue UTCTime where
  toValue = TimestampValue
  fromValue (TimestampValue ts) = Right ts
  fromValue x = Left $ fromValueException "UTCTime" x

instance IsCQLValue IPAddress where
  toValue = InetValue
  fromValue (InetValue ip) = Right ip
  fromValue x = Left $ fromValueException "IPAddress" x

-- | Helper monad to extract values from a result set
--
-- It maintains a current slice of the rows values,
-- allowing you to take one value after the other using
-- `extract`.
type ValueExtractorIO = StateT (V.Vector TypedValue) ConnectionIO

-- | Extract the next value from the result row
extract :: IsCQLValue a => ValueExtractorIO a
extract = do
  vs <- get
  when (V.null vs) $ throwError $ messageException "fvIO.empty"
  a <- liftEither $ fromValue (V.head vs)
  put (V.slice 1 (V.length vs - 1) vs)
  pure a

-- | Extract the values from a row.
extractRow :: V.Vector TypedValue -> ValueExtractorIO a -> ConnectionIO a
extractRow = flip evalStateT

-- | Error if the result is not exactly one row
--
-- extractRow should be called using traverse/for over the result set.
-- Extract single row asserts that the result set consists of exactly
-- one row, then calls extractRow on it.
extractSingleRow ::
     [V.Vector TypedValue] -> ValueExtractorIO a -> ConnectionIO a
extractSingleRow [v] m = extractRow v m
extractSingleRow [] _ = throwError $ messageException "extractSingleRow.empty"
extractSingleRow _ _ =
  throwError $ messageException "extractSingleRow.moreThanOneRow"
