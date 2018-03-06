{- |  CQL4 Values - conversion to and from TypedValue

The module provides functions to convert between TypedValue
(the on-the-wire representation) to haskell types.
-}
module Database.CQL4.Values where

import Control.Monad (when)
import Control.Monad.Except (liftEither, throwError)
import Control.Monad.State (StateT, evalStateT, get, put)
import qualified Data.Scientific as Scientific
import qualified Data.Text as T
import qualified Data.UUID as U
import qualified Data.Vector as V
import Database.CQL4.Exceptions
import Database.CQL4.Protocol
import Database.CQL4.Types

class IsCQLValue a where
  toValue :: a -> TypedValue
  fromValue :: TypedValue -> Either CQLException a

instance IsCQLValue T.Text where
  toValue = TextValue
  fromValue (TextValue t) = Right t
  fromValue v@NullValue = Left $ fromValueException "null->Text" v
  fromValue x = Left $ fromValueException "Text" x

instance IsCQLValue Integer where
  toValue = VarintValue
  fromValue (VarintValue v) = Right v
  fromValue (LongValue v) = Right (toInteger v)
  fromValue (SmallintValue v) = Right (toInteger v)
  fromValue (TinyintValue v) = Right (toInteger v)
  fromValue v@NullValue = Left $ fromValueException "null->Integer" v
  fromValue x = Left $ fromValueException "Integer" x

instance IsCQLValue Scientific.Scientific where
  toValue = DecimalValue
  fromValue (VarintValue v) = Right $ Scientific.scientific v 0
  fromValue (LongValue v) = Right $ Scientific.scientific (toInteger v) 0
  fromValue (SmallintValue v) = Right $ Scientific.scientific (toInteger v) 0
  fromValue (TinyintValue v) = Right $ Scientific.scientific (toInteger v) 0
  fromValue v@NullValue = Left $ fromValueException "null->Integer" v
  fromValue x = Left $ fromValueException "Scientific" x

instance IsCQLValue U.UUID where
  toValue = UUIDValue
  fromValue (UUIDValue uu) = Right uu
  fromValue x = Left $ fromValueException "UUID" x

-- XXX need more types ...
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
