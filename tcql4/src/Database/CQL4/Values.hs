{- |  CQL4 Values - conversion to and from TypedValue

The module provides functions to convert between TypedValue
(the on-the-wire representation) to haskell types.
-}
module Database.CQL4.Values where

import qualified Data.Scientific as Scientific
import qualified Data.Text as T
import Database.CQL4.Exceptions
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
-- XXX needs more cowbell
