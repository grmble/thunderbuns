{- | CQL4 API Types

Types used in the externally visible API.

This can be imported from the internal modules.
Protocol.hs has the types that need the internal modules.
-}
module Database.CQL4.Types where

-- | Cassandra consistency level
data Consistency
  = Any
  | One
  | Two
  | Three
  | Quorum
  | All
  | LocalQuorum
  | EachQuorum
  | Serial
  | LocalSerial
  | LocalOne
  deriving (Show, Eq, Enum)
