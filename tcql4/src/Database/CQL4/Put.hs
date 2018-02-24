module Database.CQL4.Put where

import Data.Serialize.Put as P
import Database.CQL4.Types
import Data.Int
import Data.Bits

frameHeader :: FrameVersion -> [FrameFlag] -> StreamID -> OpCode -> Int32 -> P.Put 
frameHeader v fs i c len = do
  case v of
    RequestFrame -> P.putWord8 0x4
    ResponseFrame -> P.putWord8 0x84
  P.putWord8 $ foldr foldFlags 0 fs
  P.putInt32be i
  P.putWord8 $ fromIntegral $ fromEnum c
  P.putInt32be len
  where
    foldFlags f acc =
      (1 `shiftL` fromEnum f) .|. acc
  
  
  
  

