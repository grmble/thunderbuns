{-# LANGUAGE TemplateHaskell #-}

{- | UUID represented as a bytestring that preserves order

Given the UUID V1 starting with

	4e2cc205-7aff-11e8-clck-encodedmac
	tlow    -tmid-1thi

The timestamp contained is:

	0x1e87aff4e2cc205

	timehex=0x1e87aff4e2cc205
        xxx x=x/10000000.0
	round $ xxx timehex
	===> 13749502558

	// 15.10.1582
	var greg = Date.UTC(1582,9,15) / 1000
	new Date((ts + greg)*1000)
	==> Date 2018-06-28T18:15:58.000Z

	// just doing everything in milliseconds might be easier

OrderedUUID creates a bytestring starting with the timestamp bits.
The bytestring is then encoded using d64 (similiar to base64, but
it preserves sort order)

On the javascript side, it's just a newtype'd string.
Sort order is presevered, so it can serve as a map key and
the channel entries will be ordered by time.  This also
handles duplicates and record changes.
-}
module Thunderbuns.OrderedUUID where

import Control.Lens.TH (makePrisms)
import Control.Monad.Except (MonadError, liftEither)
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), withText)
import Data.Bits ((.&.))
import qualified Data.ByteString as B
import qualified Data.ByteString.D64 as D64
import qualified Data.Serialize.Get as Get
import qualified Data.Serialize.Put as Put
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (UTCTime(..), addUTCTime, fromGregorian)
import Data.UUID (UUID)
import Data.UUID.Util (UnpackedUUID(..), pack, unpack)
import GHC.Generics (Generic)
import Web.HttpApiData

newtype OrderedUUID =
  OrderedUUID B.ByteString
  deriving (Show, Eq, Ord, Generic)

instance FromJSON OrderedUUID where
  parseJSON = withText "OrderedUUID" (pure . OrderedUUID . T.encodeUtf8)

instance ToJSON OrderedUUID where
  toJSON (OrderedUUID bs) = String $ T.decodeUtf8 bs

$(makePrisms ''OrderedUUID)

instance FromHttpApiData OrderedUUID where
  parseQueryParam s =
    if T.null s
    then fail "empty string - not a uuid"
    else OrderedUUID . T.encodeUtf8 <$> parseQueryParam s

instance ToHttpApiData OrderedUUID where
  toQueryParam (OrderedUUID o) = toQueryParam $ T.decodeUtf8 o

--
-- you would think it would be easier to access
-- the internal 2 int64s, but they use a weird internal
-- order
--
orderedUUID :: UUID -> OrderedUUID
orderedUUID u =
  OrderedUUID $
  D64.encodeBytes' $
  Put.runPut $ do
    let p = unpack u
    Put.putWord16be (time_hi_and_version p)
    Put.putWord16be (time_mid p)
    Put.putWord32be (time_low p)
    Put.putWord8 (clock_seq_hi_res p)
    Put.putWord8 (clock_seq_low p)
    Put.putWord8 (node_0 p)
    Put.putWord8 (node_1 p)
    Put.putWord8 (node_2 p)
    Put.putWord8 (node_3 p)
    Put.putWord8 (node_4 p)
    Put.putWord8 (node_5 p)

toUUID :: OrderedUUID -> UUID
toUUID o = either error id (toUUID' o)

toUUID' :: MonadError String m => OrderedUUID -> m UUID
toUUID' (OrderedUUID d64) = do
  let bs = D64.decodeBytes' d64
  liftEither $ Get.runGet getUUID bs
  where
    getUUID =
      packUUID <$> Get.getWord16be <*> Get.getWord16be <*> Get.getWord32be <*>
      Get.getWord8 <*>
      Get.getWord8 <*>
      Get.getWord8 <*>
      Get.getWord8 <*>
      Get.getWord8 <*>
      Get.getWord8 <*>
      Get.getWord8 <*>
      Get.getWord8
    packUUID th tm tl ch cl n0 n1 n2 n3 n4 n5 =
      pack
        UnpackedUUID
          { time_hi_and_version = th
          , time_mid = tm
          , time_low = tl
          , clock_seq_hi_res = ch
          , clock_seq_low = cl
          , node_0 = n0
          , node_1 = n1
          , node_2 = n2
          , node_3 = n3
          , node_4 = n4
          , node_5 = n5
          }

timestamp :: OrderedUUID -> UTCTime
timestamp (OrderedUUID o) =
  let Right cnsSinceGreg =
        fromIntegral . (0xFFFFFFFFFFFFFFF .&.) <$>
        Get.runGet Get.getWord64be (D64.decodeBytes' o)
      sinceGreg = cnsSinceGreg / 10000000.0
      greg = UTCTime (fromGregorian 1582 10 15) 0
   in addUTCTime sinceGreg greg
