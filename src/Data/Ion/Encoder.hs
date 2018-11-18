module Data.Ion.Encoder where

import Prelude hiding (length)
import Data.Word
import Data.Bits
import Data.ByteString (ByteString, pack, length)
import Data.ByteString.Builder (Builder, word8, byteString, doubleBE)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T


-- TODO: fail if value < 0
varUInt:: Int -> Builder
varUInt i = byteString . pack $ go0 i
    where
        go0:: Int -> [Word8]
        go0 i = if i<0x80 
            then [fromIntegral i .|. 0x80]
            else go (i `shiftR` 7) [fromIntegral (i .&. 0x7f) .|. 0x80]
        go:: Int -> [Word8] -> [Word8]
        go 0 acc = acc
        go v acc = go (v `shiftR` 7) (fromIntegral (v .&. 0x7f) : acc)

uintBS:: Int -> ByteString
uintBS i = pack $ go (i `shiftR` 8) [fromIntegral (i .&. 0xff)]
    where
        go 0 acc = acc
        go v acc = go (v `shiftR` 8) (fromIntegral (v .&. 0xff) : acc)

class ToIon a where
    encode :: a -> Builder

instance ToIon Int where
    encode i = word8 tag <> byteString bs
        where
            bs = uintBS $ if i>=0 then i else (-i)
            tag = (if i>=0 then 0x20 else 0x30) + fromIntegral (length bs)

instance ToIon Bool where
    encode False = word8 0x10
    encode True = word8 0x11

instance ToIon Double where
    encode v = word8 0x48 <> doubleBE v

instance ToIon T.Text where
    encode v = if len<14 
            then word8 (0x80 + fromIntegral len) <> byteString bs
            else word8 0x8E <> varUInt len <> byteString bs
        where
            bs = T.encodeUtf8 v
            len = length bs