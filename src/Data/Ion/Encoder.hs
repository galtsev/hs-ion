module Data.Ion.Encoder where

import Prelude hiding (length)
import Data.Word
import Data.Bits
import Data.ByteString (ByteString, pack, length)
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Builder (Builder, word8, byteString, lazyByteString, doubleBE, toLazyByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

builderLen:: Builder -> Int
builderLen = fromIntegral . LBS.length . toLazyByteString

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

instance ToIon a => ToIon [a] where
    encode [] = word8 0xB0
    encode v = if len<14
            then word8 (0xB0 + fromIntegral len) <> bs
            else word8 0xBE <> varUInt len <> bs
        where
            bs = foldMap encode $ v
            len = builderLen bs

newtype Symbol = Sym Int

instance Enum Symbol where
    toEnum i = Sym i
    fromEnum (Sym v) = v

instance ToIon Symbol where
    encode (Sym v) = word8 (0x70 + fromIntegral len) <> byteString bs
        where
            bs = uintBS v
            len = length bs

data Annotation a = Annotation [Symbol] a

instance ToIon a => ToIon (Annotation a) where
    encode (Annotation syms body) = if len<14
            then word8 (0xE0 + toEnum len) <> payload
            else word8 0xEE <> varUInt len <> payload
        where
            bbs = encode body
            an = foldMap (varUInt . fromEnum) syms
            anLen = builderLen an
            anLenBs = varUInt anLen
            len = builderLen anLenBs + anLen + builderLen bbs
            payload = anLenBs <> an <> bbs
