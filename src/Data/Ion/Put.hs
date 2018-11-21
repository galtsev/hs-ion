module Data.Ion.Put where

import Prelude hiding (length)
import Data.Word
import Data.Bits
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString (ByteString, pack, length, singleton)
import Data.ByteString.Builder (Builder, byteString, lazyByteString)

newtype LenBuilder = LenBuilder (Builder, Int)

instance Semigroup LenBuilder where
    LenBuilder (a, alen) <> LenBuilder (b, blen) = LenBuilder (a <> b, alen + blen)

instance Monoid LenBuilder where
    mempty = LenBuilder (mempty, 0)

lbLen :: LenBuilder -> Int
lbLen (LenBuilder (_,len)) = len

newtype Put a = Put (LenBuilder, a)

runPut:: Put () -> Builder
runPut (Put (LenBuilder (body, _), ())) = body

-- runPut:: Put a -> (Builder, a)
-- runPut (Put x) = x

instance Functor Put where
    fmap f (Put (b, a)) = Put (b, f a)

instance Applicative Put where
    pure v  = Put (mempty, v)
    Put (fb, f) <*> Put (vb, v) = Put (fb <> vb, f v)

instance Monad Put where
    Put (ba, a) >>= f =
        let
            (Put (bb, b)) = f a
        in
            Put (ba <> bb, b)

write:: ByteString -> Put ()
write bs = Put (LenBuilder (byteString bs, length bs), ())

writeLazy:: LBS.ByteString -> Put ()
writeLazy bs = Put (LenBuilder (lazyByteString bs, fromEnum $ LBS.length bs), ())

w8 :: Word8 -> Put ()
w8 = write . singleton

getLen:: Put a -> Int
getLen (Put (lb, _)) = lbLen lb

putInt:: Int -> Put ()
putInt i = write . pack $ go (i `shiftR` 8) [fromIntegral (i .&. 0xff)]
    where
        go 0 acc = acc
        go v acc = go (v `shiftR` 8) (fromIntegral (v .&. 0xff) : acc)

putVarUInt:: Int -> Put ()
putVarUInt i = write . pack $ go0 i
    where
        go0:: Int -> [Word8]
        go0 i = if i<0x80 
            then [fromIntegral i .|. 0x80]
            else go (i `shiftR` 7) [fromIntegral (i .&. 0x7f) .|. 0x80]
        go:: Int -> [Word8] -> [Word8]
        go 0 acc = acc
        go v acc = go (v `shiftR` 7) (fromIntegral (v .&. 0x7f) : acc)