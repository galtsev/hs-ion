{-# Language OverloadedStrings #-}
module Data.Ion.Encoder where

import Prelude hiding (length)
import Data.Word
import Data.Bits
import Data.ByteString (ByteString, pack, length)
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Builder (Builder, word8, byteString, lazyByteString, doubleBE, toLazyByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.Ion.Put

ivm:: Put ()
ivm = write "\xE0\x01\x00\xEA"


class ToIon a where
    encode :: a -> Put ()

tagged:: Word8 -> Put () -> Put ()
tagged tag p = hdr >> p
    where
        len = getLen p
        hdr = if len<14
            then w8 (tag + toEnum len)
            else w8 (tag + 14) >> putVarUInt len

instance ToIon Int where
    encode i = tagged tag $ putInt v
        where
            (tag, v) = if i>=0
                then (0x20, i)
                else (0x30, (-i))

instance ToIon Bool where
    encode False = w8 0x10
    encode True = w8 0x11

instance ToIon Double where
    encode v = w8 0x48 >> writeLazy bs
        where
            bs = toLazyByteString . doubleBE $ v

instance ToIon T.Text where
    encode = tagged 0x80 . write . T.encodeUtf8

instance ToIon a => ToIon [a] where
    encode v = tagged 0xB0 $ mapM_ encode v

newtype Symbol = Sym Int

instance Enum Symbol where
    toEnum i = Sym i
    fromEnum (Sym v) = v

instance ToIon Symbol where
    encode (Sym v) = tagged 0x70 $ putInt v

data Annotation a = Annotation [Symbol] a

instance ToIon a => ToIon (Annotation a) where
    encode (Annotation syms body) = tagged 0xE0 payload
        where
            an = mapM_ (putVarUInt . fromEnum) syms
            payload = putVarUInt (getLen an) >> an >> encode body


newtype IonProxy = Prox (Put ())

runProxy:: IonProxy -> Put ()
runProxy (Prox p) = p

instance ToIon IonProxy where
    encode = runProxy

proxy:: ToIon a => a -> IonProxy
proxy a = Prox (encode a)

newtype Struct = Struct [(Symbol, IonProxy)]


instance ToIon Struct where
    encode (Struct items) = tagged 0xD0 $ mapM_ fld items
        where
            fld (Sym k, prox) = putVarUInt k >> runProxy prox

newtype IonList = IonList [IonProxy]

instance ToIon IonList where
    encode (IonList items) = tagged 0xB0 $ mapM_ runProxy items
