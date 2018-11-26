{-# Language OverloadedStrings #-}
module Data.Ion.Encoder where

import Prelude hiding (length)
import Data.Word
import Data.Bits
import Data.ByteString (ByteString, pack, length)
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Builder (Builder, word8, byteString, lazyByteString, doubleBE, toLazyByteString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Control.Monad ((>=>))

import Data.Ion.Put hiding (Put, runPut)
import qualified Data.Ion.Put as PutS
import qualified Data.Ion.SymTable as SymT

type Put = PutS.Put SymT.SymTable

ivm:: Put ()
ivm = write "\xE0\x01\x00\xEA"

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

class ToIon a where
    encode :: a -> Put ()

intern:: Text -> Put Int
intern t = do
    st <- getState
    let (idx, nst) = SymT.intern t st
    putState nst
    return idx

tagged:: Word8 -> Put () -> Put ()
tagged tag p = prefix hdr p
    where
        hdr len = if len<14
            then w8 (tag + toEnum len)
            else w8 (tag + 14) >> putVarUInt len

instance ToIon Int where
    encode i = if i>= 0
        then tagged 0x20 $ putInt i
        else tagged 0x30 $ putInt (-i)

instance ToIon Bool where
    encode False = w8 0x10
    encode True = w8 0x11

instance ToIon Double where
    encode v = w8 0x48 >> writeLazy bs
        where
            bs = toLazyByteString . doubleBE $ v

instance ToIon Text where
    encode = tagged 0x80 . write . T.encodeUtf8

instance ToIon a => ToIon [a] where
    encode = tagged 0xB0 . mapM_ encode

newtype Sym = Sym Int

instance Enum Sym where
    toEnum i = Sym i
    fromEnum (Sym v) = v

instance ToIon Sym where
    encode = tagged 0x70 . putInt . fromEnum

newtype Symbol = Symbol Text

symToText :: Symbol -> Text
symToText (Symbol t) = t

instance ToIon Symbol where
    encode = tagged 0x70 . (intern . symToText >=> putInt)

data Annotation a = Annotation [Text] a

instance ToIon a => ToIon (Annotation a) where
    encode (Annotation syms body) = tagged 0xE0 payload
        where
            an = mapM_ (intern >=> putVarUInt) syms
            -- payload = putVarUInt (getLen an) >> an >> encode body
            payload = prefix putVarUInt an >> encode body


newtype IonProxy = Prox (Put ())

runProxy:: IonProxy -> Put ()
runProxy (Prox p) = p

instance ToIon IonProxy where
    encode = runProxy

proxy:: ToIon a => a -> IonProxy
proxy = Prox . encode

newtype Struct = Struct [(Text, IonProxy)]


instance ToIon Struct where
    encode (Struct items) = tagged 0xD0 $ mapM_ fld items
        where
            fld (k, prox) = (intern >=> putVarUInt $ k) >> runProxy prox

newtype IonList = IonList [IonProxy]

instance ToIon IonList where
    encode (IonList items) = tagged 0xB0 $ mapM_ runProxy items


--

runPut:: SymT.SymTable -> Put () -> (SymT.SymTable, Builder)
runPut tbl p = (flushedSymTable, encodeSyms dirty <> lbBuilder lb)
    where
        (s, lb, _) = PutS.runPut tbl p
        (flushedSymTable, dirty) = SymT.flush s
        encodeSyms :: [Text] -> Builder
        encodeSyms [] = mempty
        encodeSyms syms = lbBuilder lbs
            where
                (_, lbs, _) = PutS.runPut SymT.sysTable $ encode stPut
                stPut = Annotation ["$ion_symbol_table"] $ Struct
                    [ ("imports", proxy (Symbol "$ion_symbol_table"))
                    , ("symbols", proxy syms)
                    ]
