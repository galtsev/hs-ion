module Data.Ion.Put where

import Prelude hiding (length)
import Data.Word
import Data.Bits
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString (ByteString, length, singleton)
import Data.ByteString.Builder (Builder, byteString, lazyByteString)

newtype LenBuilder = LenBuilder (Builder, Int)

instance Semigroup LenBuilder where
    LenBuilder (a, alen) <> LenBuilder (b, blen) = LenBuilder (a <> b, alen + blen)

instance Monoid LenBuilder where
    mempty = LenBuilder (mempty, 0)

lbLen :: LenBuilder -> Int
lbLen (LenBuilder (_,len)) = len

lbBuilder :: LenBuilder -> Builder
lbBuilder (LenBuilder (b,_)) = b

newtype Put s a = Put (s -> (s, LenBuilder, a))

runPut::Put s a -> s -> (s, LenBuilder, a)
runPut (Put p) s = p s

instance Functor (Put s) where
    fmap f (Put g) = Put $ \s -> 
        let (s1, b, a) = g s in (s1, b, f a)

instance Applicative (Put s) where
    pure v  = Put $ \s -> (s, mempty, v)
    Put f <*> Put v = Put $ \s ->
        let
            (s1, fb, f1) = f s
            (s2, vb, v1) = v s1
        in
            (s2, fb <> vb, f1 v1)

instance Monad (Put s) where
    Put a >>= f = Put $ \s ->
        let
            (s1, ab, a1) = a s
            (Put ff) = f a1
            (s2, bb, b) = ff s1
        in
            (s2, ab <> bb, b)

getState:: Put s s
getState = Put $ \s -> (s, mempty, s)

putState:: s -> Put s ()
putState s = Put $ \_ -> (s, mempty, ())

write:: ByteString -> Put s ()
write bs = Put $ \s -> (s, LenBuilder (byteString bs, length bs), ())

writeLazy:: LBS.ByteString -> Put s ()
writeLazy bs = Put $ \s -> (s, LenBuilder (lazyByteString bs, fromEnum $ LBS.length bs), ())

w8 :: Word8 -> Put s ()
w8 = write . singleton

prefix :: (Int -> Put s ()) -> Put s () ->  Put s ()
prefix hdr body = Put $ \s ->
    let
        (s1, bb, _) = runPut body s
        (s2, hb, _) = runPut (hdr . lbLen $ bb) s1
    in
        (s2, hb <> bb, ())

