{-# Language OverloadedStrings #-}

import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Builder (Builder, toLazyByteString, byteString)
import qualified Data.Text as T
import Data.Ion.Encoder
import qualified Data.Ion.SystemTable as ST


-- cheap generator of pseudo-random ints
-- take 20 gen -> [0,73,1022,47,684,773,906,587,536,897,470,39,580,445,738,451,816,441,686,799]
gen:: [Int]
gen = iterate (\v-> mod (v*13+73) 1024) 0

dumpAll:: ToIon a => (Int -> a) -> IO ()
dumpAll f = LBS.writeFile "data.ion" $ toLazyByteString $ byteString ivm <> body
    where
        body = foldMap (encode . f) $ take 20 gen

genText:: Int -> T.Text
genText = ("hs"<>) . T.pack . show

genPair:: Int -> IonList
genPair v = IonList [proxy v, proxy . ("s-"<>) . T.pack . show $ v]

genStruct:: Int -> Struct
genStruct v = Struct [
    (Sym 10, proxy v),
    (Sym 12, proxy . ("str-"<>) . T.pack . show $ v),
    (Sym 11, proxy (v<500::Bool))
    ]

buildStructs:: Builder
buildStructs = byteString ivm <> encode symTable <> foldMap (encode . genStruct) (take 20 gen)
    where
        syms = ["qty", "flag", "name"]::[T.Text]
        symTable = Annotation [ST.ion_symbol_table] $ Struct [(ST.symbols, proxy syms)]

writeStructs:: IO ()
writeStructs = LBS.writeFile "data.ion" $ toLazyByteString buildStructs

main:: IO ()
main =
    writeStructs
    -- dumpAll 
        -- id
        -- genText
        -- genPair
