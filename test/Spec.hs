{-# Language OverloadedStrings #-}
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Builder (Builder, toLazyByteString, lazyByteString)
import Data.Text (Text)
import Test.Hspec

import Data.Ion.SymTable (SymTable, sysTable, shared, intern)
import Data.Ion.Encoder hiding (intern)
import Data.Ion.Put (lbBuilder, write)
import qualified Data.Ion.Put as PutS


baseSyms:: [Text]
baseSyms = ["$10", "one", "two"]

baseTable:: SymTable
baseTable = shared baseSyms sysTable

runP :: Put () -> LBS.ByteString
runP p = toLazyByteString . lbBuilder . t3_2 $ PutS.runPut p baseTable
    where
        t3_2 (_,x,_) = x

shouldEncodeTo:: ToIon a => a -> LBS.ByteString -> Expectation
shouldEncodeTo v expected = runP (encode v) `shouldBe` expected

shouldVarUInt:: Int -> LBS.ByteString -> Expectation
shouldVarUInt v expected = runP (putVarUInt v) `shouldBe` expected

main :: IO ()
main = hspec $ do
    describe "encoders" $ do
        describe "baseTable" $ do
            it "resolves to {$10:10, one:11, two:12}" $
                map (\k -> fst $ intern k baseTable) baseSyms `shouldBe` [10, 11, 12]
        describe "varUInt" $ do
            it "zero" $
                0 `shouldVarUInt` "\x80"
            it "one" $
                1 `shouldVarUInt` "\x81"
            it "single byte up to 0x7f" $
                0x7F `shouldVarUInt` "\xFF"
            it "two-bytest since 0x80" $
                0x80 `shouldVarUInt` "\x01\x80"
            it "0x0D*0x80+0x27" $
                (0x0D*0x80+0x27) `shouldVarUInt` "\x0D\xA7"
            it "three bytes" $
                (3*0x80*0x80+5*0x80+7) `shouldVarUInt` "\x03\x05\x87"
        describe "bool" $ do
            it "true" $
                True `shouldEncodeTo` "\x11"
            it "false" $
                False `shouldEncodeTo` "\x10"
        describe "int" $ do
            it "zero" $
                (0::Int) `shouldEncodeTo` "\x21\x00"
            it "one" $
                (1::Int) `shouldEncodeTo` "\x21\x01"
            it "0x82 to single byte" $
                (0x82::Int) `shouldEncodeTo` "\x21\x82"
            it "256*3+7" $
                (256*3+7::Int) `shouldEncodeTo` "\x22\x03\x07"
            it "-1" $
                (-1::Int) `shouldEncodeTo` "\x31\x01"
            it "-(256*256*0x42 + 256*0xEF + 0x9B" $
                (-(256*256*0x42 + 256*0xEF + 0x9B)::Int) `shouldEncodeTo` "\x33\x42\xEF\x9B"
        describe "double" $ do
            it "zero" $
                (0.0::Double) `shouldEncodeTo` "\x48\x00\x00\x00\x00\x00\x00\x00\x00"
            it "one" $
                (1.0::Double) `shouldEncodeTo` "\x48\x3F\xF0\x00\x00\x00\x00\x00\x00"
            it "345" $
                (345::Double) `shouldEncodeTo` "\x48\x40\x75\x90\x00\x00\x00\x00\x00"
            it "-801.5" $
                (-801.5::Double) `shouldEncodeTo` "\x48\xC0\x89\x0C\x00\x00\x00\x00\x00"

        describe "explicit symbol" $ do
            it "one" $
                Sym 1 `shouldEncodeTo` "\x71\x01"
            it "0xFF to single byte" $
                Sym 0xFF `shouldEncodeTo` "\x71\xFF"
            it "0x100 to two bytes" $
                Sym 0x100 `shouldEncodeTo` "\x72\x01\x00"

        describe "text" $ do
            it "short numbers" $
                ("123"::Text) `shouldEncodeTo` "\x83\&123"
            it "short alpha" $
                ("abc.def"::Text) `shouldEncodeTo` "\x87\&abc.def"
            it "short up to length 13" $
                ("1234567890123"::Text) `shouldEncodeTo` "\x8D\&1234567890123"
            it "varUInt length since length 14" $
                ("12345678901234"::Text) `shouldEncodeTo` "\x8E\x8E\&12345678901234"

        describe "lists" $ do
            it "empty list" $
                ([]::[Int]) `shouldEncodeTo` "\xB0"
            it "single bool" $
                [True] `shouldEncodeTo` "\xB1\x11"
            it "two bools" $
                [True, False] `shouldEncodeTo` "\xB2\x11\x10"
            it "list of ints" $
                ([2, 0x10E]::[Int]) `shouldEncodeTo` "\xB5\x21\x02\x22\x01\x0E"
            it "long list" $
                let
                    lst:: [Int]
                    lst = take 70 $ repeat 6
                    bs = foldMap (const "\x21\x06") [1..70]
                    expected = lazyByteString "\xBE\x01\x8C" <> bs
                in
                    lst `shouldEncodeTo` toLazyByteString expected

        describe "annotation" $ do
            it "single" $
                Annotation ["one"] True `shouldEncodeTo` "\xE3\x81\x8B\x11"
            it "pair of annotations for list of int" $
                let
                    dat = [1, 2, 3, 4, 5, 6]::[Int]
                    datLen = 1 + 6*2 -- 13
                    anSyms = ["$10", "two"]
                    annotLen = 1+1 -- 2
                    -- fullLen = 1 + annotLen + datLen
                    fullLen = 1 + 2 + 13 -- 16
                    expected = runP $ write "\xEE\x90\x82" >> mapM_ putVarUInt [10, 12] >> encode dat
                in
                    Annotation anSyms dat `shouldEncodeTo` expected

        describe "IonList" $ do
            it "empty IonList" $
                IonList [] `shouldEncodeTo` "\xB0"
            it "IonList of bool and int" $
                IonList [proxy True, proxy (12::Int)] `shouldEncodeTo` "\xB3\x11\x21\x0C"

        describe "Struct" $ do
            it "empty Struct" $
                Struct [] `shouldEncodeTo` "\xD0"

            it "{one: 64, two: False}" $
                let
                    f1 = ("one", proxy (0x40::Int)) -- "\x8B\x21\x40"
                    f2 = ("two", proxy False) -- "\x8C\x10"
                in
                    Struct [f1, f2] `shouldEncodeTo` "\xD5\x8B\x21\x40\x8C\x10"

            it "{$10: [True]}" $
                let
                    f1 = ("$10", proxy [True]) -- "\x8A\xB1\x11"
                in
                    Struct [f1] `shouldEncodeTo` "\xD3\x8A\xB1\x11"
