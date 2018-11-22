{-# Language OverloadedStrings #-}
module Data.Ion.SymTable where

import Data.Text (Text)
import qualified Data.Map as M

data SymTable = SymTable {
    stMap:: M.Map Text Int,
    stNext:: Int
    }

empty :: SymTable
empty = SymTable M.empty 1

intern :: Text -> SymTable -> (Int, SymTable)
intern sym tbl@SymTable{stMap=m, stNext=next} = case M.lookup sym m of
    Just v -> (v, tbl)
    Nothing -> (next, tbl{stMap=M.insert sym next m, stNext=next+1})

shared :: [Text] -> SymTable -> SymTable
shared syms tbl = tbl{
        stMap = foldl appnd (stMap tbl) $ zip [nxt..] syms,
        stNext = nxt + length syms
        }
    where
        nxt = stNext tbl
        appnd t (idx, k) = M.insert k idx t

sysSyms :: [Text]
sysSyms = [
    "$ion",
    "$ion_1_0",
    "$ion_symbol_table",
    "name",
    "version",
    "imports",
    "symbols",
    "max_id",
    "$ion_shared_symbol_table"
    ]

sysTable = shared sysSyms empty
