{-# Language OverloadedStrings #-}
import Data.Dynamic
import Data.String
import qualified Data.Map as M
import Data.Traversable (traverse, sequence)

newtype Key a = Key String

instance IsString (Key a) where
    fromString = Key

k_id = "id"::Key Int
k_name = "name"::Key String
k_qty = "qty"::Key Int

dynGet :: Typeable a => Key a -> M.Map String Dynamic -> Maybe a
dynGet (Key k) m = 
    case M.lookup k m of
        Nothing -> Nothing
        Just dyn -> fromDynamic dyn

(!:) :: Typeable a => Key a -> a -> (String, Dynamic)
(!:) (Key k) v = (k, toDyn v)

infixl 3 !:

run = print v
    where
        m :: M.Map String Dynamic
        m = M.fromList [k_id !: 13, k_name !: "Alice", k_qty !: 15]
        gt k = dynGet k m
        v = Just (,,) <*> gt k_name <*> gt k_qty <*> gt k_id

main =
    run