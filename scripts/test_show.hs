{-# Language ExistentialQuantification #-}

data Prox = forall a. Show a => Prox a

instance Show Prox where
    show (Prox a) = show a

-- is `x` a value? if yes - what type? is it Int? or Float?
x :: Num a => a
x = 0

main = print [Prox 12, Prox "hello", Prox True, Prox (Just x)]