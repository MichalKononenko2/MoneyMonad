module Money (Money(..), Dollars, Cents) where
    type Dollars = Integer
    type Cents = Integer

    data Money = Money Dollars Cents

    instance Semigroup Money where
        (Money a x) <> (Money b y) = Money (a + b + (div (x + y) 100)) (mod (x + y) 100)

    instance Monoid Money where
        mempty = Money 0 0 

    instance Show Money where
        show (Money a b) 
            | b < 10 =  "$ " ++ (show a) ++ ".0" ++ (show b)
            | otherwise = "$ " ++ (show a) ++ "." ++ (show b)
