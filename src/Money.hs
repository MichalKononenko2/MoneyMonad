module Money (Money(..), dollars, cents, toMoney) where
    data Money = TotalCents Integer
    
    dollars :: Money -> Integer
    dollars (TotalCents a) = div a 100
    
    cents :: Money -> Integer
    cents (TotalCents a) = mod a 100
    
    toMoney :: Integer -> Integer -> Money
    toMoney a b = TotalCents (a * 100 + b)
    
    instance Semigroup Money where
        (TotalCents x) <> (TotalCents y) = TotalCents (x + y)

    instance Monoid Money where
        mempty = TotalCents 0
        
    instance Eq Money where
        (TotalCents a) == (TotalCents b) = a == b
        
    instance Ord Money where
        (TotalCents a) <= (TotalCents b) = a <= b
        
    instance Show Money where
        show a
            | cents a < 10 =  "$ " ++ (show (dollars a)) ++ ".0" ++ (show (cents a))
            | otherwise = "$ " ++ (show (dollars a)) ++ "." ++ (show (cents a))
