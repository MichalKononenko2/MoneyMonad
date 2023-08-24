module Money (Money(..), dollars, cents, toMoney) where
    import GHC.Float
    import Text.Printf
    
    data Money = TotalCents Integer
    
    dollars :: Money -> Integer
    dollars (TotalCents a)
        |  a < 0 = - (div a (-100))
        |  otherwise = div a 100
    
    cents :: Money -> Integer
    cents (TotalCents a)
        |  a < 0 = - (mod a 100)
        |  otherwise = mod a 100
    
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
        show (TotalCents a) = "$ " ++ (printf "%.2f" (rationalToFloat a 100))
