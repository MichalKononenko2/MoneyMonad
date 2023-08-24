module Assets (Stock, Portfolio, Quote(..), market, value) where
    import Money
    import Data.Semigroup

    type Stock = String
    type Portfolio = [(Integer, Stock)]

    data Quote = Quote {stock :: Stock, bid :: Money, ask :: Money}

    instance Show Quote where
        show (Quote s b a) = mconcat ["Stock: ", show s, " Bid: ", show b, " Ask: ", show a]


    market :: Stock -> Maybe Quote
    market "NYSE:INFN" = Just (Quote "NYSE:INFN" (Money 5 30) (Money 5 35))
    market "NYSE:GOOG" = Just (Quote "NYSE:GOOG" (Money 100 0) (Money 100 0))
    market _ = Nothing

    value :: Portfolio -> (Stock -> Quote) -> Money
    value [] _ = mempty
    value [(n, s)] m = stimesMonoid n (bid . m) s
    value (p:(ps)) m = (value [p] m) <> (value ps m)