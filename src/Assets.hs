module Assets (Stock, Portfolio, Quote(..), market, value) where
    import Money
    import Data.Semigroup

    type Stock = String
    type Portfolio = [(Integer, Stock)]

    data Quote = Quote {stock :: Stock, bid :: Money, ask :: Money}

    instance Show Quote where
        show = mconcat . (interleave ["Stock: ", " Bid: ", " Ask: "] (show <$> [stock, bid, ask]))

    market :: Stock -> Maybe Quote
    market "NYSE:INFN" = Just (Quote "NYSE:INFN" (toMoney 5 30) (toMoney 5 35))
    market "NYSE:GOOG" = Just (Quote "NYSE:GOOG" (toMoney 100 0) (toMoney 100 0))
    market _ = Nothing

    value :: Portfolio -> (Stock -> Quote) -> Money
    value [] _ = mempty
    value [(n, s)] m = stimesMonoid n (bid . m) s
    value (p:(ps)) m = (value [p] m) <> (value ps m)