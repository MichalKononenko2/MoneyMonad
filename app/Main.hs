module Main (main) where

import Money
import Assets
import Data.Maybe

port :: Portfolio
port = [(1, "NYSE:INFN"), (2, "NYSE:GOOG")]
    
main :: IO ()
main = putStrLn (show (value port (fromMaybe (Quote "" (Money 0 0) (Money 0 0)) . market)))