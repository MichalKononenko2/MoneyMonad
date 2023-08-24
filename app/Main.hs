module Main (main) where

import Assets
import Data.Maybe

port :: Portfolio
port = [(1, "NYSE:INFN"), (2, "NYSE:GOOG")]
    
main :: IO ()
main = putStrLn (show (value port (fromMaybe (Quote "" (mempty) (mempty)) . market)))