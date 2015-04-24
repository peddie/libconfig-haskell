module Main where

import Test.DocTest

-- Bit of a hack, this . . .
main :: IO ()
main = doctest [ "-lconfig", "dist/build/Language/Libconfig.hs" ]
