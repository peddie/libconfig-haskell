{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE CPP #-}

module Main where

import Test.DocTest

-- Bit of a hack, this . . .
main :: IO ()
main = do
  doctest [ "-lconfig" , "dist/build/Language/Libconfig/Bindings.hs" ]
  doctest [ "-lconfig" , "-isrc", "src/Language/Libconfig/Types.hs" ]
  doctest [ "-lconfig" , "-isrc", "-idist/build", "src/Language/Libconfig/Decode.hs" ]
  doctest [ "-lconfig" , "-isrc", "-idist/build", "src/Language/Libconfig/Encode.hs" ]
#ifdef DEFINE_PRISMS
  doctest [ "-lconfig" , "-isrc", "-DDEFINE_PRISMS", "src/Language/Libconfig/Optics.hs" ]
#else
#endif
