{-# OPTIONS_GHC -Wall #-}

{-|
Module      :  Language.Libconfig
Copyright   :  (c) Matthew Peddie 2014
License     :  BSD3

Maintainer  :  mpeddie@gmail.com
Stability   :  experimental
Portability :  GHC

Top-level tools for working with the
<http://www.hyperrealm.com/libconfig/ libconfig>
configuration file library.  Please see the
<http://www.hyperrealm.com/libconfig/libconfig_manual.html libconfig manual>
for documentation on the underlying model of the libconfig API.

This module re-exports everything you need for basic libconfig usage
from Haskell.  If you want to do something special and don't mind a
bunch of mutation, you should import the low-level bindings:

> import qualified Language.Libconfig.Bindings as C

-}

module Language.Libconfig (
  module L
  ) where

import Language.Libconfig.Types as L
import Language.Libconfig.Decode as L
import Language.Libconfig.Encode as L
import Language.Libconfig.Optics as L
