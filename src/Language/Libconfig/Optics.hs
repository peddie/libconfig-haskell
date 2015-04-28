{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}

{-|
Module      :  Language.Libconfig.Optics
Copyright   :  (c) Matthew Peddie 2014
License     :  BSD3

Maintainer  :  mpeddie@gmail.com
Stability   :  experimental
Portability :  GHC

Optics for the libconfig types in "Language.Libconfig.Types".


-}

module Language.Libconfig.Optics (
  -- $prismnote
  -- * A note on examples

  -- $setup

  -- * 'Setting'

  -- |
  -- These @Lens@es are first-class references into the parts of a
  -- 'Setting'.
  settingValue
  , settingName
#ifdef DEFINE_PRISMS
  , _nameText
    -- * 'Value'

    -- |
    -- These @Prisms@ provide @Traversal@s for tweaking the relevant
    -- part of a 'Value'.  They can also be turned around to obtain
    -- the embedding into the relevant constructor.
    --
  , _Scalar
  , _Array
  , _List
  , _Group
    -- * 'Scalar'
    --
    -- |
    -- These @Prisms@ provide @Traversal@s for tweaking the relevant
    -- part of a 'Scalar'.  They can also be turned around to obtain
    -- the embedding into the relevant constructor.
    --
  , _Boolean
  , _Integer
  , _Integer64
  , _Hex
  , _Hex64
  , _Float
  , _String
#endif
  ) where

#ifdef DEFINE_PRISMS
import Data.Profunctor
import Control.Applicative
#endif

import Language.Libconfig.Types

#ifndef DEFINE_PRISMS
-- $prismnote
--
-- This package was built with the non-default cabal configuration flag
-- @-f -prisms@ and hence contains no @Prism@s for the 'Value' and
-- 'Scalar' sum types.  If you want @Prism@s, please rebuild this package
-- with the default configuration value of @-f prisms@.  Rebuilding will
-- incur a dependency on the @profunctors@ package (but __not__ on the
-- @lens@ package).
#else
-- $prismnote
#endif

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

type Lens' s a = Lens s s a a

#ifdef DEFINE_PRISMS
-- $setup
-- In order to run the usage examples in @ghci@, some setup is required:
--
-- >>> :set -XOverloadedStrings
-- >>> import Control.Lens
-- >>> :set -XCPP
-- >>> :set -DDEFINE_PRISMS
-- >>> let Just asset = textToName "asset"
-- >>> let Just price = textToName "price"
#else
-- $setup
-- In order to run the usage examples in @ghci@, some setup is required:
--
-- >>> :set -XOverloadedStrings
-- >>> import Control.Lens
-- >>> let Just asset = textToName "asset"
-- >>> let Just price = textToName "price"
#endif

-- |
-- >>> (asset := Scalar (String "butts")) ^. settingValue
-- Scalar (String "butts")
--
-- >>> (asset := Scalar (String "butts")) & settingValue .~ Scalar (Float 22.2)
-- "asset" := Scalar (Float 22.2)
settingValue :: Lens' Setting Value
settingValue f (name := value) = fmap (\value' -> name := value') (f value)

-- |
-- >>> (asset := Scalar (String "butts")) ^. settingName
-- "asset"
--
-- >>> let Just shake = textToName "shake"
-- >>> (asset := Scalar (String "butts")) & settingName .~ shake
-- "shake" := Scalar (String "butts")
settingName :: Lens' Setting Name
settingName f (name := value) = fmap (\name' -> name' := value) (f name)



#ifdef DEFINE_PRISMS
type Prism s t a b = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)

type Prism' s a = Prism s s a a

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt seta = dimap seta (either pure (fmap bt)) . right'

-- |
-- Here is a 'Prism'' for accessing the string value of a 'Name'.
--
-- >>> _nameText # asset
-- "asset"
--
-- >>> "butts" ^? _nameText
-- Just "butts"
-- >>> :t ("butts" :: Text) ^? _nameText
-- ("butts" :: Text) ^? _nameText :: Maybe Name
--
-- __N.B.__: '_nameText' is partial in the opposite direction to the
-- usual 'Prism's for sum types (e.g. @_Left@, @_Just@).  This makes
-- it a bit puzzling to compose.  We use @re _nameText@ with @view@:
--
-- >>> (asset := Scalar (String "butts")) ^. settingName . re _nameText
-- "asset"
--
-- I don't know how to get it to compose properly for setting.
_nameText :: Prism' Text Name
_nameText = prism nameToText $ \x -> case textToName x of
                                      Nothing -> Left x
                                      Just nm -> Right nm

-- |
-- >>> Scalar (String "butts") ^? _Scalar
-- Just (String "butts")
--
-- >>> (asset := Scalar (String "butts")) & settingValue . _Scalar . _String .~ "money"
-- "asset" := Scalar (String "money")
--
-- >>> _Scalar # String "butts"
-- Scalar (String "butts")
_Scalar :: Prism' Value Scalar
_Scalar = prism Scalar $ \x -> case x of
                                (Scalar s) -> Right s
                                _          -> Left x

-- |
-- >>> Array [String "butts"] ^? _Array
-- Just [String "butts"]
--
-- >>> (asset := Array [String "butts"]) & settingValue . _Array . traverse . _String .~ "money"
-- "asset" := Array [String "money"]
_Array :: Prism' Value Array
_Array = prism Array $ \x -> case x of
                              (Array a) -> Right a
                              _         -> Left x

-- |
-- >>> Group [asset := Scalar (String "butts"), price := Scalar (Float 22.2)] ^? _Group . ix 0
-- Just ("asset" := Scalar (String "butts"))
--
-- >>> Group [asset := Scalar (String "butts"), price := Scalar (Float 22.2)] & _Group . traverse . settingValue . _Scalar . _Float %~ (*2)
-- Group ["asset" := Scalar (String "butts"),"price" := Scalar (Float 44.4)]
_Group :: Prism' Value Group
_Group = prism Group $ \x -> case x of
                              (Group a) -> Right a
                              _         -> Left x
-- |
-- >>> List [Scalar (String "butts"), Scalar (Float 22.2)] ^? _List . ix 0
-- Just (Scalar (String "butts"))
--
-- >>> List [Scalar (String "butts"), Scalar (Float 22.2)] & _List . traverse . _Scalar . _Float %~ (*2)
-- List [Scalar (String "butts"),Scalar (Float 44.4)]
_List :: Prism' Value List
_List = prism List $ \x -> case x of
                            (List a) -> Right a
                            _        -> Left x

-- |
-- >>> Boolean False ^? _Boolean
-- Just False
--
-- >>> Scalar (Boolean False) & _Scalar . _Boolean %~ not
-- Scalar (Boolean True)
_Boolean :: Prism' Scalar Bool
_Boolean = prism Boolean $ \x -> case x of
                                  (Boolean b) -> Right b
                                  _           -> Left x

-- |
-- >>> Integer 22 ^? _Integer
-- Just 22
--
-- >>> Scalar (Integer 22) & _Scalar . _Integer %~ (*2)
-- Scalar (Integer 44)
_Integer :: Prism' Scalar Int32
_Integer = prism Integer $ \x -> case x of
                                  (Integer b) -> Right b
                                  _           -> Left x
-- |
-- >>> Integer64 2222222222 ^? _Integer64
-- Just 2222222222
--
-- >>> Scalar (Integer64 2222222222) & _Scalar . _Integer64 %~ (*2)
-- Scalar (Integer64 4444444444)
_Integer64 :: Prism' Scalar Int64
_Integer64 = prism Integer64 $ \x -> case x of
                                      (Integer64 b) -> Right b
                                      _             -> Left x

-- |
-- >>> Hex 22 ^? _Hex
-- Just 22
--
-- >>> Scalar (Hex 22) & _Scalar . _Hex %~ (*2)
-- Scalar (Hex 44)
_Hex :: Prism' Scalar Word32
_Hex = prism Hex $ \x -> case x of
                          (Hex b) -> Right b
                          _       -> Left x

-- |
-- >>> Hex64 2222222222 ^? _Hex64
-- Just 2222222222
--
-- >>> Scalar (Hex64 2222222222) & _Scalar . _Hex64 %~ (*2)
-- Scalar (Hex64 4444444444)
_Hex64 :: Prism' Scalar Word64
_Hex64 = prism Hex64 $ \x -> case x of
                              (Hex64 b) -> Right b
                              _         -> Left x

-- |
-- >>> Float 22.22 ^? _Float
-- Just 22.22
--
-- >>> Scalar (Float 22.22) & _Scalar . _Float %~ (*2)
-- Scalar (Float 44.44)
_Float :: Prism' Scalar Double
_Float = prism Float $ \x -> case x of
                              (Float b) -> Right b
                              _         -> Left x

-- |
-- >>> String "butts" ^? _String
-- Just "butts"
--
-- >>> Float 22.22 ^? _String
-- Nothing
--
-- >>> import Data.Monoid ((<>))
-- >>> Scalar (String "butts") & _Scalar . _String %~ ("hello " <>)
-- Scalar (String "hello butts")
_String :: Prism' Scalar Text
_String = prism String $ \x -> case x of
                              (String b) -> Right b
                              _          -> Left x

#endif
