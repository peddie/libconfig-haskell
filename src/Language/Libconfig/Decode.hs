{-# OPTIONS_GHC -Wall #-}

{-|
Module      :  Language.Libconfig.Decode
Copyright   :  (c) Matthew Peddie 2014
License     :  BSD3

Maintainer  :  mpeddie@gmail.com
Stability   :  experimental
Portability :  GHC

Converting libconfig native data into friendly Haskell structures from
"Language.Libconfig.Types".

-}

module Language.Libconfig.Decode (
  -- * Decoding libconfig native data
  decode
  , decodeFrom
  ) where

import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe

import qualified Data.Text as T (pack)

import Language.Libconfig.Types
import Language.Libconfig.Bindings (ConfigType(..))
import qualified Language.Libconfig.Bindings as C

-- TODO(MP): Properly convert integral types depending on the setting
-- format.

toScalar :: C.Setting -> MaybeT IO Scalar
toScalar s = MaybeT $ C.configSettingType s >>= go
  where
    go :: ConfigType -> IO (Maybe Scalar)
    go IntType = Just . Integer . fromIntegral <$> C.configSettingGetInt s
    go Int64Type = Just . Integer64 . fromIntegral <$> C.configSettingGetInt64 s
    go FloatType = Just . Float <$> C.configSettingGetFloat s
    go BoolType = Just . Boolean <$> C.configSettingGetBool s
    go StringType = Just . String . T.pack <$> C.configSettingGetString s
    go _ = return Nothing

toList :: C.Setting -> MaybeT IO List
toList s = liftIO (C.configSettingType s) >>= go
  where
    go :: ConfigType -> MaybeT IO List
    go ListType = do
      l <- liftIO $ C.configSettingLength s
      mapM (\i -> MaybeT (C.configSettingGetElem s i) >>= toValue) [0 .. l - 1]
    go _ = MaybeT $ return Nothing

toArray :: C.Setting -> MaybeT IO Array
toArray s = liftIO (C.configSettingType s) >>= go
  where
    go :: ConfigType -> MaybeT IO Array
    go ArrayType = do
      l <- liftIO $ C.configSettingLength s
      mapM (\i -> MaybeT (C.configSettingGetElem s i) >>= toScalar) [0 .. l - 1]
    go _ = MaybeT $ return Nothing

toGroup :: C.Setting -> MaybeT IO Group
toGroup s = liftIO (C.configSettingType s) >>= go
  where
    go :: ConfigType -> MaybeT IO Group
    go GroupType = do
      l <- liftIO $ C.configSettingLength s
      mapM (\i -> MaybeT (C.configSettingGetElem s i) >>= decodeSetting) [0 .. l - 1]
    go _ = MaybeT $ return Nothing

toValue :: C.Setting -> MaybeT IO Value
toValue s = liftIO (C.configSettingType s) >>= go
  where
    go :: ConfigType -> MaybeT IO Value
    go NoneType = MaybeT $ return Nothing
    go ListType = List <$> toList s
    go ArrayType = Array <$> toArray s
    go GroupType = Group <$> toGroup s
    go _ = Scalar <$> toScalar s

decodeSetting :: C.Setting -> MaybeT IO Setting
decodeSetting s = liftIO (C.configSettingType s) >>= go
  where
    go NoneType = MaybeT $ return Nothing
    go _ =
      (:=) <$> fmap T.pack (MaybeT $ C.configSettingName s) <*> toValue s

-- | Convert a native 'C.Configuration' into a top-level 'Group' of
-- 'Setting's.
--
-- >>> Just conf <- C.configNew "test/test.conf"
-- >>> decode conf
-- Just ["version" := Scalar (String "1.0"),"application" := Group ["window" := Group ["title" := Scalar (String "My Application"),"size" := Group ["w" := Scalar (Integer 640),"h" := Scalar (Integer 480)],"pos" := Group ["x" := Scalar (Integer 350),"y" := Scalar (Integer 250)]],"list" := List [List [Scalar (String "abc"),Scalar (Integer 123),Scalar (Boolean True)],Scalar (Float 1.234),List []],"books" := List [Group ["title" := Scalar (String "Treasure Island"),"author" := Scalar (String "Robert Louis Stevenson"),"price" := Scalar (Float 29.95),"qty" := Scalar (Integer 5)],Group ["title" := Scalar (String "Snow Crash"),"author" := Scalar (String "Neal Stephenson"),"price" := Scalar (Float 9.99),"qty" := Scalar (Integer 8)]],"misc" := Group ["pi" := Scalar (Float 3.141592654),"bigint" := Scalar (Integer64 9223372036854775807),"columns" := Array [String "Last Name",String "First Name",String "MI"],"bitmask" := Scalar (Integer 8131)]]]
decode :: C.Configuration -> IO (Maybe Group)
decode c = do
  res <- runMaybeT $ MaybeT (C.configRootSetting c) >>= toGroup
  C.touchConfiguration c
  return res

-- | Load the libconfig configuration file at the given path and try
-- to convert it to a top-level 'Group' of 'Setting's.
--
-- >>> decodeFrom "test/test.conf"
-- Just ["version" := Scalar (String "1.0"),"application" := Group ["window" := Group ["title" := Scalar (String "My Application"),"size" := Group ["w" := Scalar (Integer 640),"h" := Scalar (Integer 480)],"pos" := Group ["x" := Scalar (Integer 350),"y" := Scalar (Integer 250)]],"list" := List [List [Scalar (String "abc"),Scalar (Integer 123),Scalar (Boolean True)],Scalar (Float 1.234),List []],"books" := List [Group ["title" := Scalar (String "Treasure Island"),"author" := Scalar (String "Robert Louis Stevenson"),"price" := Scalar (Float 29.95),"qty" := Scalar (Integer 5)],Group ["title" := Scalar (String "Snow Crash"),"author" := Scalar (String "Neal Stephenson"),"price" := Scalar (Float 9.99),"qty" := Scalar (Integer 8)]],"misc" := Group ["pi" := Scalar (Float 3.141592654),"bigint" := Scalar (Integer64 9223372036854775807),"columns" := Array [String "Last Name",String "First Name",String "MI"],"bitmask" := Scalar (Integer 8131)]]]
decodeFrom :: String -> IO (Maybe Group)
decodeFrom filename = do
  c <- C.configNew filename
  case c of
   Just conf -> decode conf
   Nothing -> return Nothing
