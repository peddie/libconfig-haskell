{-# OPTIONS_GHC -Wall #-}

{-|
Module      :  Language.Libconfig.Encode
Copyright   :  (c) Matthew Peddie 2014
License     :  BSD3

Maintainer  :  mpeddie@gmail.com
Stability   :  experimental
Portability :  GHC

Converting from "Language.Libconfig.Types" structures to native
libconfig 'C.Configuration's.

-}

module Language.Libconfig.Encode (
  -- * Encoding libconfig native data
  encode
  , encodeAt
  , encodeValue
  , encodeTo
  -- * Helpers
  , valueType
  , scalarType
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe
import Control.Monad (guard, replicateM_)

import qualified Data.Text as T (unpack)

import Language.Libconfig.Types
import Language.Libconfig.Bindings (ConfigType(..))
import qualified Language.Libconfig.Bindings as C

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> let test = ["version" := Scalar (String "1.0"),"application" := Group ["window" := Group ["title" := Scalar (String "My Application"),"size" := Group ["w" := Scalar (Integer 640),"h" := Scalar (Integer 480)],"pos" := Group ["x" := Scalar (Integer 350),"y" := Scalar (Integer 250)]],"list" := List [List [Scalar (String "abc"),Scalar (Integer 123),Scalar (Boolean True)],Scalar (Float 1.234),List []],"books" := List [Group ["title" := Scalar (String "Treasure Island"),"author" := Scalar (String "Robert Louis Stevenson"),"price" := Scalar (Float 29.95),"qty" := Scalar (Integer 5)],Group ["title" := Scalar (String "Snow Crash"),"author" := Scalar (String "Neal Stephenson"),"price" := Scalar (Float 9.99),"qty" := Scalar (Integer 8)]],"misc" := Group ["pi" := Scalar (Float 3.141592654),"bigint" := Scalar (Integer64 9223372036854775807),"columns" := Array [String "Last Name",String "First Name",String "MI"],"bitmask" := Scalar (Integer 8131)]]]

-- | Compute the 'C.ConfigType' of a 'Value'
--
-- >>> valueType (Scalar (String "butts"))
-- StringType
-- >>> valueType (Array [String "butts"])
-- ArrayType
valueType :: Value -> ConfigType
valueType (Scalar s) = scalarType s
valueType (Array _) = ArrayType
valueType (List _) = ListType
valueType (Group _) = GroupType

-- | Compute the 'C.ConfigType' of a 'Scalar'
--
-- >>> scalarType (String "butts")
-- StringType
-- >>> scalarType (Boolean False)
-- BoolType
scalarType :: Scalar -> ConfigType
scalarType (Boolean _) = BoolType
scalarType (Integer _) = IntType
scalarType (Hex _) = IntType
scalarType (Integer64 _) = Int64Type
scalarType (Hex64 _) = Int64Type
scalarType (Float _) = FloatType
scalarType (String _) = StringType

scalarSet :: C.Setting -> Scalar -> MaybeT IO ()
scalarSet sp = MaybeT . go
  where
    go (Boolean b) = C.configSettingSetBool sp b
    go (Integer i) = C.configSettingSetInt sp (fromIntegral i)
    go (Integer64 i) = C.configSettingSetInt64 sp i
    go (Hex h) = C.configSettingSetInt sp (fromIntegral h)
    go (Hex64 h) = C.configSettingSetInt64 sp (fromIntegral h)
    go (Float f) = C.configSettingSetFloat sp f
    go (String s) = C.configSettingSetString sp (T.unpack s)

addValue :: C.Setting -> Value -> MaybeT IO C.Setting
addValue parent value = do
  newset <- MaybeT $ C.configSettingAdd parent "" (valueType value)
  setValue newset value
  return newset

addSetting :: C.Setting -> Setting -> MaybeT IO C.Setting
addSetting parent (name := value) = do
  newset <- MaybeT $ C.configSettingAdd parent (T.unpack name) (valueType value)
  setValue newset value
  return newset

setValue :: C.Setting -> Value -> MaybeT IO ()
setValue sp (Scalar s) = scalarSet sp s
setValue sp (Array a)  = mapM_ (addValue sp . Scalar) a
setValue sp (Group g)  = mapM_ (addSetting sp) g
setValue sp (List l)   = mapM_ (addValue sp) l

-- | Convert a top-level 'Group' of 'Setting's into a native
-- 'C.Configuration'.  This allocates a new 'C.Configuration'.
--
-- >>> Just conf <- runMaybeT $ encode test
-- >>> C.configWriteFile conf "/tmp/encode_output_test.conf"
-- Just ()
-- >>> Just newconf <- C.configNew "/tmp/encode_output_test.conf"
encode :: Group -> MaybeT IO C.Configuration
encode g = do
  conf <- liftIO C.configInit
  encodeAt conf g
  return conf

-- | Convert a top-level 'Group' of 'Setting's into a native
-- libconfig structure and output it to the specified file path.
--
-- >>> runMaybeT $ encodeTo test "/tmp/encode_output_test_2.conf"
-- Just ()
-- >>> Just newconf <- C.configNew "/tmp/encode_output_test.conf"
encodeTo :: Group -> String -> MaybeT IO ()
encodeTo g filename = do
  c <- encode g
  MaybeT $ C.configWriteFile c filename

-- | Encode a top-level 'Group' of 'Setting's and write them to the
-- specified 'C.Configuration'.
encodeAt :: C.Configuration -> Group -> MaybeT IO ()
encodeAt conf g = do
  root <- MaybeT $ C.configRootSetting conf
  setValue root (Group g)

checkType :: C.Setting -> C.ConfigType -> MaybeT IO ()
checkType sp ty = do
  ty' <- liftIO $ C.configSettingType sp
  guard (ty == ty')

removeKids :: C.Setting -> MaybeT IO ()
removeKids sp = do
  count <- liftIO $ C.configSettingLength sp
  replicateM_ count (MaybeT $ C.configSettingRemoveElem sp 0)

-- | Set the value of the given 'C.Setting' to the provided 'Value'
-- (recursively).  If this 'C.Setting' is of a collection type, any
-- pre-existing children will be removed.
encodeValue :: C.Setting -> Value -> MaybeT IO ()
encodeValue sp v = do
  checkType sp (valueType v)
  removeKids sp
  setValue sp v
