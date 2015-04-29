{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

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
  -- $setup

  -- * Encoding libconfig native data
  encode
  , encodeAt
  , encodeValue
  , encodeTo
    -- * Encoding errors
  , EncodeError(..)
    -- * Helpers
  , valueType
  , scalarType
  ) where

import Control.Applicative

import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Control.Monad (when, replicateM_)

import qualified Data.Text as T (unpack, pack, empty)
import Data.Monoid ((<>))

import Language.Libconfig.Types
import Language.Libconfig.Bindings (ConfigType(..), ConfigFormat(..))
import qualified Language.Libconfig.Bindings as C

-- $setup
-- To run these usage examples, you must tell GHC it's allowed to
-- parse string literals as 'Text' values:
--
-- >>> :set -XOverloadedStrings
--
-- We also pre-define the @test@ 'Group' to be the example from the
-- @libconfig@ manual.
--
-- >>> let Just [version, application, window, title, size, w, h, pos, x, y, list, books, author, price, qty, misc, pi, columns, bigint, bitmask] = mapM textToName ["version", "application", "window", "title", "size", "w", "h", "pos", "x", "y", "list", "books", "author", "price", "qty", "misc", "pi", "columns", "bigint", "bitmask"]
-- >>> let test = [version := Scalar (String "1.0"),application := Group [window := Group [title := Scalar (String "My Application"),size := Group [w := Scalar (Integer 640),h := Scalar (Integer 480)],pos := Group [x := Scalar (Integer 350),y := Scalar (Integer 250)]],list := List [List [Scalar (String "abc"),Scalar (Integer 123),Scalar (Boolean True)],Scalar (Float 1.234),List []],books := List [Group [title := Scalar (String "Treasure Island"),author := Scalar (String "Robert Louis Stevenson"),price := Scalar (Float 29.95),qty := Scalar (Integer 5)],Group [title := Scalar (String "Snow Crash"),author := Scalar (String "Neal Stephenson"),price := Scalar (Float 9.99),qty := Scalar (Integer 8)]],misc := Group [pi := Scalar (Float 3.141592654),bigint := Scalar (Integer64 9223372036854775807),columns := Array [String "Last Name",String "First Name",String "MI"],bitmask := Scalar (Hex 8131)]]]

-- | Any of these problems can occur while encoding a @libconfig@
-- 'C.Configuration'.
data EncodeError = EncoderRoot  -- ^ No root setting was found
                                -- (possibly this configuration is
                                -- invalid?)
                 | TypeMismatch {
                   encodeErrSetting :: Text  -- ^ The Haskell structure contained
                                             -- invalid libconfig types for this setting.
                 }
                 | FileOutput {
                   encodeErrFilename :: Text  -- ^ Failed to open this file
                 }
                 | AddSetting {
                   encodeErrParent :: Text    -- ^ Failed to create a new 'C.Setting'
                 , encodeErrValue :: Value    -- ^ This was the value we
                                              -- were making a spot for
                 }
                 | RemoveOldValue {
                   encodeErrSetting :: Text  -- ^ Failed to remove an old value
                                             -- from this setting
                 }
                 | SetValue {
                   encodeErrSetting :: Text  -- ^ Failed to assign a value
                                             -- to this 'C.Setting'
                 , encodeErrValue :: Value   -- ^ This was the value we
                                             -- tried to assign
                 }
                 | SetIndex {
                   encodeErrParent :: Text  -- ^ Failed to assign an element
                                            -- within this collection
                 , encodeErrIndex :: Int    -- ^ This is the index of the element
                 , encodeErrValue :: Value  -- ^ This is the value we
                                            -- tried to assign
                 } deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance NFData EncodeError

type Encoder a = ExceptT EncodeError IO a

withErr :: Maybe a -> e -> Either e a
withErr Nothing e  = Left e
withErr (Just x) _ = Right x

addTrace :: C.Setting -> Encoder a -> Encoder a
addTrace s = flip catchE handler
  where
    mapSetting _ e@(FileOutput _)    = e
    mapSetting f (SetIndex p i v) = SetIndex (f p) i v
    mapSetting f (AddSetting p v) = AddSetting (f p) v
    mapSetting f e = e { encodeErrSetting = f (encodeErrSetting e) }
    repair name path
      | path == T.empty = name
      | otherwise       = name <> "." <> path
    handler e = do
      name <- liftIO $ getName s
      throwE $ mapSetting (repair name) e

getName :: C.Setting -> IO Text
getName s = do
  name <- C.configSettingName s
  return $ case name of
   Nothing -> "<no name>"
   Just x  -> T.pack x

scalarSet :: C.Setting -> Scalar -> Encoder ()
scalarSet sp v = addTrace sp $
                 ExceptT $ (`withErr` SetValue "" (Scalar v)) <$> go v
  where
    go (Boolean b) = C.configSettingSetBool sp b
    go (Integer i) = C.configSettingSetInt sp (fromIntegral i)
    go (Integer64 i) = C.configSettingSetInt64 sp i
    go (Float f) = C.configSettingSetFloat sp f
    go (String s) = C.configSettingSetString sp (T.unpack s)
    go (Hex h) = runMaybeT $ do
      MaybeT $ C.configSettingSetFormat sp HexFormat
      MaybeT $ C.configSettingSetInt sp (fromIntegral h)
    go (Hex64 h) = runMaybeT $ do
      MaybeT $ C.configSettingSetFormat sp HexFormat
      MaybeT $ C.configSettingSetInt64 sp (fromIntegral h)

addValue :: Text -> C.Setting -> Value -> Encoder C.Setting
addValue nm parent value = do
  newset <- ExceptT $ (`withErr` AddSetting "" value) <$> add
  addTrace newset $ setValue newset value
  return newset
  where
    add = C.configSettingAdd parent (T.unpack nm) (valueType value)

addSetting :: C.Setting -> Setting -> Encoder C.Setting
addSetting parent (name := value) =
  addValue (nameToText name) parent value

setValue :: C.Setting -> Value -> Encoder ()
setValue sp (Scalar s) = scalarSet sp s
setValue sp (Group g)  = mapM_ (addSetting sp) g
setValue sp (List l)   = mapM_ (addValue "" sp) l
setValue sp (Array a)  = do
  if arrayCheck a
    then mapM_ (addValue "" sp . Scalar) a
    else throwE $ TypeMismatch ""

-- | Convert a top-level 'Group' of 'Setting's into a native
-- 'C.Configuration'.  This allocates a new 'C.Configuration'.
--
-- >>> Right conf <- encode test
-- >>> C.configWriteFile conf "/tmp/encode_output_test.conf"
-- Just ()
--
-- >>> Just newconf <- C.configNew "/tmp/encode_output_test.conf"
encode :: Group -> IO (Either EncodeError C.Configuration)
encode g = runExceptT $ do
  conf <- liftIO C.configInit
  ExceptT $ encodeAt conf g
  return conf

-- | Convert a top-level 'Group' of 'Setting's into a native
-- libconfig structure and output it to the specified file path.
--
-- >>> encodeTo test "/tmp/encode_output_test_2.conf"
-- Right ()
--
-- >>> Just newconf <- C.configNew "/tmp/encode_output_test_2.conf"
encodeTo :: Group -> String -> IO (Either EncodeError ())
encodeTo g filename = runExceptT $ do
  c <- ExceptT $ encode g
  ExceptT $ (`withErr` FileOutput (T.pack filename)) <$>
    C.configWriteFile c filename

-- | Encode a top-level 'Group' of 'Setting's and write them to the
-- specified 'C.Configuration'.
encodeAt :: C.Configuration -> Group -> IO (Either EncodeError ())
encodeAt conf g = runExceptT $ do
  root <- ExceptT $ (`withErr` EncoderRoot) <$> C.configRootSetting conf
  setValue root (Group g)

checkType :: C.Setting -> C.ConfigType -> Encoder ()
checkType sp ty = addTrace sp $ do
  ty' <- liftIO $ C.configSettingType sp
  when (ty == ty') $ throwE $ TypeMismatch ""

removeKids :: C.Setting -> Encoder ()
removeKids sp = addTrace sp $ do
  count <- liftIO $ C.configSettingLength sp
  replicateM_ count (ExceptT $ (`withErr` RemoveOldValue "") <$>
                     C.configSettingRemoveElem sp 0)

-- | Set the value of the given 'C.Setting' to the provided 'Value'
-- (recursively).  If this 'C.Setting' is of a collection type, any
-- pre-existing children will be removed.
encodeValue :: C.Setting -> Value -> IO (Either EncodeError ())
encodeValue sp v = runExceptT $ do
  checkType sp (valueType v)
  removeKids sp
  setValue sp v

-- | Compute the 'C.ConfigType' of a 'Value'
--
-- >>> valueType (Scalar (String "butts"))
-- StringType
--
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
--
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

arrayCheck :: Array -> Bool
arrayCheck [] = True
arrayCheck ((Boolean _):arr)   = all isBoolean arr
arrayCheck ((Integer _):arr)   = all isInteger arr
arrayCheck ((Integer64 _):arr) = all isInteger64 arr
arrayCheck ((Hex _):arr)       = all isHex arr
arrayCheck ((Hex64 _):arr)     = all isHex64 arr
arrayCheck ((Float _):arr)     = all isFloat arr
arrayCheck ((String _):arr)    = all isString arr
