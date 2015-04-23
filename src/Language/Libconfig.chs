{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      :  Language.Libconfig
Copyright   :  (c) Matthew Peddie 2014
License     :  BSD3

Maintainer  :  mpeddie@gmail.com
Stability   :  experimental
Portability :  GHC

Low-level FFI bindings to the <http://www.hyperrealm.com/libconfig/ libconfig>
configuration file library.  Please see the
<http://www.hyperrealm.com/libconfig/libconfig_manual.html libconfig manual>
for documentation on what the various functions do functionally; the
documentation in this module is only to do with FFI details and
C-vs.-Haskell impedance mismatches.

-}

module Language.Libconfig (
  -- * Types
  Config
  , ConfigPtr
  , Setting
  , SettingPtr
  , ConfigErr(..)
  , ConfigType(..)
    -- * Construction and destruction
  , configInit
  , configDestroy
    -- * Config I/O
  , configReadFile
  , configWriteFile
  , configReadString
    -- * Safe (capable of returning an error) getting of primitives
  , configSettingLookupInt
  , configSettingLookupInt64
  , configSettingLookupFloat
  , configSettingLookupBool
  , configSettingLookupString
    -- * Unsafe getting of primitives
  , configSettingGetInt
  , configSettingGetInt64
  , configSettingGetFloat
  , configSettingGetBool
  , configSettingGetString
    -- * Setting of primitives
  , configSettingSetInt
  , configSettingSetInt64
  , configSettingSetFloat
  , configSettingSetBool
  , configSettingSetString
    -- * Unsafe getting of primitives from a collection
  , configSettingGetIntElem
  , configSettingGetInt64Elem
  , configSettingGetFloatElem
  , configSettingGetBoolElem
  , configSettingGetStringElem
    -- * Setting of primitives within a collection
  , configSettingSetIntElem
  , configSettingSetInt64Elem
  , configSettingSetFloatElem
  , configSettingSetBoolElem
  , configSettingSetStringElem
    -- * Collection management
  , configSettingIndex
  , configSettingLength
  , configSettingGetElem
  , configSettingGetMember
  , configSettingAdd
  , configSettingRemove
  , configSettingRemoveElem
    -- * Miscellaneous
  , configSettingName
  , configSettingParent
  , configSettingIsRoot
    -- * Config file type system
  , configSettingType
  , configSettingIsGroup
  , configSettingIsList
  , configSettingIsArray
  , configSettingIsAggregate
  , configSettingIsNumber
  , configSettingIsScalar
    -- * Direct lookup by path
  , configLookup
  , configLookupFrom
  , configLookupInt
  , configLookupInt64
  , configLookupFloat
  , configLookupBool
  , configLookupString
  ) where

import Foreign
import Foreign.C
import Control.Applicative

#include <libconfig.h>

{#enum config_error_t as ConfigErr {underscoreToCase} deriving (Show, Eq) #}

data ConfigType = NoneType
                | GroupType
                | IntType
                | Int64Type
                | FloatType
                | StringType
                | BoolType
                | ArrayType
                | ListType
                deriving (Eq, Show, Read, Ord, Enum, Bounded)

fromConfigType :: Integral a => ConfigType -> a
fromConfigType = fromIntegral . fromEnum

toConfigType :: Integral a => a -> ConfigType
toConfigType = toEnum . fromIntegral

data ConfigFormat = DefaultFormat
                  | HexFormat
                  deriving (Eq, Show, Read, Ord, Enum, Bounded)

data ConfigBool = ConfigFalse
                | ConfigTrue
                deriving (Eq, Show, Read, Ord, Enum, Bounded)

{#pointer *config_list_t as ConfigListPtr -> ConfigList #}

-- {#pointer *config_value_t as ConfigValuePtr -> ConfigValue #}

{#pointer *config_setting_t as SettingPtr -> Setting #}

{#pointer *config_t as ConfigPtr -> Config #}

data ConfigList = ConfigList {
  length'ConfigList :: CUInt
  , elements'ConfigList :: Ptr SettingPtr
  }

instance Storable ConfigList where
  sizeOf _ = {#sizeof config_list_t #}
  alignment _ = {#alignof config_list_t #}
  peek p = ConfigList <$>
           {#get config_list_t->length #} p <*>
           {#get config_list_t->elements #} p
  poke p ConfigList{..} = do
    {#set config_list_t->length #} p length'ConfigList
    {#set config_list_t->elements #} p elements'ConfigList

data ConfigValue = IVal CInt
                 | LLVal CLLong
                 | FVal CDouble
                 | SVal CString
                 | List ConfigListPtr
                 | None

configValueType :: ConfigValue -> String
configValueType (IVal _) = "IVal"
configValueType (LLVal _) = "LLVal"
configValueType (FVal _) = "FVal"
configValueType (SVal _) = "SVal"
configValueType (List _) = "List"
configValueType None = "None"

data Setting = Setting {
    name'Setting :: CString
    , type'Setting :: CShort
    , format'Setting :: CShort
    , value'Setting :: ConfigValue
    , parent'Setting :: SettingPtr
    , config'Setting :: ConfigPtr
    , hook'Setting :: Ptr ()
    , line'Setting :: CUInt
    , file'Setting :: CString
    }

peekConfigValue :: Ptr b -> Int -> ConfigType -> IO ConfigValue
peekConfigValue p n IntType = IVal <$> (peekByteOff p n :: IO CInt)
peekConfigValue p n BoolType = IVal <$> (peekByteOff p n :: IO CInt)
peekConfigValue p n Int64Type = LLVal <$> (peekByteOff p n :: IO CLLong)
peekConfigValue p n FloatType = FVal <$> (peekByteOff p n :: IO CDouble)
peekConfigValue p n StringType = SVal <$> (peekByteOff p n :: IO CString)
peekConfigValue _ _ NoneType = return None
-- I hope this applies for all aggregate types . . .
peekConfigValue p n _ = List <$> (peekByteOff p n :: IO (Ptr ConfigList))

pokeConfigValue :: Ptr b -> Int -> ConfigValue -> ConfigType -> IO ()
pokeConfigValue p n (IVal x) IntType = pokeByteOff p n x
pokeConfigValue p n (IVal x) BoolType = pokeByteOff p n x
pokeConfigValue p n (LLVal x) Int64Type = pokeByteOff p n x
pokeConfigValue p n (FVal x) FloatType = pokeByteOff p n x
pokeConfigValue p n (SVal x) StringType = pokeByteOff p n x
pokeConfigValue _ _ None NoneType = return ()
pokeConfigValue p n (List x) ListType = pokeByteOff p n x
pokeConfigValue p n (List x) GroupType = pokeByteOff p n x
pokeConfigValue p n (List x) ArrayType = pokeByteOff p n x
pokeConfigValue _ _ v ty =
  error $ "Internal error: libconfig type mismatch between config_value_t '" ++
          configValueType v ++ "' and config_setting_t type tag '" ++
          show ty ++ "'!"

instance Storable Setting where
  sizeOf _ = {#sizeof config_setting_t #}
  alignment _ = {#alignof config_setting_t #}
  peek p = do
    nm <- {#get config_setting_t->name #} p
    ty <- {#get config_setting_t->type #} p
    fmt <- {#get config_setting_t->format #} p
    val <- peekConfigValue p 8 (toConfigType ty)
    Setting nm ty fmt val <$>
           {#get config_setting_t->parent #} p <*>
           {#get config_setting_t->config #} p <*>
           {#get config_setting_t->hook #} p <*>
           {#get config_setting_t->line #} p <*>
           {#get config_setting_t->file #} p
  poke p Setting{..} = do
    {#set config_setting_t->name #} p name'Setting
    {#set config_setting_t->type #} p type'Setting
    {#set config_setting_t->format #} p format'Setting
    pokeConfigValue p 8 value'Setting (toConfigType type'Setting)
    {#set config_setting_t->parent #} p parent'Setting
    {#set config_setting_t->config #} p config'Setting
    {#set config_setting_t->hook #} p hook'Setting
    {#set config_setting_t->line #} p line'Setting
    {#set config_setting_t->file #} p file'Setting

data Config = Config {
    root'Config :: SettingPtr
  , destructor'Config :: FunPtr (Ptr () -> IO ())
  , flags'Config :: CUShort
  , tab_width'Config :: CUShort
  , default_format'Config :: CShort
  , include_dir'Config :: CString
  , error_text'Config :: CString
  , error_file'Config :: CString
  , error_line'Config :: CInt
  , error_type'Config :: CInt
  , filenames'Config :: Ptr CString
  , num_filenames'Config :: CUInt
  }

instance Storable Config where
  sizeOf _ = {#sizeof config_t #}
  alignment _  = {#alignof config_t #}
  peek p = Config <$>
           ({#get config_t->root #} p) <*>
           ({#get config_t->destructor #} p) <*>
           ({#get config_t->flags #} p) <*>
           ({#get config_t->tab_width #} p) <*>
           ({#get config_t->default_format #} p) <*>
           ({#get config_t->include_dir #} p) <*>
           ({#get config_t->error_text #} p) <*>
           ({#get config_t->error_file #} p) <*>
           ({#get config_t->error_line #} p) <*>
           ({#get config_t->error_type #} p) <*>
           ({#get config_t->filenames #} p) <*>
           ({#get config_t->num_filenames #} p)
  poke p Config{..} = do
           {#set config_t->root #} p root'Config
           {#set config_t->destructor #} p destructor'Config
           {#set config_t->flags #} p flags'Config
           {#set config_t->tab_width #} p tab_width'Config
           {#set config_t->default_format #} p default_format'Config
           {#set config_t->include_dir #} p include_dir'Config
           {#set config_t->error_text #} p error_text'Config
           {#set config_t->error_file #} p error_file'Config
           {#set config_t->error_line #} p error_line'Config
           {#set config_t->error_type #} p error_type'Config
           {#set config_t->filenames #} p filenames'Config
           {#set config_t->num_filenames #} p num_filenames'Config

withConfig :: Config -> (ConfigPtr -> IO b) -> IO b
withConfig = with

withSetting :: Setting -> (SettingPtr -> IO b) -> IO b
withSetting = with

{- Resource management -}

{#fun unsafe config_init as ^ { alloca- `Config' peek* } -> `()' #}

{#fun unsafe config_destroy as ^ { withConfig* `Config' } -> `()' #}

{- I/O -}

{#fun unsafe config_read_file as ^ { withConfig* `Config' peek*, `String' } -> `Int' #}

{#fun unsafe config_write_file as ^ { withConfig* `Config' peek*, `String' } -> `Int' #}

{#fun unsafe config_read_string as ^ { withConfig* `Config' peek*, `String' } -> `Int' #}

{- Unsafe getting -}

{#fun unsafe config_setting_get_int as ^ { withSetting* `Setting' } -> `Int' #}

{#fun unsafe config_setting_get_int64 as ^ { withSetting* `Setting' } -> `Int64' #}

{#fun unsafe config_setting_get_float as ^ { withSetting* `Setting' } -> `Double' #}

{#fun unsafe config_setting_get_bool as ^ { withSetting* `Setting' } -> `Bool' toBool #}

{#fun unsafe config_setting_get_string as ^ { withSetting* `Setting' } -> `String' #}

{- Safe getting -}

{#fun unsafe config_setting_lookup_int as ^
 { withSetting* `Setting', `String', alloca- `CInt' peek* } -> `Int' #}

{#fun unsafe config_setting_lookup_int64 as ^
 { withSetting* `Setting', `String', alloca- `CLLong' peek* } -> `Int' #}

{#fun unsafe config_setting_lookup_float as ^
 { withSetting* `Setting', `String', alloca- `CDouble' peek* } -> `Int' #}

{#fun unsafe config_setting_lookup_bool as ^
 { withSetting* `Setting', `String', alloca- `CInt' peek* } -> `Int' #}

{#fun unsafe config_setting_lookup_string as ^
 { withSetting* `Setting', `String', alloca- `CString' peek* } -> `Int' #}

{- SettingPtr -}

{#fun unsafe config_setting_set_int as ^ { withSetting* `Setting', `Int' } -> `Int' #}
{#fun unsafe config_setting_set_int64 as ^ { withSetting* `Setting', `Int64' } -> `Int' #}
{#fun unsafe config_setting_set_float as ^ { withSetting* `Setting', `Double' } -> `Int' #}
{#fun unsafe config_setting_set_bool as ^ { withSetting* `Setting', `Bool' } -> `Int' #}
{#fun unsafe config_setting_set_string as ^ { withSetting* `Setting', `String' } -> `Int' #}

{- Unsafe getting elements in collections -}

{#fun unsafe config_setting_get_int_elem as ^ { withSetting* `Setting', `Int' } -> `Int' #}
{#fun unsafe config_setting_get_int64_elem as ^ { withSetting* `Setting', `Int' } -> `Int64' #}
{#fun unsafe config_setting_get_float_elem as ^ { withSetting* `Setting', `Int' } -> `Double' #}
{#fun unsafe config_setting_get_bool_elem as ^ { withSetting* `Setting', `Int' } -> `Bool' toBool #}
{#fun unsafe config_setting_get_string_elem as ^ { withSetting* `Setting', `Int' } -> `String' #}

{- Setting elements in collections -}

{#fun unsafe config_setting_set_int_elem as ^
 { withSetting* `Setting', `Int', `Int' } -> `Setting' peek* #}
{#fun unsafe config_setting_set_int64_elem as ^
 { withSetting* `Setting', `Int', `Int64' } -> `Setting' peek* #}
{#fun unsafe config_setting_set_float_elem as ^
 { withSetting* `Setting', `Int', `Double' } -> `Setting' peek* #}
{#fun unsafe config_setting_set_bool_elem as ^
 { withSetting* `Setting', `Int', `Bool' } -> `Setting' peek* #}
{#fun unsafe config_setting_set_string_elem as ^
 { withSetting* `Setting', `Int', `String' } -> `Setting' peek* #}

{- Collection management -}

{#fun unsafe config_setting_index as ^ { withSetting* `Setting' } -> `Int' #}

{#fun config_setting_length as ^ { withSetting* `Setting' } -> `Int' #}

{#fun config_setting_get_elem as ^ { withSetting* `Setting', id `CUInt' } -> `Setting' peek* #}

{#fun config_setting_get_member as ^ { withSetting* `Setting', `String' } -> `Setting' peek* #}

-- TODO(MP): pass back the modified parent as well?
{#fun config_setting_add as ^
 { withSetting* `Setting', `String', fromConfigType `ConfigType' } -> `Setting' peek* #}

{#fun config_setting_remove as ^
 { withSetting* `Setting', `String' } -> `Int' #}

{#fun config_setting_remove_elem as ^
 { withSetting* `Setting', id `CUInt' } -> `Int' #}

{- Path search -}

{#fun config_lookup as ^
 { withConfig* `Config', `String' } -> `Setting' peek* #}

{#fun config_lookup_from as ^
 { withSetting* `Setting', `String' } -> `Setting' peek* #}

{#fun config_lookup_int as ^
 { withConfig* `Config', `String', alloca- `CInt' peek* } -> `Int' #}

{#fun config_lookup_int64 as ^
 { withConfig* `Config', `String', alloca- `CLLong' peek* } -> `Int' #}

{#fun config_lookup_float as ^
 { withConfig* `Config', `String', alloca- `CDouble' peek* } -> `Int' #}

{#fun config_lookup_bool as ^
 { withConfig* `Config', `String', alloca- `CInt' peek* } -> `Int' #}

{#fun config_lookup_string as ^
 { withConfig* `Config', `String', alloca- `CString' peek* } -> `Int' #}

-- TODO(MP): Reproduce the libconfig macros

configSettingType :: Setting -> ConfigType
configSettingType = toConfigType . type'Setting

configSettingIsGroup :: Setting -> Bool
configSettingIsGroup = (== GroupType) . configSettingType

configSettingIsArray :: Setting -> Bool
configSettingIsArray = (== ArrayType) . configSettingType

configSettingIsList :: Setting -> Bool
configSettingIsList = (== ListType) . configSettingType

configSettingIsAggregate :: Setting -> Bool
configSettingIsAggregate s =
  configSettingType s `elem` [ListType, GroupType, ArrayType]

configSettingIsNumber :: Setting -> Bool
configSettingIsNumber s =
  configSettingType s `elem` [IntType, Int64Type, FloatType]

configSettingIsScalar :: Setting -> Bool
configSettingIsScalar s =
  configSettingType s `elem` [IntType, Int64Type, FloatType, BoolType, StringType]

configSettingName :: Setting -> IO String
configSettingName = peekCString . name'Setting

configSettingParent :: Setting -> IO Setting
configSettingParent = peek . parent'Setting

configSettingIsRoot :: Setting -> Bool
configSettingIsRoot s = parent'Setting s == nullPtr


-- TODO(MP): Null pointer checks for functions that return a config
-- setting, config, etc.

-- TODO(MP): Adapt to native Haskell types in alloca- X peek* cases
-- where c2hs can't do it
