{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
Module      :  Language.Libconfig.Bindings
Copyright   :  (c) Matthew Peddie 2014
License     :  BSD3

Maintainer  :  mpeddie@gmail.com
Stability   :  experimental
Portability :  GHC

Low-level FFI bindings to the <http://www.hyperrealm.com/libconfig/ libconfig>
configuration file library.  Please see the
<http://www.hyperrealm.com/libconfig/libconfig_manual.html libconfig manual>
for documentation on what the various functions actually do and the
underlying model of the libconfig API.  The documentation in this
module contains many usage examples which double as tests, but the
focus is only on FFI details and C-vs.-Haskell impedance mismatches.
As a result, there is no explanation of the behavior of many of the
functions.

-}

module Language.Libconfig.Bindings (
  -- * Doctest example setup

  -- $setup
  -- * Types
  Configuration
  , Setting
  , ConfigErr(..)
  , ConfigType(..)
  , isCollectionType
  , isScalarType
  , ConfigFormat(..)
    -- * Resource management
  , configInit
  , configNew
  , touchConfiguration
    -- * Config I/O
  , configReadFile
  , configWriteFile
  , configReadString
    -- * Safe (capable of returning an error) getting of primitive
    -- settings from the parent setting, by name.

    -- | These Haskell functions return 'Nothing' if the lookup fails,
    -- there is a type mismatch, etc.
  , configSettingLookupInt
  , configSettingLookupInt64
  , configSettingLookupFloat
  , configSettingLookupBool
  , configSettingLookupString
    -- * Unsafe getting of primitives

    -- |
    -- These functions are sketchy if used directly, because there is
    -- no way to distinguish between a successful result and a failure
    -- (at the @libconfig@ level).  Take care to only ever use these
    -- once you've already checked the 'ConfigType' of the 'Setting'
    -- using 'configSettingType'.
  , configSettingGetInt
  , configSettingGetInt64
  , configSettingGetFloat
  , configSettingGetBool
  , configSettingGetString
    -- * Setting of primitives

    -- | These functions return a value of type 'Maybe' @()@, indicating
    -- whether the action was successful.  (It may fail if, for
    -- example, there is a setting type mismatch.)
  , configSettingSetInt
  , configSettingSetInt64
  , configSettingSetFloat
  , configSettingSetBool
  , configSettingSetString
    -- * Unsafe getting of primitives from a collection

    -- |
    -- These functions are sketchy if used directly, because there is
    -- no way to distinguish between a successful result and a failure
    -- (at the @libconfig@ level).  Take care to only ever use these
    -- once you've already checked the 'ConfigType' of the element
    -- using 'configSettingType' or verified it for other elements of
    -- an array.
    --
    -- These functions may be used on collections with type
    -- 'GroupType', 'ArrayType' or 'ListType'.
  , configSettingGetIntElem
  , configSettingGetInt64Elem
  , configSettingGetFloatElem
  , configSettingGetBoolElem
  , configSettingGetStringElem
    -- * Setting of primitives within a collection

    -- | In the event of an out-of-bounds index or a type mismatch,
    -- these functions return 'Nothing'.  If the function succeeds,
    -- the Setting that is returned will be either the same 'Setting'
    -- that previously existed at that spot or a newly allocated one.
    --
    -- These functions may be used on collections with type
    -- 'ArrayType' or 'ListType' (but __not__ 'GroupType').
  , configSettingSetIntElem
  , configSettingSetInt64Elem
  , configSettingSetFloatElem
  , configSettingSetBoolElem
  , configSettingSetStringElem
    -- * Direct lookup by path

    -- | In the event of a name lookup failure or type mismatch, these
    -- functions return 'Nothing'.
  , configLookup
  , configLookupFrom
  , configLookupInt
  , configLookupInt64
  , configLookupFloat
  , configLookupBool
  , configLookupString
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
  , configRootSetting
  , configSettingSourceLine
  , configSettingSourceFile
    -- ** Formatting
  , configGetDefaultFormat
  , configSetDefaultFormat
  , configSettingGetFormat
  , configSettingSetFormat
  , configGetTabWidth
  , configSetTabWidth
    -- * Error reporting
  , configErrorFile
  , configErrorText
  , configErrorLine
  , configErrorType
    -- * Config file type system
  , configSettingType
  , configSettingIsGroup
  , configSettingIsList
  , configSettingIsArray
  , configSettingIsAggregate
  , configSettingIsNumber
  , configSettingIsScalar
  ) where

import Foreign
import Foreign.C
import Control.Monad ((>=>))
import Control.Applicative

import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

-- $setup
--
-- All the examples run on the included test file @test/test.conf@,
-- which is reproduced here from the
-- <http://www.hyperrealm.com/libconfig/libconfig_manual.html#Configuration-Files libconfig manual>.
--
-- @
--      # Example application configuration file
--
--      version = "1.0";
--
--      application:
--      {
--        window:
--        {
--          title = "My Application";
--          size = { w = 640; h = 480; };
--          pos = { x = 350; y = 250; };
--        };
--
--        list = ( ( "abc", 123, true ), 1.234, ( /* an empty list */) );
--
--        books = ( { title  = "Treasure Island";
--                    author = "Robert Louis Stevenson";
--                    price  = 29.95;
--                    qty    = 5; },
--                  { title  = "Snow Crash";
--                    author = "Neal Stephenson";
--                    price  = 9.99;
--                    qty    = 8; } );
--
--        misc:
--        {
--          pi = 3.141592654;
--          bigint = 9223372036854775807L;
--          columns = [ "Last Name", "First Name", "MI" ];
--          bitmask = 0x1FC3;
--        };
--      };
-- @
--
-- The following setup actions are assumed for many of the usage
-- examples below.
--
-- >>> Just conf <- configNew "test/test.conf"
-- >>> Just app <- configLookup conf "application"
-- >>> Just misc <- configLookupFrom app "misc"
-- >>> Just winsize <- configLookupFrom app "window.size"
--
-- @conf'@ is used for modifying values.
--
-- >>> Just conf' <- configNew "test/test.conf"
--

#include <libconfig.h>

-- | This is a set of possible errors that can occur when @libconfig@
-- tries to read in a config file.
{#enum config_error_t as ConfigErr {underscoreToCase} deriving (Show, Eq) #}


-- | This is a set of possible @libconfig@ types.  Many functions will
-- return 'Nothing' if you attempt to use a value as the incorrect
-- type.  See the @libconfig@ manual for more details.
data ConfigType = NoneType
                | GroupType
                | IntType
                | Int64Type
                | FloatType
                | StringType
                | BoolType
                | ArrayType
                | ListType
                deriving (Eq, Show, Read, Ord, Enum, Bounded, Data, Typeable, Generic)

-- | Tells whether a 'ConfigType' value is a collection ('ListType',
-- 'ArrayType' or 'GroupType').
--
-- >>> isCollectionType GroupType
-- True
-- >>> isCollectionType BoolType
-- False
isCollectionType :: ConfigType -> Bool
isCollectionType ArrayType = True
isCollectionType ListType  = True
isCollectionType GroupType = True
isCollectionType _         = False

-- | Tells whether a 'ConfigType' value is a scalar (i.e. not a
-- collection).
--
-- >>> isScalarType FloatType
-- True
--
-- >>> isScalarType ListType
-- False
--
-- __Note:__
--
-- >>> isScalarType NoneType
-- True
isScalarType :: ConfigType -> Bool
isScalarType = not . isCollectionType


fromEnumIntegral :: (Enum c, Integral a) => c -> a
fromEnumIntegral = fromIntegral . fromEnum

toEnumIntegral :: (Enum c, Integral a) => a -> c
toEnumIntegral = toEnum . fromIntegral

-- | This is used for fine-grained configuration of how integers are
-- output when a config file is written.  See 'configGetDefaultFormat'
-- and the @libconfig@ manual.
data ConfigFormat = DefaultFormat
                  | HexFormat
                  deriving (Eq, Show, Read, Ord, Enum, Bounded, Data, Typeable, Generic)

data ConfigBool = ConfigFalse
                | ConfigTrue
                deriving (Eq, Show, Read, Ord, Enum, Bounded, Data, Typeable, Generic)

{#pointer *config_list_t as ConfigListPtr -> ConfigList #}

{#pointer *config_setting_t as SettingPtr -> Setting' #}

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

data Setting' = Setting' {
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

instance Storable Setting' where
  sizeOf _ = {#sizeof config_setting_t #}
  alignment _ = {#alignof config_setting_t #}
  peek p = do
    nm <- {#get config_setting_t->name #} p
    ty <- {#get config_setting_t->type #} p
    fmt <- {#get config_setting_t->format #} p
    val <- peekConfigValue p 8 (toEnumIntegral ty)
    Setting' nm ty fmt val <$>
           {#get config_setting_t->parent #} p <*>
           {#get config_setting_t->config #} p <*>
           {#get config_setting_t->hook #} p <*>
           {#get config_setting_t->line #} p <*>
           {#get config_setting_t->file #} p
  poke p Setting'{..} = do
    {#set config_setting_t->name #} p name'Setting
    {#set config_setting_t->type #} p type'Setting
    {#set config_setting_t->format #} p format'Setting
    pokeConfigValue p 8 value'Setting (toEnumIntegral type'Setting)
    {#set config_setting_t->parent #} p parent'Setting
    {#set config_setting_t->config #} p config'Setting
    {#set config_setting_t->hook #} p hook'Setting
    {#set config_setting_t->line #} p line'Setting
    {#set config_setting_t->file #} p file'Setting

-- libconfig itself manages all the 'Setting's, including deallocation
-- and allocation, so we don't have to use a 'ForeignPtr' or
-- 'StablePtr' here.  TODO(MP): Ensure that a 'Setting' can't get used
-- outside the scope of its parent 'Configuration'

-- | Corresponds to a @libconfig@ @config_setting_t@ value; wrapped
-- opaquely for pointer safety.
newtype Setting = Setting { getSetting :: Ptr Setting' } deriving (Eq)

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

foreign import ccall unsafe "src/Language/Libconfig.chs.h config_init"
  configInit' :: Ptr Config -> IO ()

foreign import ccall unsafe "src/Language/Libconfig.chs.h &config_destroy"
  configDestroy' :: FunPtr (Ptr Config -> IO ())

-- | Top-level configuration value, corresponding to the libconfig
-- @config_t@.  Wrapped opaquely for pointer-safety.
newtype Configuration = Configuration { getConfiguration :: ForeignPtr Config }
                      deriving (Eq)

-- | This function allocates a new 'Configuration' and initializes it.
configInit :: IO Configuration
configInit = do
  c <- mallocForeignPtr
  addForeignPtrFinalizer configDestroy' c
  withForeignPtr c configInit'
  return $ Configuration c

withConfiguration :: Configuration -> (Ptr Config -> IO a) -> IO a
withConfiguration (Configuration c) = withForeignPtr c

modifyConfiguration :: Configuration -> (Config -> Config) -> IO ()
modifyConfiguration (Configuration p) mf = withForeignPtr p $ \cp -> do
  c <- peek cp
  poke cp $ mf c

-- |
-- @libconfig@ manages storage for all 'Setting' objects via the
-- 'Configuration', so if a 'Configuration' goes out of scope, GHC may
-- get rid of it, and any 'Setting' objects may become invalid.  This
-- function can be used to ensure that a 'Configuration' doesn't get
-- automatically garbage-collected too early.
touchConfiguration :: Configuration -> IO ()
touchConfiguration = touchForeignPtr . getConfiguration

onConfiguration :: (Config -> a) -> Configuration -> IO a
onConfiguration f = flip withConfiguration (fmap f . peek)

{- Marshalling -}

checkPtr :: Storable a => Ptr a -> Maybe (Ptr a)
checkPtr p
  | nullPtr == p = Nothing
  | otherwise    = Just p

checkSetting :: Ptr Setting' -> Maybe Setting
checkSetting = fmap Setting . checkPtr

peekIntegral :: (Integral a, Storable a, Num b) => Ptr a -> IO b
peekIntegral = fmap fromIntegral . peek

peekFloat :: (Real a, Storable a, Fractional b) => Ptr a -> IO b
peekFloat = fmap realToFrac . peek

peekBool :: (Eq a, Num a, Storable a) => Ptr a -> IO Bool
peekBool = fmap toBool . peek

peekString :: Ptr CString -> IO String
peekString = peek >=> peekCString

asBool :: Integral a => a -> ConfigBool
asBool = toEnum . fromIntegral

checkBool :: Integral a => a -> Maybe ()
checkBool a = case asBool a of
  ConfigTrue  -> Just ()
  ConfigFalse -> Nothing

checkTuple :: (ConfigBool, a) -> Maybe a
checkTuple (ConfigTrue, x) = Just x
checkTuple _               = Nothing

{- Resource management -}

{- I/O -}

-- | Read in a 'Configuration' from the specified configuration file.
-- The 'Configuration' should already be initialized with
-- 'configInit'.
{#fun unsafe config_read_file as ^
 { withConfiguration* `Configuration', `String' } -> `Maybe ()' checkBool #}

-- | Create a new 'Configuration' and read in the data from the
-- specified configuration file.
--
-- >> configNew s = configInit >>= \c -> configReadFile c s
configNew :: String -> IO (Maybe Configuration)
configNew s = do
  c <- configInit
  red <- configReadFile c s
  return $ case red of
            Nothing -> Nothing
            Just _  -> Just c

-- | Write out a 'Configuration' to the specified configuration file.
{#fun unsafe config_write_file as ^
 { withConfiguration* `Configuration', `String' } -> `Maybe ()' checkBool #}

-- | Read configuration data from a string.
{#fun unsafe config_read_string as ^
 { withConfiguration* `Configuration', `String' } -> `Maybe ()' checkBool #}

{- Unsafe getting -}

-- |
-- >>> Just appwinwidth <- configLookup conf "application.window.size.w"
-- >>> configSettingGetInt appwinwidth
-- 640
{#fun unsafe config_setting_get_int as ^ { getSetting `Setting' } -> `Int' #}

-- |
-- >>> Just miscbigint <- configLookup conf "application.misc.bigint"
-- >>> configSettingGetInt64 miscbigint
-- 9223372036854775807
{#fun unsafe config_setting_get_int64 as ^ { getSetting `Setting' } -> `Int64' #}

-- |
-- >>> Just miscpi <- configLookup conf "application.misc.pi"
-- >>> configSettingGetFloat miscpi
-- 3.141592654
{#fun unsafe config_setting_get_float as ^ { getSetting `Setting' } -> `Double' #}

-- |
-- >>> Just listbool <- configLookup conf "application.list.[0].[2]"
-- >>> configSettingGetBool listbool
-- True
{#fun unsafe config_setting_get_bool as ^ { getSetting `Setting' } -> `Bool' toBool #}

-- |
-- >>> Just wintitle <- configLookup conf "application.window.title"
-- >>> configSettingGetString wintitle
-- "My Application"
{#fun unsafe config_setting_get_string as ^ { getSetting `Setting' } -> `String' #}

{- Safe getting -}

{#fun unsafe config_setting_lookup_int as configSettingLookupInt'
 { getSetting `Setting', `String', alloca- `Int' peekIntegral* } -> `ConfigBool' asBool #}

{#fun unsafe config_setting_lookup_int64 as configSettingLookupInt64'
 { getSetting `Setting', `String', alloca- `Int64' peekIntegral* } -> `ConfigBool' asBool #}

{#fun unsafe config_setting_lookup_float as configSettingLookupFloat'
 { getSetting `Setting', `String', alloca- `Double' peekFloat* } -> `ConfigBool' asBool #}

{#fun unsafe config_setting_lookup_bool as configSettingLookupBool'
 { getSetting `Setting', `String', alloca- `Bool' peekBool* } -> `ConfigBool' asBool #}

{#fun unsafe config_setting_lookup_string as configSettingLookupString'
 { getSetting `Setting', `String', alloca- `String' peekString* } -> `ConfigBool' asBool #}

-- |
-- >>> configSettingLookupInt winsize "w"
-- Just 640
configSettingLookupInt :: Setting -> String -> IO (Maybe Int)
configSettingLookupInt s = fmap checkTuple . configSettingLookupInt' s

-- |
-- >>> configSettingLookupInt64 misc "bigint"
-- Just 9223372036854775807
configSettingLookupInt64 :: Setting -> String -> IO (Maybe Int64)
configSettingLookupInt64 s = fmap checkTuple . configSettingLookupInt64' s

-- |
-- >>> configSettingLookupFloat misc "pi"
-- Just 3.141592654
configSettingLookupFloat :: Setting -> String -> IO (Maybe Double)
configSettingLookupFloat s = fmap checkTuple . configSettingLookupFloat' s

-- | (The example configuration file does not contain any boolean
-- values that are direct children of a @config_setting_t@.)
configSettingLookupBool :: Setting -> String -> IO (Maybe Bool)
configSettingLookupBool s = fmap checkTuple . configSettingLookupBool' s

-- |
-- >>> Just win <- configLookupFrom app "window"
-- >>> configSettingLookupString win "title"
-- Just "My Application"
configSettingLookupString :: Setting -> String -> IO (Maybe String)
configSettingLookupString s = fmap checkTuple . configSettingLookupString' s

{- Setting values -}

-- |
-- >>> Just treasureqty <- configLookup conf' "application.books.[0].qty"
-- >>> configSettingSetInt treasureqty 222
-- Just ()
-- >>> configSettingGetInt treasureqty
-- 222
{#fun unsafe config_setting_set_int as ^
 { getSetting `Setting', `Int' } -> `Maybe ()' checkBool #}

-- |
-- >>> Just miscbigint <- configLookup conf' "application.misc.bigint"
-- >>> configSettingSetInt64 miscbigint 92233720368547758
-- Just ()
-- >>> configSettingGetInt64 miscbigint
-- 92233720368547758
{#fun unsafe config_setting_set_int64 as ^
 { getSetting `Setting', `Int64' } -> `Maybe ()' checkBool #}

-- |
-- >>> Just treasureprice <- configLookup conf' "application.books.[0].price"
-- >>> configSettingSetFloat treasureprice 22.22
-- Just ()
-- >>> configSettingGetFloat treasureprice
-- 22.22
{#fun unsafe config_setting_set_float as ^
 { getSetting `Setting', `Double' } -> `Maybe ()' checkBool #}

-- |
-- >>> Just listbool <- configLookup conf' "application.list.[0].[2]"
-- >>> configSettingSetBool listbool False
-- Just ()
-- >>> configSettingGetBool listbool
-- False
{#fun unsafe config_setting_set_bool as ^
 { getSetting `Setting', `Bool' } -> `Maybe ()' checkBool #}

-- |
-- >>> Just treasureauthor <- configLookup conf' "application.books.[0].author"
-- >>> configSettingSetString treasureauthor "Robert L. Stevenson"
-- Just ()
-- >>> configSettingGetString treasureauthor
-- "Robert L. Stevenson"
{#fun unsafe config_setting_set_string as ^
 { getSetting `Setting', `String' } -> `Maybe ()' checkBool #}

{- Unsafe getting elements in collections -}

-- |
-- >>> Just treasure <- configLookup conf "application.books.[0]"
-- >>> configSettingGetIntElem treasure 3
-- 5
{#fun unsafe config_setting_get_int_elem as ^
 { getSetting `Setting', `Int' } -> `Int' #}

-- |
-- >>> Just misc <- configLookup conf "application.misc"
-- >>> configSettingGetInt64Elem misc 1
-- 9223372036854775807
{#fun unsafe config_setting_get_int64_elem as ^
 { getSetting `Setting', `Int' } -> `Int64' #}

-- |
-- >>> Just list <- configLookup conf "application.list"
-- >>> configSettingGetFloatElem list 1
-- 1.234
{#fun unsafe config_setting_get_float_elem as ^
 { getSetting `Setting', `Int' } -> `Double' #}

-- | (The example configuration does not contain any boolean values
-- that are direct children of collections of type
-- @config_setting_t@).
{#fun unsafe config_setting_get_bool_elem as ^
 { getSetting `Setting', `Int' } -> `Bool' toBool #}

-- |
-- >>> Just win <- configLookup conf "application.window"
-- >>> configSettingGetStringElem win 0
-- "My Application"
{#fun unsafe config_setting_get_string_elem as ^
 { getSetting `Setting', `Int' } -> `String' #}

{- Setting elements in collections -}

-- | (This example appends a new value of type 'IntType' to
-- @application.list@, because the example file contains no suitable
-- example values for us to modify.)
--
-- >>> Just list <- configLookup conf' "application.list"
-- >>> Just new3 <- configSettingSetIntElem list (-1) 22
-- >>> configSettingGetIntElem list 3
-- 22
-- >>> configSettingGetInt new3
-- 22
{#fun unsafe config_setting_set_int_elem as ^
 { getSetting `Setting', `Int', `Int' } -> `Maybe Setting' checkSetting #}

-- | (This example appends a new value of type 'Int64Type' to
-- @application.list@, because the example file contains no suitable
-- example values for us to modify.)
--
-- >>> Just list <- configLookup conf' "application.list"
-- >>> Just new3 <- configSettingSetInt64Elem list (-1) 92233720368547758
-- >>> configSettingGetInt64Elem list 3
-- 92233720368547758
-- >>> configSettingGetInt64 new3
-- 92233720368547758
{#fun unsafe config_setting_set_int64_elem as ^
 { getSetting `Setting', `Int', `Int64' } -> `Maybe Setting' checkSetting #}

-- |
-- >>> Just list <- configLookup conf' "application.list"
-- >>> Just new1 <- configSettingSetFloatElem list 1 0.2222
-- >>> configSettingGetFloatElem list 1
-- 0.2222
-- >>> configSettingGetFloat new1
-- 0.2222
{#fun unsafe config_setting_set_float_elem as ^
 { getSetting `Setting', `Int', `Double' } -> `Maybe Setting' checkSetting #}

-- | (This example appends a new value of type 'BoolType' to
-- @application.list@, because the example file contains no suitable
-- example values for us to modify.)
--
-- >>> Just list <- configLookup conf' "application.list"
-- >>> Just new3 <- configSettingSetBoolElem list (-1) False
-- >>> configSettingGetBoolElem list 3
-- False
-- >>> configSettingGetBool new3
-- False
{#fun unsafe config_setting_set_bool_elem as ^
 { getSetting `Setting', `Int', `Bool' } -> `Maybe Setting' checkSetting #}

-- |
-- >>> Just misccols <- configLookup conf' "application.misc.columns"
-- >>> Just new0 <- configSettingSetStringElem misccols 0 "butts"
-- >>> configSettingGetStringElem misccols 0
-- "butts"
-- >>> configSettingGetString new0
-- "butts"
{#fun unsafe config_setting_set_string_elem as ^
 { getSetting `Setting', `Int', `String' } -> `Maybe Setting' checkSetting #}

{- Collection management -}

-- |
-- >>> Just col0 <- configLookup conf "application.misc.columns.[0]"
-- >>> configSettingIndex col0
-- 0
{#fun unsafe config_setting_index as ^
 { getSetting `Setting' } -> `Int' #}

-- |
-- >>> Just cols <- configLookup conf "application.misc.columns"
-- >>> configSettingLength cols
-- 3
{#fun config_setting_length as ^
 { getSetting `Setting' } -> `Int' #}

-- |
-- >>> Just cols <- configLookup conf "application.misc.columns"
-- >>> Just col0 <- configSettingGetElem cols 0
-- >>> configSettingGetString col0
-- "Last Name"
{#fun config_setting_get_elem as ^
 { getSetting `Setting', fromIntegral `Int' } -> `Maybe Setting' checkSetting #}

-- |
-- >>> Just miscpi <- configSettingGetMember misc "pi"
-- >>> configSettingGetFloat miscpi
-- 3.141592654
{#fun config_setting_get_member as ^
 { getSetting `Setting', `String' } -> `Maybe Setting' checkSetting #}

-- |
-- >>> Just misc' <- configLookup conf' "application.misc"
-- >>> Just randSeed <- configSettingAdd misc' "random_seed" IntType
-- >>> configSettingSetInt randSeed 55
-- Just ()
-- >>> configSettingGetInt randSeed
-- 55
-- >>> configSettingLookupInt misc' "random_seed"
-- Just 55
-- >>> configSettingGetIntElem misc' 4
-- 55
{#fun config_setting_add as ^
 { getSetting `Setting', `String', fromEnumIntegral `ConfigType' }
   -> `Maybe Setting' checkSetting #}

-- |
-- >>> Just misc' <- configLookup conf' "application.misc"
-- >>> configSettingLength misc'
-- 4
-- >>> configSettingRemove misc' "bitmask"
-- Just ()
-- >>> configSettingLength misc'
-- 3
{#fun config_setting_remove as ^
 { getSetting `Setting', `String' } -> `Maybe ()' checkBool #}

-- |
-- >>> Just misc' <- configLookup conf' "application.misc"
-- >>> configSettingLength misc'
-- 4
-- >>> configSettingRemoveElem misc' 2
-- Just ()
-- >>> configSettingLength misc'
-- 3
-- >>> Just new2 <- configSettingGetElem misc' 2
-- >>> configSettingType new2
-- IntType
-- >>> configSettingGetInt new2
-- 8131
{#fun config_setting_remove_elem as ^
 { getSetting `Setting', fromIntegral `Int' } -> `Maybe ()' checkBool #}

-- I haven't worked out a good way to do this one yet.  What's
-- necessary is to register a finalizer for 'freeStablePtr xp' with
-- the relevant 'Configuration' pointer.  Unfortunately, we can't go
-- from a 'Setting' to the ForeignPtr contained in a 'Configuration'.
--
-- Punting for now, since I don't even know what you use this for.

-- foreign import ccall unsafe "src/Language/Libconfig.chs.h config_setting_set_hook"
--   configSettingSetHook' :: Ptr Setting' -> Ptr () -> IO ()

-- configSettingSetHook :: Storable a => Setting -> a -> IO ()
-- configSettingSetHook (Setting s) x = do
--   xp <- newStablePtr x
--   cfgp <- config'Setting <$> peek s

-- configSettingGetHook :: Storable a => Setting -> IO a
-- configSettingGetHook (Setting s) = fmap (castPtr . hook'Setting) (peek s) >>= peek


{- Path search -}

-- |
-- >>> Just app <- configLookup conf "application"
-- >>> configSettingName app
-- Just "application"
{#fun config_lookup as ^
 { withConfiguration* `Configuration', `String' } -> `Maybe Setting' checkSetting #}

-- |
-- >>> Just list <- configLookupFrom app "list"
-- >>> configSettingName list
-- Just "list"
{#fun config_lookup_from as ^
 { getSetting `Setting', `String' } -> `Maybe Setting' checkSetting #}

{#fun config_lookup_int as configLookupInt'
 { withConfiguration* `Configuration', `String', alloca- `Int' peekIntegral* }
   -> `ConfigBool' asBool #}

{#fun config_lookup_int64 as configLookupInt64'
 { withConfiguration* `Configuration', `String', alloca- `Int64' peekIntegral* }
   -> `ConfigBool' asBool #}

{#fun config_lookup_float as configLookupFloat'
 { withConfiguration* `Configuration', `String', alloca- `Double' peekFloat* }
   -> `ConfigBool' asBool #}

{#fun config_lookup_bool as configLookupBool'
 { withConfiguration* `Configuration', `String', alloca- `Bool' peekBool* }
   -> `ConfigBool' asBool #}

{#fun config_lookup_string as configLookupString'
 { withConfiguration* `Configuration', `String', alloca- `String' peekString* }
   -> `ConfigBool' asBool #}

-- |
-- >>> Just appwinwidth <- configLookup conf "application.window.size.w"
-- >>> configSettingGetFormat appwinwidth
-- DefaultFormat
{#fun config_setting_get_format as ^
 { getSetting `Setting' }
   -> `ConfigFormat' toEnumIntegral #}

-- |
-- >>> Just appwinwidth' <- configLookup conf' "application.window.size.w"
-- >>> configSettingGetFormat appwinwidth'
-- DefaultFormat
-- >>> configSettingSetFormat appwinwidth' HexFormat
-- Just ()
-- >>> configSettingGetFormat appwinwidth'
-- HexFormat
{#fun config_setting_set_format as ^
 { getSetting `Setting', fromEnumIntegral `ConfigFormat' }
   -> `Maybe ()' checkBool #}

-- |
-- >>> configLookupInt conf "application.window.size.w"
-- Just 640
configLookupInt :: Configuration -> String -> IO (Maybe Int)
configLookupInt c = fmap checkTuple . configLookupInt' c

-- |
-- >>> configLookupInt64 conf "application.misc.bigint"
-- Just 9223372036854775807
configLookupInt64 :: Configuration -> String -> IO (Maybe Int64)
configLookupInt64 c = fmap checkTuple . configLookupInt64' c

-- |
-- >>> configLookupFloat conf "application.misc.pi"
-- Just 3.141592654
configLookupFloat :: Configuration -> String -> IO (Maybe Double)
configLookupFloat c = fmap checkTuple . configLookupFloat' c

-- |
-- >>> configLookupBool conf "application.list.[0].[2]"
-- Just True
configLookupBool :: Configuration -> String -> IO (Maybe Bool)
configLookupBool c = fmap checkTuple . configLookupBool' c


-- |
-- >>> configLookupString conf "application.window.title"
-- Just "My Application"
configLookupString :: Configuration -> String -> IO (Maybe String)
configLookupString c = fmap checkTuple . configLookupString' c

-- |
-- >>> Just list <- configLookup conf "application.list"
-- >>> configSettingType list
-- ListType
configSettingType :: Setting -> IO ConfigType
configSettingType = fmap (toEnumIntegral . type'Setting) . peek . getSetting

-- |
-- >>> Just grp <- configLookup conf "application.window"
-- >>> configSettingIsGroup grp
-- True
configSettingIsGroup :: Setting -> IO Bool
configSettingIsGroup = fmap (== GroupType) . configSettingType

-- |
-- >>> Just arr <- configLookup conf "application.misc.columns"
-- >>> configSettingIsArray arr
-- True
configSettingIsArray :: Setting -> IO Bool
configSettingIsArray = fmap (== ArrayType) . configSettingType

-- |
-- >>> Just list <- configLookup conf "application.list"
-- >>> configSettingIsList list
-- True
configSettingIsList :: Setting -> IO Bool
configSettingIsList = fmap (== ListType) . configSettingType

-- |
-- >>> Just grp <- configLookup conf "application.window"
-- >>> Just arr <- configLookup conf "application.misc.columns"
-- >>> Just list <- configLookup conf "application.list"
-- >>> Just width <- configLookup conf "application.window.size.w"
-- >>> mapM configSettingIsAggregate [grp, arr, list, width]
-- [True,True,True,False]
configSettingIsAggregate :: Setting -> IO Bool
configSettingIsAggregate =
  fmap (`elem` [ListType, GroupType, ArrayType]) . configSettingType

-- |
-- >>> Just int <- configLookup conf "application.window.pos.x"
-- >>> Just bigint <- configLookup conf "application.misc.bigint"
-- >>> Just float <- configLookup conf "application.misc.pi"
-- >>> Just grp <- configLookup conf "application.window"
-- >>> mapM configSettingIsNumber [int, bigint, float, grp]
-- [True,True,True,False]
configSettingIsNumber :: Setting -> IO Bool
configSettingIsNumber =
  fmap (`elem` [IntType, Int64Type, FloatType]) . configSettingType

-- |
-- >>> Just int <- configLookup conf "application.window.pos.x"
-- >>> Just bigint <- configLookup conf "application.misc.bigint"
-- >>> Just float <- configLookup conf "application.misc.pi"
-- >>> Just bool <- configLookup conf "application.list.[0].[2]"
-- >>> Just str <- configLookup conf "application.window.title"
-- >>> Just grp <- configLookup conf "application.window"
-- >>> mapM configSettingIsScalar [int, bigint, float, bool, str, grp]
-- [True,True,True,True,True,False]
configSettingIsScalar :: Setting -> IO Bool
configSettingIsScalar =
  fmap (`elem` [IntType, Int64Type, FloatType, BoolType, StringType]) .
  configSettingType

-- |
-- >>> Just list <- configLookup conf "application.list"
-- >>> configSettingName list
-- Just "list"
--
-- >>> Just list1 <- configLookup conf "application.list.[0]"
-- >>> configSettingName list1
-- Nothing
configSettingName :: Setting -> IO (Maybe String)
configSettingName (Setting sp) = do
  s <- peek sp
  if (name'Setting s == nullPtr)
    then return Nothing
    else Just <$> peekCString (name'Setting s)

-- |
-- >>> Just list <- configLookup conf "application.list"
-- >>> Just app <- configSettingParent list
-- >>> configSettingName app
-- Just "application"
configSettingParent :: Setting -> IO (Maybe Setting)
configSettingParent = fmap (checkSetting . parent'Setting) . peek . getSetting

-- |
-- >>> configSettingIsRoot app
-- False
-- >>> Just root <- configRootSetting conf
-- >>> configSettingIsRoot root
-- True
configSettingIsRoot :: Setting -> IO Bool
configSettingIsRoot = fmap ((==nullPtr) . parent'Setting) . peek . getSetting

-- |
-- >>> Just root <- configRootSetting conf
-- >>> Just version <- configSettingGetMember root "version"
-- >>> configSettingGetString version
-- "1.0"
configRootSetting :: Configuration -> IO (Maybe Setting)
configRootSetting =
  flip withForeignPtr (fmap (checkSetting . root'Config) . peek) . getConfiguration

-- |
-- >>> configGetDefaultFormat conf'
-- DefaultFormat
-- >>> configSetDefaultFormat conf' HexFormat
-- >>> configGetDefaultFormat conf'
-- HexFormat
configSetDefaultFormat :: Configuration -> ConfigFormat -> IO ()
configSetDefaultFormat c' f =
  modifyConfiguration c' $
  \c -> c { default_format'Config = fromIntegral $ fromEnum f }

-- |
-- >>> configGetDefaultFormat conf
-- DefaultFormat
configGetDefaultFormat :: Configuration -> IO ConfigFormat
configGetDefaultFormat =
  onConfiguration (toEnum . fromIntegral . default_format'Config)

-- |
-- >>> configGetTabWidth conf'
-- 2
-- >>> configSetTabWidth conf' 8
-- >>> configGetTabWidth conf'
-- 8
configSetTabWidth :: Configuration -> Int -> IO ()
configSetTabWidth c' w =
  modifyConfiguration c' $
  \c -> c { tab_width'Config = fromIntegral w }

-- |
-- >>> configGetTabWidth conf
-- 2
configGetTabWidth :: Configuration -> IO Int
configGetTabWidth =
  onConfiguration (fromIntegral . tab_width'Config)

-- |
-- >>> configSettingSourceLine app
-- 5
configSettingSourceLine :: Setting -> IO Int
configSettingSourceLine =
  fmap (fromIntegral . line'Setting) . peek . getSetting

-- |
-- >>> configSettingSourceFile app
-- "test/test.conf"
configSettingSourceFile :: Setting -> IO String
configSettingSourceFile (Setting s) = peek s >>= peekCString . file'Setting

configErrorFile :: Configuration -> IO String
configErrorFile c =
  withConfiguration c $
  \p -> peek p >>= peekCString . error_file'Config

configErrorText :: Configuration -> IO String
configErrorText c =
  withConfiguration c $
  \p -> peek p >>= peekCString . error_text'Config

configErrorLine :: Configuration -> IO Int
configErrorLine =
  onConfiguration (fromIntegral . error_line'Config)

configErrorType :: Configuration -> IO ConfigErr
configErrorType =
  onConfiguration (toEnum . fromIntegral . error_type'Config)

-- TODO(MP): Perhaps a MonadIO m => ConfigT m which both wraps non-IO
-- config actions and uses ExceptT or MaybeT to keep track of errors
