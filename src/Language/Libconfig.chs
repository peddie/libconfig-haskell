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
for documentation on what the various functions actually do; the
documentation in this module is only to do with FFI details and
C-vs.-Haskell impedance mismatches.

-}

module Language.Libconfig (
  -- * Doctest example setup

  -- $setup
  -- * Types
  Configuration
  , Setting
  , ConfigErr(..)
  , ConfigType(..)
  , ConfigBool(..)
  , ConfigFormat(..)
    -- * Resource management
  , configInit
    -- * Config I/O
  , configReadFile
  , configWriteFile
  , configReadString
    -- * Safe (capable of returning an error) getting of primitives

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
    -- no way to distinguish between a successful result and a
    -- failure.
  , configSettingGetInt
  , configSettingGetInt64
  , configSettingGetFloat
  , configSettingGetBool
  , configSettingGetString
    -- * Setting of primitives

    -- | These functions return a value of type 'Maybe ()', indicating
    -- whether the action was successful.  (It may fail if, for
    -- example, there is a setting type mismatch.)
  , configSettingSetInt
  , configSettingSetInt64
  , configSettingSetFloat
  , configSettingSetBool
  , configSettingSetString
    -- * Unsafe getting of primitives from a collection

    -- | These functions are sketchy if used directly, because
    -- there is no way to distinguish between a successful result and
    -- a failure.
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
  , configGetDefaultFormat
  , configSetDefaultFormat
  , configGetTabWidth
  , configSetTabWidth
  , configSettingSourceLine
  , configSettingSourceFile
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

-- $setup
--
-- All the examples run on the included test file @test/test.conf@,
-- which is provided in the
-- <http://www.hyperrealm.com/libconfig/libconfig_manual.html#Configuration-Files libconfig manual>.
--
-- >>> conf <- configInit
-- >>> configReadFile conf "test/test.conf"
-- Just ()

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
    val <- peekConfigValue p 8 (toConfigType ty)
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
    pokeConfigValue p 8 value'Setting (toConfigType type'Setting)
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

{#fun unsafe config_read_file as ^
 { withConfiguration* `Configuration', `String' } -> `Maybe ()' checkBool #}

{#fun unsafe config_write_file as ^
 { withConfiguration* `Configuration', `String' } -> `Maybe ()' checkBool #}

{#fun unsafe config_read_string as ^
 { withConfiguration* `Configuration', `String' } -> `Maybe ()' checkBool #}

{- Unsafe getting -}

{#fun unsafe config_setting_get_int as ^ { getSetting `Setting' } -> `Int' #}

{#fun unsafe config_setting_get_int64 as ^ { getSetting `Setting' } -> `Int64' #}

{#fun unsafe config_setting_get_float as ^ { getSetting `Setting' } -> `Double' #}

{#fun unsafe config_setting_get_bool as ^ { getSetting `Setting' } -> `Bool' toBool #}

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

configSettingLookupInt :: Setting -> String -> IO (Maybe Int)
configSettingLookupInt s = fmap checkTuple . configSettingLookupInt' s
configSettingLookupInt64 :: Setting -> String -> IO (Maybe Int64)
configSettingLookupInt64 s = fmap checkTuple . configSettingLookupInt64' s
configSettingLookupFloat :: Setting -> String -> IO (Maybe Double)
configSettingLookupFloat s = fmap checkTuple . configSettingLookupFloat' s
configSettingLookupBool :: Setting -> String -> IO (Maybe Bool)
configSettingLookupBool s = fmap checkTuple . configSettingLookupBool' s
configSettingLookupString :: Setting -> String -> IO (Maybe String)
configSettingLookupString s = fmap checkTuple . configSettingLookupString' s

{- Setting values -}

{#fun unsafe config_setting_set_int as ^
 { getSetting `Setting', `Int' } -> `Maybe ()' checkBool #}
{#fun unsafe config_setting_set_int64 as ^
 { getSetting `Setting', `Int64' } -> `Maybe ()' checkBool #}
{#fun unsafe config_setting_set_float as ^
 { getSetting `Setting', `Double' } -> `Maybe ()' checkBool #}
{#fun unsafe config_setting_set_bool as ^
 { getSetting `Setting', `Bool' } -> `Maybe ()' checkBool #}
{#fun unsafe config_setting_set_string as ^
 { getSetting `Setting', `String' } -> `Maybe ()' checkBool #}

{- Unsafe getting elements in collections -}

{#fun unsafe config_setting_get_int_elem as ^ { getSetting `Setting', `Int' } -> `Int' #}
{#fun unsafe config_setting_get_int64_elem as ^ { getSetting `Setting', `Int' } -> `Int64' #}
{#fun unsafe config_setting_get_float_elem as ^ { getSetting `Setting', `Int' } -> `Double' #}
{#fun unsafe config_setting_get_bool_elem as ^ { getSetting `Setting', `Int' } -> `Bool' toBool #}
{#fun unsafe config_setting_get_string_elem as ^ { getSetting `Setting', `Int' } -> `String' #}

{- Setting elements in collections -}

{#fun unsafe config_setting_set_int_elem as ^
 { getSetting `Setting', `Int', `Int' } -> `Maybe Setting' checkSetting #}
{#fun unsafe config_setting_set_int64_elem as ^
 { getSetting `Setting', `Int', `Int64' } -> `Maybe Setting' checkSetting #}
{#fun unsafe config_setting_set_float_elem as ^
 { getSetting `Setting', `Int', `Double' } -> `Maybe Setting' checkSetting #}
{#fun unsafe config_setting_set_bool_elem as ^
 { getSetting `Setting', `Int', `Bool' } -> `Maybe Setting' checkSetting #}
{#fun unsafe config_setting_set_string_elem as ^
 { getSetting `Setting', `Int', `String' } -> `Maybe Setting' checkSetting #}

{- Collection management -}

{#fun unsafe config_setting_index as ^
 { getSetting `Setting' } -> `Int' #}

{#fun config_setting_length as ^
 { getSetting `Setting' } -> `Int' #}

{#fun config_setting_get_elem as ^
 { getSetting `Setting', fromIntegral `Int' } -> `Maybe Setting' checkSetting #}

{#fun config_setting_get_member as ^
 { getSetting `Setting', `String' } -> `Maybe Setting' checkSetting #}

{#fun config_setting_add as ^
 { getSetting `Setting', `String', fromConfigType `ConfigType' }
   -> `Maybe Setting' checkSetting #}

{#fun config_setting_remove as ^
 { getSetting `Setting', `String' } -> `Int' #}

{#fun config_setting_remove_elem as ^
 { getSetting `Setting', fromIntegral `Int' } -> `Int' #}

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
-- >>> Just app <- configLookup conf "application"
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

configSettingType :: Setting -> IO ConfigType
configSettingType = fmap (toConfigType . type'Setting) . peek . getSetting

configSettingIsGroup :: Setting -> IO Bool
configSettingIsGroup = fmap (== GroupType) . configSettingType

configSettingIsArray :: Setting -> IO Bool
configSettingIsArray = fmap (== ArrayType) . configSettingType

configSettingIsList :: Setting -> IO Bool
configSettingIsList = fmap (== ListType) . configSettingType

configSettingIsAggregate :: Setting -> IO Bool
configSettingIsAggregate =
  fmap (`elem` [ListType, GroupType, ArrayType]) . configSettingType

configSettingIsNumber :: Setting -> IO Bool
configSettingIsNumber =
  fmap (`elem` [IntType, Int64Type, FloatType]) . configSettingType

configSettingIsScalar :: Setting -> IO Bool
configSettingIsScalar =
  fmap (`elem` [IntType, Int64Type, FloatType, BoolType, StringType]) .
  configSettingType

configSettingName :: Setting -> IO (Maybe String)
configSettingName (Setting sp) = do
  s <- peek sp
  if (name'Setting s == nullPtr)
    then return Nothing
    else Just <$> peekCString (name'Setting s)

configSettingParent :: Setting -> IO (Maybe Setting)
configSettingParent = fmap (checkSetting . parent'Setting) . peek . getSetting

configSettingIsRoot :: Setting -> IO Bool
configSettingIsRoot = fmap ((==nullPtr) . parent'Setting) . peek . getSetting

configRootSetting :: Configuration -> IO (Maybe Setting)
configRootSetting =
  flip withForeignPtr (fmap (checkSetting . root'Config) . peek) . getConfiguration

configSetDefaultFormat :: Configuration -> ConfigFormat -> IO ()
configSetDefaultFormat c' f =
  modifyConfiguration c' $
  \c -> c { default_format'Config = fromIntegral $ fromEnum f }

configGetDefaultFormat :: Configuration -> IO ConfigFormat
configGetDefaultFormat =
  onConfiguration (toEnum . fromIntegral . default_format'Config)

configSetTabWidth :: Configuration -> Int -> IO ()
configSetTabWidth c' w =
  modifyConfiguration c' $
  \c -> c { tab_width'Config = fromIntegral w }

configGetTabWidth :: Configuration -> IO Int
configGetTabWidth =
  onConfiguration (fromIntegral . tab_width'Config)

configSettingSourceLine :: Setting -> IO Int
configSettingSourceLine =
  fmap (fromIntegral . line'Setting) . peek . getSetting

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
