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
    -- * Decoding errors
  , DecodeError(..)
  ) where

import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader

import qualified Data.Text as T (pack)

import Language.Libconfig.Types
import Language.Libconfig.Bindings (ConfigType(..), ConfigFormat(..))
import qualified Language.Libconfig.Bindings as C

-- TODO(MP): Properly convert integral types depending on the setting
-- format.

data DecodeError = Root  -- ^ No root setting was found (possibly this
                         -- configuration is invalid?)
                 | Name {
                   decodeErrSetting :: C.Setting  -- ^ This setting had no name
                                                  -- but was in a 'Group'.
                 }
                 | GetNone {
                   decodeErrSetting :: C.Setting  -- ^ This setting was of type
                                                  -- 'NoneType', but it should
                                                  -- have a type.
                 }
                 | GetIndex {
                   decodeErrParent :: C.Setting  -- ^ Failed to get a child of
                                                 -- this 'C.Setting'
                 , decodeErrIndex :: Int         -- ^ This was the index we
                                                 -- tried to look up
                 }
                 | Parse {
                   decodeErrFilename :: Text    -- ^ The file in which
                                                -- parsing failed
                 , decodeErrLine :: Word32      -- ^ The line of the file on
                                                -- which parsing failed
                 , decodeErrDescription :: Text  -- ^ @libconfig@'s description
                                                 -- of the parsing failure
                 }
                 | FileIO {
                   decodeErrFilename :: Text    -- ^ Failed to open this file
                 } deriving (Eq)

instance Show DecodeError where
  show Root = "Root"
  show (Name _) = "Name <setting>"
  show (GetNone _) = "GetNone <setting>"
  show (GetIndex _ i) = "GetNone <setting> " ++ show i
  show (Parse file line desc) =
    "Parse " ++ show file ++ " " ++ show line ++ " " ++ show desc
  show (FileIO file) = "FileIO " ++ show file

withErr :: Maybe a -> e -> Either e a
withErr Nothing e  = Left e
withErr (Just x) _ = Right x

decoder :: IO (Either DecodeError a) -> Decoder a
decoder = lift . ExceptT

throw :: DecodeError -> Decoder a
throw = lift . throwE

type Decoder a = ReaderT ConfigFormat (ExceptT DecodeError IO) a

toScalar :: C.Setting -> Decoder Scalar
toScalar s = do
  ty <- liftIO $ C.configSettingType s
  localFormat <- liftIO $ C.configSettingGetFormat s
  format <- case localFormat of
    HexFormat -> return HexFormat
    DefaultFormat -> ask
  decoder $ go format ty
  where
    go :: ConfigFormat -> ConfigType -> IO (Either DecodeError Scalar)
    go DefaultFormat IntType =
      Right . Integer . fromIntegral <$> C.configSettingGetInt s
    go DefaultFormat Int64Type =
      Right . Integer64 . fromIntegral <$> C.configSettingGetInt64 s
    go HexFormat IntType =
      Right . Hex . fromIntegral <$> C.configSettingGetInt s
    go HexFormat Int64Type =
      Right . Hex64 . fromIntegral <$> C.configSettingGetInt64 s
    go _ FloatType = Right . Float <$> C.configSettingGetFloat s
    go _ BoolType = Right . Boolean <$> C.configSettingGetBool s
    go _ StringType = Right . String . T.pack <$> C.configSettingGetString s
    go _ t =
      error $ "Language.Libconfig.Decode.toScalar: internal error (bug!): expected " ++
      "a type in [IntType, Int64Type, FloatType, BoolType, StringType], but got '" ++
      show t ++ "'!"

toList :: C.Setting -> Decoder List
toList s = do
  ty <- liftIO $ C.configSettingType s
  go ty
  where
    go :: ConfigType -> Decoder List
    go ListType = do
      l <- liftIO $ C.configSettingLength s
      mapM get [0 .. l - 1]
    go ty =
      error $ "Language.Libconfig.Decode.toList: internal error (bug!): expected " ++
      "a value with 'ListType', but got '" ++ show ty ++ "'!"
    get :: Int -> Decoder Value
    get i = do
      el <- decoder $ (`withErr` GetIndex s i) <$> C.configSettingGetElem s i
      toValue el

toArray :: C.Setting -> Decoder Array
toArray s = liftIO (C.configSettingType s) >>= go
  where
    go :: ConfigType -> Decoder Array
    go ArrayType = do
      l <- liftIO $ C.configSettingLength s
      mapM get [0 .. l - 1]
    go ty =
      error $ "Language.Libconfig.Decode.toArray: internal error (bug!): expected " ++
      "a value with 'ArrayType', but got '" ++ show ty ++ "'!"
    get i = do
      el <- decoder $ (`withErr` GetIndex s i) <$> C.configSettingGetElem s i
      toScalar el

toGroup :: C.Setting -> Decoder Group
toGroup s = liftIO (C.configSettingType s) >>= go
  where
    go :: ConfigType -> Decoder Group
    go GroupType = do
      l <- liftIO $ C.configSettingLength s
      mapM get [0 .. l - 1]
    go ty =
      error $ "Language.Libconfig.Decode.toGroup: internal error (bug!): expected " ++
      "a value with 'GroupType', but got '" ++ show ty ++ "'!"
    get i = do
      el <- decoder $ (`withErr` GetIndex s i) <$> C.configSettingGetElem s i
      decodeSetting el

toValue :: C.Setting -> Decoder Value
toValue s = liftIO (C.configSettingType s) >>= go
  where
    go :: ConfigType -> Decoder Value
    go NoneType = throw $ GetNone s
    go ListType = List <$> toList s
    go ArrayType = Array <$> toArray s
    go GroupType = Group <$> toGroup s
    go _ = Scalar <$> toScalar s

decodeSetting :: C.Setting -> Decoder Setting
decodeSetting s = liftIO (C.configSettingType s) >>= go
  where
    go NoneType = throw $ GetNone s
    go _ =
      (:=) <$>
      fmap T.pack (decoder $ (`withErr` Name s) <$> C.configSettingName s) <*>
      toValue s

-- | Convert a native 'C.Configuration' into a top-level 'Group' of
-- 'Setting's.
--
-- >>> Just conf <- C.configNew "test/test.conf"
-- >>> decode conf
-- Right ["version" := Scalar (String "1.0"),"application" := Group ["window" := Group ["title" := Scalar (String "My Application"),"size" := Group ["w" := Scalar (Integer 640),"h" := Scalar (Integer 480)],"pos" := Group ["x" := Scalar (Integer 350),"y" := Scalar (Integer 250)]],"list" := List [List [Scalar (String "abc"),Scalar (Integer 123),Scalar (Boolean True)],Scalar (Float 1.234),List []],"books" := List [Group ["title" := Scalar (String "Treasure Island"),"author" := Scalar (String "Robert Louis Stevenson"),"price" := Scalar (Float 29.95),"qty" := Scalar (Integer 5)],Group ["title" := Scalar (String "Snow Crash"),"author" := Scalar (String "Neal Stephenson"),"price" := Scalar (Float 9.99),"qty" := Scalar (Integer 8)]],"misc" := Group ["pi" := Scalar (Float 3.141592654),"bigint" := Scalar (Integer64 9223372036854775807),"columns" := Array [String "Last Name",String "First Name",String "MI"],"bitmask" := Scalar (Hex 8131)]]]
decode :: C.Configuration -> IO (Either DecodeError Group)
decode c = do
  format <- C.configGetDefaultFormat c
  res <- runExceptT $ runReaderT (getRoot c >>= toGroup) format
  C.touchConfiguration c
  return res
  where
    getRoot cnf = decoder $ (`withErr` Root) <$> C.configRootSetting cnf

-- | Load the libconfig configuration file at the given path and try
-- to convert it to a top-level 'Group' of 'Setting's.
--
-- >>> decodeFrom "test/test.conf"
-- Right ["version" := Scalar (String "1.0"),"application" := Group ["window" := Group ["title" := Scalar (String "My Application"),"size" := Group ["w" := Scalar (Integer 640),"h" := Scalar (Integer 480)],"pos" := Group ["x" := Scalar (Integer 350),"y" := Scalar (Integer 250)]],"list" := List [List [Scalar (String "abc"),Scalar (Integer 123),Scalar (Boolean True)],Scalar (Float 1.234),List []],"books" := List [Group ["title" := Scalar (String "Treasure Island"),"author" := Scalar (String "Robert Louis Stevenson"),"price" := Scalar (Float 29.95),"qty" := Scalar (Integer 5)],Group ["title" := Scalar (String "Snow Crash"),"author" := Scalar (String "Neal Stephenson"),"price" := Scalar (Float 9.99),"qty" := Scalar (Integer 8)]],"misc" := Group ["pi" := Scalar (Float 3.141592654),"bigint" := Scalar (Integer64 9223372036854775807),"columns" := Array [String "Last Name",String "First Name",String "MI"],"bitmask" := Scalar (Hex 8131)]]]
decodeFrom :: String -> IO (Either DecodeError Group)
decodeFrom filename = do
  c <- C.configInit
  red <- C.configReadFile c filename
  case red of
   Nothing -> do
     ty <- C.configErrorType c
     fn <- T.pack <$> C.configErrorFile c
     case ty of
      C.ConfigErrFileIo -> return . Left $ FileIO fn
      C.ConfigErrParse  -> do
        err <- Parse fn <$>
               (fromIntegral <$> C.configErrorLine c) <*>
               (T.pack <$> C.configErrorText c)
        return $ Left err
      _               ->
        error "Language.Libconfig.Decode.decodeFrom: something is really broken!"
   Just _ -> decode c
