{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}

{-|
Module      :  Language.Libconfig.Types
Copyright   :  (c) Matthew Peddie 2014
License     :  BSD3

Maintainer  :  mpeddie@gmail.com
Stability   :  experimental
Portability :  GHC

Core types for configuration file data.

-}

module Language.Libconfig.Types (
  -- $intro

  -- ** A note about the examples

  -- $setup

  -- * Primitive types
  Name
  , nameToText
  , textToName
  , Setting(..)
  , getSettingName
  , getSettingValue
  , Value(..)
  , isScalar
  , isCollection
  , isArray
  , isGroup
  , isList
  , Scalar(..)
  , isBoolean
  , isInteger
  , isInteger64
  , isHex
  , isHex64
  , isFloat
  , isString
    -- * Collection types
  , Array
  , List
  , Group
    -- * Re-exports
  , Int32
  , Int64
  , Word32
  , Word64
  , Text
  ) where

import GHC.Generics (Generic)
import Data.Data (Data)
import Data.Typeable (Typeable)

import Data.Hashable (Hashable)
import Control.DeepSeq (NFData)

import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid ((<>))

import Control.Monad (guard)
import Data.Maybe (isJust)

#ifdef BINARY_INSTANCES
import Data.Binary (Binary)
import Data.Text.Binary ()
#endif
#ifdef CEREAL_INSTANCES
import Data.Serialize (Serialize)
import Data.Serialize.Text ()
#endif

import Data.Int (Int32, Int64)
import Data.Word (Word32, Word64)

-- $setup
--
-- To run these usage examples, you must tell GHC it's allowed to
-- parse string literals as 'Text' values:
--
-- >>> :set -XOverloadedStrings


-- | A @libconfig@ 'Name' is a string of restricted form.  It must
-- match the regular expression @[A-Za-z\*][-A-Za-z0-9_\*]*@.
newtype Name = Name { getName :: Text }
             deriving (Eq, Ord, Data, Typeable, Generic)

instance Show Name where
  show (Name text) = show text

instance Read Name where
  readsPrec p str = do
    (x, moar) <- readsPrec p str
    guard (isJust $ textToName x)
    return (Name x, moar)

instance Hashable Name
instance NFData Name

-- | Convert a 'Text' string to a 'Name'.
--
-- >>> textToName "robert"
-- Just "robert"
--
-- >>> textToName "Obi-wan"
-- Just "Obi-wan"
--
-- If the given string does not match the restrictions on the form of
-- 'Name's, then 'Nothing' is returned.
--
-- >>> textToName ""
-- Nothing
--
-- >>> textToName "0bi-wan"
-- Nothing
textToName :: Text -> Maybe Name
textToName t
  | t == T.empty = Nothing
  | hd `T.isInfixOf` nameFirstLetters &&
    T.all (\c -> T.singleton c `T.isInfixOf` nameLetters) tl = Just $ Name t
  | otherwise = Nothing
  where
    (hd, tl) = T.splitAt 1 t
    nameFirstLetters = T.pack $ ['a'..'z'] ++ ['A'..'Z'] ++ "*"
    nameLetters = nameFirstLetters <> T.pack ('_' : '-' : ['0'..'9'])

-- | Convert a 'Name' to 'Text'
--
-- >>> let Just robert = textToName "robert"
-- >>> nameToText robert
-- "robert"
nameToText :: Name -> Text
nameToText = getName

infixr 3 :=
-- | A @libconfig@ 'Setting' is a name-value pair, @name := value@.
data Setting = !Name := !Value
             deriving (Eq, Show, Read, Ord, Data, Typeable, Generic)

instance Hashable Setting where
instance NFData Setting where

-- | Get out the name of a 'Setting'
getSettingName :: Setting -> Name
getSettingName (n := _) = n

-- | Get out the value of a 'Setting'
getSettingValue :: Setting -> Value
getSettingValue (_ := v) = v

-- | A libconfig 'Value' is either a 'Scalar' value or some type of
-- collection.
data Value = Scalar !Scalar
           | Array !Array
           | List !List
           | Group !Group
           deriving (Eq, Show, Read, Ord, Data, Typeable, Generic)

instance Hashable Value where
instance NFData Value where

-- |
-- >>> isScalar $ Scalar (String "butts")
-- True
--
-- >>> isScalar $ Array [String "butts"]
-- False
isScalar :: Value -> Bool
isScalar (Scalar _) = True
isScalar _          = False

-- | @isCollection = not . isScalar@
--
-- >>> isCollection $ Scalar (String "butts")
-- False
--
-- >>> isCollection $ Array [String "butts"]
-- True
isCollection :: Value -> Bool
isCollection = not . isScalar

-- |
-- >>> isArray $ Array [String "butts"]
-- True
--
-- >>> isArray $ List [Scalar $ String "butts"]
-- False
isArray :: Value -> Bool
isArray (Array _) = True
isArray _          = False

-- |
-- >>> isList $ Array [String "butts"]
-- False
--
-- >>> isList $ List [Scalar $ String "butts"]
-- True
isList :: Value -> Bool
isList (List _) = True
isList _          = False

-- |
-- >>> isGroup $ Array [String "butts"]
-- False
--
-- >>> let Just asset = textToName "asset"
-- >>> isGroup $ Group [asset := Scalar (String "butts")]
-- True
isGroup :: Value -> Bool
isGroup (Group _) = True
isGroup _          = False

-- | A libconfig 'Scalar' value is a boolean value, a string or one of
-- an assortment of numeric types.
data Scalar = Boolean !Bool
            | Integer !Int32
            | Integer64 !Int64
            | Hex !Word32
            | Hex64 !Word64
            | Float !Double
            | String !Text
            deriving (Eq, Show, Read, Ord, Data, Typeable, Generic)

-- |
-- >>> isBoolean $ Boolean True
-- True
--
-- >>> isBoolean $ Float 22.22
-- False
isBoolean :: Scalar -> Bool
isBoolean (Boolean _) = True
isBoolean _           = False

-- |
-- >>> isInteger $ Integer 19
-- True
--
-- >>> isInteger $ Float 22.22
-- False
isInteger :: Scalar -> Bool
isInteger (Integer _) = True
isInteger _           = False

-- |
-- >>> isInteger64 $ Integer64 22222222222
-- True
--
-- >>> isInteger64 $ Float 22.22
-- False
isInteger64 :: Scalar -> Bool
isInteger64 (Integer64 _) = True
isInteger64 _           = False

-- |
-- >>> isHex $ Hex 0x13
-- True
--
-- >>> isHex $ Float 22.22
-- False
isHex :: Scalar -> Bool
isHex (Hex _) = True
isHex _           = False


-- |
-- >>> isHex64 $ Hex64 0x52c8c338e
-- True
--
-- >>> isHex64 $ Float 22.22
-- False
isHex64 :: Scalar -> Bool
isHex64 (Hex64 _) = True
isHex64 _           = False

-- |
-- >>> isFloat $ Float 22.22
-- True
--
-- >>> isFloat $ Integer 19
-- False
isFloat :: Scalar -> Bool
isFloat (Float _) = True
isFloat _           = False

-- |
-- >>> isString $ String "BUTTS"
-- True
--
-- >>> isString $ Float 22.22
-- False
isString :: Scalar -> Bool
isString (String _) = True
isString _           = False

instance Hashable Scalar where
instance NFData Scalar where

#ifdef BINARY_INSTANCES
instance Binary Name
instance Binary Setting
instance Binary Value
instance Binary Scalar
#endif

#ifdef CEREAL_INSTANCES
instance Serialize Name
instance Serialize Setting
instance Serialize Value
instance Serialize Scalar
#endif

-- | libconfig 'Array's can contain any number of 'Scalar' values.
-- These values must be of the same type.  This is currently not
-- enforced by the data structure, and violating it may lead to
-- failures to encode.
type Array = [Scalar]

-- | libconfig 'List's can contain any number of 'Value's.
type List = [Value]

-- | libconfig 'Group's are like 'List's, except that each element in the
-- 'Group' is a 'Setting' with its own unique name, not just an
-- unlabeled 'Value'.
type Group = [Setting]

-- $intro
--
-- Here is the example configuration file @test/test.conf@ from the
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
-- The 'Language.Libconfig.Decode.decode' function renders this as the
-- following structure:
--
-- @
-- [
--   "version" := Scalar (String "1.0")
-- , "application" := Group [
--       "window" := Group [
--          "title" := Scalar (String "My Application")
--        , "size" := Group [
--              "w" := Scalar (Integer 640)
--            , "h" := Scalar (Integer 480)
--           ]
--         , "pos" := Group [
--              "x" := Scalar (Integer 350)
--            , "y" := Scalar (Integer 250)
--           ]
--        ]
--     , "list" := List [
--           List [
--              Scalar (String "abc")
--            , Scalar (Integer 123)
--            , Scalar (Boolean True)
--           ]
--         , Scalar (Float 1.234)
--         , List []
--        ]
--     , "books" := List [
--           Group [
--              "title" := Scalar (String "Treasure Island")
--            , "author" := Scalar (String "Robert Louis Stevenson")
--            , "price" := Scalar (Float 29.95)
--            , "qty" := Scalar (Integer 5)
--           ]
--         , Group [
--              "title" := Scalar (String "Snow Crash")
--            , "author" := Scalar (String "Neal Stephenson")
--            , "price" := Scalar (Float 9.99)
--            , "qty" := Scalar (Integer 8)
--           ]
--        ]
--     , "misc" := Group [
--           "pi" := Scalar (Float 3.141592654)
--         , "bigint" := Scalar (Integer64 9223372036854775807)
--         , "columns" := Array [
--               String "Last Name"
--             , String "First Name"
--             , String "MI"
--           ]
--         , "bitmask" := Scalar (Integer 8131)
--       ]
--    ]
-- ]
-- @
