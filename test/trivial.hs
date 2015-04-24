{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class (liftIO)
import System.Exit (exitFailure, exitSuccess)

import Language.Libconfig

lookups :: Configuration -> MaybeT IO ()
lookups c = MaybeT $ do
  _ <- configLookupInt c "application.window.size.pos.x"
  _ <- configLookupInt64 c "application.misc.bigint"
  _ <- configLookupFloat c "application.misc.pi"
  _ <- configLookupBool c "application.list[0][2]"
  _ <- configLookupString c "application.window.title"
  return $ Just ()

runTest :: IO (Maybe ())
runTest = runMaybeT $ do
  c <- liftIO configInit
  c' <- liftIO configInit
  MaybeT $ do
    _ <- configReadFile c "test/test.conf"
    _ <- configWriteFile c "/tmp/test_output.conf"
    configReadFile c' "/tmp/test_output.conf"
  _ <- lookups c
  lookups c'

main :: IO ()
main = do
  result <- runTest
  case result of
   Nothing -> exitFailure
   Just _  -> exitSuccess
