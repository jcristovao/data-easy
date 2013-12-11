{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module CMTCTest (specs) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.Maybe

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic as QCM

import Data.Text (Text)
import qualified Data.Text as T

import Data.UniformUtils
import Control.Monad.Trans.Convert

import System.IO.Error
{-import System.Directory-}
import System.Environment
import System.Posix.Env hiding (getEnv)

import Data.EitherR
import Control.Error.Util
import Control.Exception

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary
  shrink xs = T.pack <$> shrink (T.unpack xs)

instance CoArbitrary T.Text where
    coarbitrary = coarbitrary . T.unpack

{-# ANN specs ("HLint: ignore Redundant do"::String) #-}
{-# ANN specs ("HLint: ignore Use mappend"::String) #-}
specs :: Spec
specs = do
  let noInvalidChars k = '\NUL' `notElem` k
                      && '=' `notElem` k
                      && "C" /= k
  describe "tryIoMonoidToMaybeT" $ do
    it "Matches an equivalent implementation"
      . property . monadicIO $ do
          k <- pick arbitrary
          pre $ (not . null $ k) && noInvalidChars k
          v <- pick arbitrary
          pre $ noInvalidChars v
          run $ setEnv k v True
          tst <- run $ runMaybeT (tryIoMonoidToMaybeT (getEnv k))
          eqv <- run . runMaybeT  $  join . fmap monoidToMaybe . eitherToMaybe
                           <$> (lift . tryIOError) (getEnv k)
                           >>= hoistMaybe
          QCM.assert $ tst == eqv

    it "Handles Exceptions" . property $
      \k -> (not . null $ k) && noInvalidChars k ==> do
        runMaybeT (tryIoMonoidToMaybeT (getEnv k))
        `shouldReturn` Nothing

  describe "tryIoMonoidToEitherT" $ do
    it "Matches an equivalent implementation"
      . property . monadicIO $ do
          k <- pick arbitrary
          pre $ (not . null $ k) && noInvalidChars k
          v <- pick arbitrary
          pre $ noInvalidChars v
          run $ setEnv k v True
          tst <- run $ runEitherT (tryIoMonoidToEitherT (getEnv k))
          eqv <- run . runEitherT $ do
                  res <- lift . tryIOError $ getEnv k
                  case res of
                    Left x  -> left x
                    Right y -> hoistEither
                             $ monoidToEither (userError "empty monoid") y
          QCM.assert $ tst == eqv

    it "Handles Exceptions" . property $
      \k -> (not . null $ k) && noInvalidChars k ==> do
        res <- runEitherT (tryIoMonoidToEitherT (getEnv k))
        return $ case res of
          Left (e:: IOException) -> isDoesNotExistError e
          _                -> False
        `shouldReturn` True


  describe "tryIoET'" $ do
    it "Matches an equivalent implementation"
      . property . monadicIO $ do
          k <- pick arbitrary
          pre $ (not . null $ k) && noInvalidChars k
          v <- pick arbitrary
          pre $ noInvalidChars v
          exceptStr <- pick arbitrary
          run $ setEnv k v True
          tst <- run $ runEitherT (tryIoET' exceptStr (getEnv k))
          eqv <- run . runEitherT $ do
                  res <- lift . tryIOError $ getEnv k
                  case res of
                    Left x  -> left x
                    Right y -> hoistEither
                             $ monoidToEither (userError exceptStr) y
          QCM.assert $ tst == eqv

    it "Handles Exceptions" . property $
      \k -> (not . null $ k) && noInvalidChars k ==> do
        res <- runEitherT (tryIoET (getEnv k))
        return $ case res of
          Left (e:: IOException) -> isDoesNotExistError e
          _                -> False
        `shouldReturn` True


  describe "fmapLeftT" $ do
    it "fmapLeftT == fmapLT (1)" . property $
      \s -> (fmapLeftT T.pack (hoistEither (Left (s :: String))) :: EitherT Text [] Int)
         == fmapLT     T.pack (hoistEither (Left (s :: String)))

    it "fmapLeftT == fmapLT (2)" . property $
      \i -> (fmapLeftT fromIntegral (hoistEither (Left (i :: Int))) :: EitherT Double Maybe Int)
         == fmapLT     fromIntegral (hoistEither (Left (i :: Int)))

    it "fmapLeftT keeps Right element" . property $
      \i -> (fmapLeftT T.pack (hoistEither (Right (i :: Int))) :: EitherT Text Maybe Int)
         == fmapLT     T.pack (hoistEither (Right (i :: Int)))


  describe "fmapRightT" $ do
    it "fmapRightR == fmapRT (1)" . property $
      \s -> (fmapRightT T.pack (hoistEither (Right (s :: String))) :: EitherT Int [] Text)
         == fmapRT      T.pack (hoistEither (Right (s :: String)))

    it "fmapRightR == fmapRT (2)" . property $
      \i -> (fmapRightT fromIntegral (hoistEither (Right (i :: Int))) :: EitherT Int Maybe Double)
         == fmapRT      fromIntegral (hoistEither (Right (i :: Int)))


    it "fmapRightT keeps Left element" . property $
      \i -> (fmapRightT T.pack (hoistEither (Left (i :: Int))) :: EitherT Int Maybe Text)
         == fmapRT      T.pack (hoistEither (Left (i :: Int)))


