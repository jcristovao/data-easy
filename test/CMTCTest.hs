{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module CMTCTest (specs) where

import Control.Applicative
import Control.Exception
import Control.Monad.Trans.Either

import Test.Hspec
import Test.QuickCheck

import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf

import Data.UniformUtils
import Control.Monad.Trans.Convert

import Data.EitherR
import Control.Error.Util

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary
  shrink xs = T.pack <$> shrink (T.unpack xs)

instance CoArbitrary T.Text where
    coarbitrary = coarbitrary . T.unpack

{-# ANN specs ("HLint: ignore Redundant do"::String) #-}
specs :: Spec
specs = do
  describe "fmapLeftT" $ do
    it "fmapLeftT == fmapLT (1)" $ property $
      \s -> (fmapLeftT T.pack (hoistEither (Left (s :: String))) :: EitherT Text [] Int)
         == fmapLT     T.pack (hoistEither (Left (s :: String)))

    it "fmapLeftT == fmapLT (2)" $ property $
      \i -> (fmapLeftT fromIntegral (hoistEither (Left (i :: Int))) :: EitherT Double Maybe Int)
         == fmapLT     fromIntegral (hoistEither (Left (i :: Int)))

    it "fmapLeftT keeps Right element" $ property $
      \i -> (fmapLeftT T.pack (hoistEither (Right (i :: Int))) :: EitherT Text Maybe Int)
         == fmapLT     T.pack (hoistEither (Right (i :: Int)))


  describe "fmapRightT" $ do
    it "fmapRightR == fmapRT (1)" $ property $
      \s -> (fmapRightT T.pack (hoistEither (Right (s :: String))) :: EitherT Int [] Text)
         == fmapRT      T.pack (hoistEither (Right (s :: String)))

    it "fmapRightR == fmapRT (2)" $ property $
      \i -> (fmapRightT fromIntegral (hoistEither (Right (i :: Int))) :: EitherT Int Maybe Double)
         == fmapRT      fromIntegral (hoistEither (Right (i :: Int)))


    it "fmapRightT keeps Left element" $ property $
      \i -> (fmapRightT T.pack (hoistEither (Left (i :: Int))) :: EitherT Int Maybe Text)
         == fmapRT      T.pack (hoistEither (Left (i :: Int)))

  {-it "Implements fromList function" $ do-}
    {-fromList ("a"::String) "b"    `shouldBe` "b"-}
    {-fromList ("a"::String) ""     `shouldBe` "a"-}
    {-fromList ("" ::String) "b"    `shouldBe` "b"-}
    {-fromList ("" ::String) ""     `shouldBe` ""-}

  {-it "Implements singleton' function" $ do-}
    {-singleton' 'a' `shouldBe` "a"-}

  {-it "Implements listToEither function" $ do-}
    {-listToEither "abc" ([]::[Int]) `shouldBe` Left "abc"-}
    {-listToEither "abc" [1,2,3]     `shouldBe` Right (1::Int)-}

  {-it "Implements lfmap function" $ do-}
    {-let lv = Left 1  :: Either Int Int-}
    {-let rv = Right 1 :: Either Int Int-}
    {-lfmap (+1) lv `shouldBe` Left (2::Int)-}
    {-lfmap (+1) rv `shouldBe` Right(1::Int)-}

  {-it "Implements fromJustNote function" $ do-}
    {-fromJustNote "abc" (Just 1) `shouldBe` (1::Int)-}
    {-evaluate (fromJustNote "abc" Nothing)  `shouldThrow` errorCall "abc"-}

  {-it "Implements toTmuxFormat function (2)" $-}
    {-property $ \s -> (head . toTmuxFormat $ s) == '\t'-}

