{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module DataEasyTest (specs) where

import Test.Hspec
import Test.QuickCheck

import Data.Text (Text)
import qualified Data.Text as T

{-import Data.Monoid-}
{-import Data.EitherR-}
import Data.Easy

import Control.Applicative
import Control.Exception
{-import Control.Error.Util-}

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary
  shrink xs = T.pack <$> shrink (T.unpack xs)

instance CoArbitrary T.Text where
    coarbitrary = coarbitrary . T.unpack

-- Custom data type just to avoid further type annotations
data Custom = X | Y
  deriving (Eq,Ord,Show)

{-# ANN specs ("HLint: ignore Redundant do"::String) #-}
{-# ANN specs ("HLint: ignore Use mappend"::String) #-}
specs :: Spec
specs = do
------------------------------------------------------------------------------
-- Maybe ---------------------------------------------------------------------
------------------------------------------------------------------------------
  describe "Maybe" $ do
    describe "maybeToMonoid" $ do
      it "converts a Nothing value to mempty" $ do
        ((maybeToMonoid Nothing) :: [Int])  `shouldBe` []
        ((maybeToMonoid Nothing) :: [Text]) `shouldBe` []
        ((maybeToMonoid Nothing) :: Text  ) `shouldBe` T.empty
        ((maybeToMonoid Nothing) :: String) `shouldBe` ""

      it "converts a Just value to a non-empty value" $ do
        ((maybeToMonoid (Just ([5]    :: [Int])))  :: [Int])  `shouldBe` [5]
        ((maybeToMonoid (Just (["abc"]:: [Text]))) :: [Text]) `shouldBe` ["abc"]
        ((maybeToMonoid (Just ("abc"  :: Text)))   :: Text)   `shouldBe` "abc"
        ((maybeToMonoid (Just ("abc"  :: String))) :: String) `shouldBe` "abc"


    describe "monoidToMaybe" $ do
      it "converts a mempty value to a Nothing" $ do
        (monoidToMaybe ([] :: [Int])) `shouldBe` Nothing
        (monoidToMaybe ([] :: [Text]))`shouldBe` Nothing
        monoidToMaybe T.empty         `shouldBe` Nothing
        monoidToMaybe ("" :: String)  `shouldBe` Nothing

    describe "monoidToMaybe" $ do
      it "converts a non-empty value to a Just" $ do
        (monoidToMaybe ([5]    :: [Int]))   `shouldBe` Just [5]
        (monoidToMaybe (["abc"]:: [Text]))  `shouldBe` Just ["abc"]
        (monoidToMaybe ("abc"  :: Text))    `shouldBe` Just "abc"
        (monoidToMaybe ("abc"  :: String))  `shouldBe` Just "abc"

------------------------------------------------------------------------------
-- Either --------------------------------------------------------------------
------------------------------------------------------------------------------
  describe "Either" $ do
    describe "fromRightNote" $ do
      it "extracts a Right value" $ do
        fromRightNote "error" (Right 1   :: Either String Int ) `shouldBe` 1
      it "raises the specified error for a Left value" $ do
        evaluate (fromRightNote "error" (Left "?!" :: Either String Int ))
          `shouldThrow` errorCall "error"

    describe "fromLeftNote" $ do
      it "extracts a Left value" $ do
        fromLeftNote "error" (Left 1   :: Either Int String ) `shouldBe` 1
      it "raises the specified error for a Left value" $ do
        evaluate (fromLeftNote "error" (Right "?!" :: Either Int String ))
          `shouldThrow` errorCall "error"

    describe "fromEither" $ do
      it "extracts a Right value" $ do
        fromEither 0 (Right 1 :: Either String Int) `shouldBe` 1
      it "returns a default value for a Left value" $ do
        fromEither 0 (Left "?!" :: Either String Int) `shouldBe` 0

    describe "listToEither" $ do
      it "extracts the first element of a list as a Right value" $ do
        listToEither ("!?" ::String) ([1,2,3]::[Int]) `shouldBe` Right 1
      it "uses the default value provided as a Left value for an empty list" $ do
        listToEither ("!?" ::String) ([]::[Int]) `shouldBe` Left "!?"

    describe "eitherToList" $ do
      it "extracts a Right value into a singleton list" $ do
        eitherToList (Right 1 :: Either String Int) `shouldBe` [1]
      it "converts an Left value into an empty list" $ do
        eitherToList (Left "?!" :: Either String Int) `shouldBe` []

    -- just for completeness, but this is stupid, since the functions
    -- are defined as the same
    describe "catEithers" $ do
      it "behaves like righs in the Data.Either" . property
        $ \(eLst :: [Either Int String]) -> catEithers eLst == rights eLst

    describe "mapEither" $ do
      it "applies an either returning function to a list, and keeps only the right elements, extracted" $ do
        mapEither (\x -> if odd x then Left x else Right x) ([1,2,3,4,5,6,7]::[Int])
        `shouldBe` [2,4,6]

    describe "maybeToEither" $ do
      it "converts a Nothing value into the provided default as Left value" $ do
        (maybeToEither "?!" (Nothing :: Maybe Int) :: Either String Int)
        `shouldBe` Left "?!"
      it "converts a Just value into a Right value" $ do
        (maybeToEither "?!" (Just 5 :: Maybe Int) :: Either String Int)
        `shouldBe` Right 5

    describe "eitherToMaube" $ do
      it "converts a Left value into a Nothing" $
        (eitherToMaybe (Left "?!"::Either String Int)) `shouldBe` Nothing
      it "converts a Right value into a Just value" $
        (eitherToMaybe (Right 5 :: Either String Int)) `shouldBe` Just 5

    describe "eitherToMonoid" $ do
      it "converts a Left value into a mempty" $ do
        (eitherToMonoid (Left 1 :: Either Int String)) `shouldBe` ""
        (eitherToMonoid (Left 1 :: Either Int Text  )) `shouldBe` T.empty
        (eitherToMonoid (Left 1 :: Either Int [Text])) `shouldBe` []
      it "extracts the right monoid value" $ do
        (eitherToMonoid (Right "abc"  :: Either Int String)) `shouldBe` "abc"
        (eitherToMonoid (Right "abc"  :: Either Int Text  )) `shouldBe` "abc"
        (eitherToMonoid (Right ["abc"]:: Either Int [Text])) `shouldBe` ["abc"]

    describe "monoidToEither" $ do
      it "converts an mempty value into a Left value" $ do
        monoidToEither X (""   :: String) `shouldBe` Left X
        monoidToEither X (""   :: Text  ) `shouldBe` Left X
        monoidToEither X ([]   :: [Text]) `shouldBe` Left X
      it "converts a non-empty value into a Right value" $ do
        monoidToEither X ("abc"   :: String) `shouldBe` Right "abc"
        monoidToEither X ("abc"   :: Text  ) `shouldBe` Right "abc"
        monoidToEither X (["abc"] :: [Text]) `shouldBe` Right ["abc"]

    describe "joinEitherMonoid" $ do
      it "keeps a Left value" $ do
        joinEitherMonoid "err" (Left "abc" :: Either String Text )
          `shouldBe` Left "abc"
        joinEitherMonoid "err" (Left "" :: Either String Text)
          `shouldBe` Left ""
      it "keeps a non-empty Right value" $ do
        joinEitherMonoid "err" (Right "abc" :: Either String Text)
          `shouldBe` Right "abc"
      it "replaces an mempty Right value with the provided default value as a Left value" $ do
        joinEitherMonoid "err" (Right "" :: Either String Text)
          `shouldBe` Left "err"



------------------------------------------------------------------------------
-- List ----------------------------------------------------------------------
------------------------------------------------------------------------------
  describe "List" $ do
    describe "getFirst'" $ do
      it "gets the first non-empty element from a list featuring non-empty elements" $ do
        getFirst' (["abc","def"] :: [String]) `shouldBe` "abc"
        getFirst' (["","def"] :: [String])    `shouldBe` "def"
        getFirst' (["abc"] :: [String])       `shouldBe` "abc"

      it "returns mempty for an empty list" $ do
        getFirst' ([] :: [String]) `shouldBe` ""


      it "returns mempty for an list of empty values" $ do
        getFirst' (["",""] :: [String]) `shouldBe` ""
