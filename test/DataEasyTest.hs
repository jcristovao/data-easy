{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module DataEasyTest (specs) where

import Test.Hspec
import Test.QuickCheck

import Data.Text (Text)
import qualified Data.Text as T

import Data.Monoid
{-import Data.EitherR-}
import Data.Char
import Data.Easy
import qualified Data.List as L

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


-- http://stackoverflow.com/a/9992112/516184
-- considerably speeds up mapList test
-- lists of up to 50 elements and with values [0,100]
type SmallIntList = [Int]

instance Arbitrary SmallIntList where
  arbitrary = sized $ \s -> do
                 n <- choose (2,s `min` 50)
                 xs <- vectorOf n (choose (0,100))
                 return xs

newtype EvenSizedList = EvenSizedList [Int] deriving (Eq,Show)

instance Arbitrary EvenSizedList where
  arbitrary = sized $ \s -> do
                 n <- suchThat (choose (0,s `min` 50)) even
                 xs <- vectorOf n (choose (-10000,10000))
                 return (EvenSizedList xs)
  shrink (EvenSizedList xs) = fmap EvenSizedList (shrink xs)

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
    describe "list" $ do
      it "applies a function to a non-empty list" $ do
        list "empty" concat (["abc","def"]::[String]) `shouldBe` "abcdef"
      it "returns the default value for an empty list" $ do
        list "empty" concat ([]::[String]) `shouldBe` "empty"

    describe "isFilled | notNull | isNull" $ do
      it "matches equivalent implementations" $ do
        property $ \(lst :: String) -> isFilled lst == (not . null) lst
        property $ \(lst :: String) -> isFilled lst == notNull lst
        property $ \(lst :: String) -> isFilled lst == not (isNull lst)

    describe "fromHeadNote" $ do
      {-let excpt e = case e of { PatternMatchFail _ -> True ; _ -> False }-}
      it "matches equivalent implementation" . property $
        \(lst :: String) -> notNull lst ==> fromHeadNote "error" lst == headNote "error" lst
      it "raises the specified error on an empty list" $
        evaluate (fromHeadNote "error" []) `shouldThrow` errorCall "error"

    describe "fromList" $ do
      it "matches equivalent implementation" . property $
        \(lst :: String) -> fromList 'a' lst == headDef 'a' lst

    describe "catLists" $ do
      it "filters out empty lists inside a list" $
        catLists (["","abc","","","def",""]::[String]) `shouldBe` ["abc","def"]

    describe "mapList" $ do
      let rep i = replicate i "?" :: [String]
      it "matches an equivalent implementation" . property $
        \(lst :: SmallIntList) -> mapList rep lst == (rep =<< lst)

    describe "singleton" $ do
      it "matches an equivalent implementation" . property $
        \(x::Int) -> singleton x == (:[]) x

    describe "mapF" $ do
      let functionList = [isSpace,isSeparator,isDigit]
      it "applies a list of functions to a value, and returns a list of values" $ do
        mapF ' ' functionList `shouldBe` [True,True,False]
      it "matches an equivalent implementation" . property $
        \ch -> mapF ch functionList == sequence functionList ch

    describe "nubSort" $ do
      it "matches an equivalent implementation" . property $
        \(lst :: [String]) -> nubSort lst == (L.nub . L.sort) lst

    describe "nubSort'" $ do
      it "matches an equivalent implementation" . property $
        \(lst :: [String]) -> nubSort' lst == (nonEmpty . L.nub . L.sort) lst

    describe "atLeast" $ do
      it "matches an equivalent implementation" . property $
        \((lst,loc)::([String],Positive Int)) ->
        (length lst >= getPositive loc) == atLeast (getPositive loc) lst

------------------------------------------------------------------------------
-- Tuple Pair ----------------------------------------------------------------
-- Monoid class restriction will be used when applicable ---------------------
------------------------------------------------------------------------------
  describe "Tuple Pair" $ do
    describe "pair" $ do
      it "joins the two tuple member results" . property $
        \((x,y)::(String,Int)) -> pair (++"a") show (x,y) == x ++ "a" ++ (show y)

      it "keeps only one if the other results in mempty" . property $
        \((x,y)::(String,Int)) -> pair (const "") show (x,y) == show y

    describe "pairS" $ do
      it "joins the two tuple member results" . property $
        \((x,y)::(String,String))
          -> pair (++"a") (++"b") (x,y) == x ++ "a" ++ y ++ "b"

      -- we count on QuickCheck to generate empty values in either side
      it "keeps only one if the other results in mempty" . property $
        \((x,y)::(String,String)) -> pair id id (x,y) == x ++ y

    describe "isPairNotEmpty" $ do
      let emptS = "" :: String
      let emptT = "" :: Text
      it "two empty values returns False" $
        isPairNotEmpty (emptS,emptT) `shouldBe` False
      it "one non-empty value returns True" . property $
        \((x,y) :: (String,Text)) -> not (isEmpty x && isEmpty y)
          ==> isPairNotEmpty (x,y)
      it "two non-empty values return True" . property $
        \((x,y) :: (String,Text)) -> notEmpty x && notEmpty y
          ==> isPairNotEmpty (x,y)

    describe "isPairEmpty" $ do
      it "not . isPairNotEmpty" . property $
        \((x,y) :: (String,Text)) -> isPairEmpty (x,y) == not (isPairNotEmpty (x,y))

    describe "fromPairNote" $ do
      it "joins the two non-empty tuple members" . property $
        \((x,y)::(String,String)) -> not (isEmpty x && isEmpty y)
          ==> fromPairNote "err" (x,y) == x ++ y
      it "raises the specified error on two empty tuple members " $
        evaluate (fromPairNote "error" (("","")::(String,String)))
          `shouldThrow` errorCall "error"

    describe "fromPair" $ do
      it "joins the two non-empty tuple members" . property $
        \((x,y)::(String,String)) -> not (isEmpty x && isEmpty y)
          ==> fromPair "default" (x,y) == x ++ y
      it "uses the default value on two empty tuple members" $
        fromPair "default" (("","")::(String,String)) `shouldBe` "default"

    describe "listToPairNote" $ do
      it "raises the specified error on an empty list" $
        evaluate (listToPairNote "error" []) `shouldThrow` errorCall "error"
      it "raises the specified error on an singleton list" $
        evaluate (listToPairNote "error" ([1]::[Int])) `shouldThrow` errorCall "error"
      it "convert the first two list elements into a tuple" $ do
        let lst = [1,2,3,4,5] :: [Int]
        listToPairNote "error" lst `shouldBe` (1,2)
        listToPairNote "error" (drop 3 lst) `shouldBe` (4,5)

    describe "listToPairs" $ do
      it "returns a tuple pair of empty lists on an empty list" $
        listToPairs ([]::[Int]) `shouldBe` ([],[])
      it "returns the 'remaining' element of a singleton list as the second element" $
        listToPairs ([1]::[Int]) `shouldBe` ([],[1])
      it "groups list elements two by two, with no second element, for even sized lists (excluding size zero) (1)" $ do
        listToPairs ([1,2]::[Int])    `shouldBe` ([(1,2)],[])
        listToPairs ([1,2,3,4]::[Int])`shouldBe` ([(1,2),(3,4)],[])
      it "groups list elements two by two, with no second element, for even sized lists (excluding size zero) (2)"
        . property $ \(EvenSizedList lst) -> null $ snd (listToPairs lst)
      it "groups list elements two by two, with the last list element as the second element, for odd sized lists (excluding size one) (1)" $ do
        listToPairs ([1,2,3]::[Int])    `shouldBe` ([(1,2)],[3])
        listToPairs ([1,2,3,4,5]::[Int])`shouldBe` ([(1,2),(3,4)],[5])
      it "groups list elements two by two, with the last list element as the second element, for odd sized lists (excluding size one) (2)"
        . property $ \(EvenSizedList lst) -> length (snd (listToPairs (0:lst))) == 1

    -- no point in testing out group2, it is just fst . listToPairs

    describe "pairToList" $ do
      it "inserts both elements of the tuple pair in a list" . property $
        \((x,y)::(Int,Int)) -> pairToList (x,y) == [x,y]

    describe "catPairs" $ do
      it "eliminates 'empty' pairs from list" . property $
        \(lst::[(String,String)]) -> catPairs lst
          == ( fmap (uncurry (++))
             $ filter (\(a,b) -> not (isEmpty a && isEmpty b)) lst)

    describe "mapPair" $ do
      it "works for a simple case" $ do
        let f str = ('a':str,tailSafe str)
        mapPair f ["sd","xpto"] `shouldBe` (["asdd","axptopto"] :: [String])
      it "Applies a function to each pair, and keeps only non-empty results" . property $ do
        let f str = ('a':str,tailSafe str)
        \(lst::[String]) -> mapPair f lst
          == ( fmap (uncurry (++))
             . filter (\(a,b) -> not (isEmpty a && isEmpty b))
             . fmap f) lst

    describe "pairToEither" $ do
      it "keeps the second pair value if not empty" . property $
        \((a,b)::(Int,String)) -> let
          f = fromEither "abc" . pairToEither $ (a,b)
          in f == "abc" || f == b
      it "keeps the first element for empty values" $ do
        pairToEither (0,(""::String)) `shouldBe` (Left 0 :: Either Int String)
        pairToEither (0,(""::Text  )) `shouldBe` (Left 0 :: Either Int Text  )

    describe "pairToEither'" $ do
      it "keeps the first pair value if not empty" . property $
        \((a,b)::(Int,String)) -> let
          f = fromRight ("abc"::String) . pairToEither' $ (b,a)
          in f == "abc" || f == b
      it "keeps the first element for empty values" $ do
        pairToEither' ((""::String),0) `shouldBe` (Left 0 :: Either Int String)
        pairToEither' ((""::Text  ),0) `shouldBe` (Left 0 :: Either Int Text)

    describe "pairBothToEither" $ do
      it "keeps the first non-empty monoid value" . property $
        \((a,b)::(String,String)) -> not (isNull a && isNull b) ==>
        pairBothToEither ("!?"::String) (a,b) == Right (if isNull a then b else a)
      it "returns the default value as Left value if both pair members are mempty" $ do
        pairBothToEither "!?" (("","")::(String, String))
          `shouldBe` Left ("!?"::String)
        pairBothToEither "!?" (("","")::(Text  , Text  ))
          `shouldBe` Left ("!?"::Text)

    describe "eitherToPair" $ do
      it "transforms a Left value into a (left,mempty) pair" $
        eitherToPair 0 ((Left 1)::Either Int Text)
          `shouldBe` ((1,"")::(Int,Text))
      it "transforms a Right value into a (def,right) pair" $
        eitherToPair 0 ((Right "abc")::Either Int Text)
          `shouldBe` ((0,"abc")::(Int,Text))


    describe "eitherToPair'" $ do
      it "transforms a Left value into a (mempty,left) pair" $
        eitherToPair' 0 ((Left 1)::Either Int Text)
          `shouldBe` (("",1)::(Text,Int))
      it "transforms a Right value into a (def,right) pair" $
        eitherToPair' 0 ((Right "abc")::Either Int Text)
          `shouldBe` (("abc",0)::(Text,Int))

    describe "pairToMaybe" $ do
      it "keeps the first non-empty monoid value, starting by the second pair element"
        . property $ \((a,b)::(String,String)) -> not (isNull a && isNull b) ==>
          pairToMaybe (a,b) == Just (if isNull b then a else b)
      it "returns Nothing if both are mempty" $
        pairToMaybe (("","")::(String,String)) `shouldBe` (Nothing :: Maybe String)

    describe "pairToMaybe'" $ do
      it "keeps the first non-empty monoid value, starting by the first pair element"
        . property $ \((a,b)::(String,String)) -> not (isNull a && isNull b) ==>
          pairToMaybe'(a,b) == Just (if isNull a then b else a)
      it "returns Nothing if both are mempty" $
        pairToMaybe'(("","")::(String,String)) `shouldBe` (Nothing :: Maybe String)

    describe "pairFstToMaybe" $ do
      it "extracts the first element and applies monoidToMaybe" . property $
        \((a,b)::(String,Int)) ->
          pairFstToMaybe (a,b) == if isEmpty a then Nothing else Just a

    describe "pairSndToMaybe" $ do
      it "extracts the last element and applies monoidToMaybe" . property $
        \((a,b)::(Int,String)) ->
          pairSndToMaybe (a,b) == if isEmpty b then Nothing else Just b

    describe "maybeToPair" $ do
      it "tranforms a Just value x into (default,x)" . property $
        \(a::String) ->
          maybeToPair (1::Int) (Just a) == (1,a)
      it "transforms a Nothing value into (default,mempty) (string)" $
        maybeToPair (1::Int) Nothing == ((1,"")::(Int,String))
      it "transforms a Nothing value into (default,mempty) (Text)" $
        maybeToPair (1::Int) Nothing == ((1,"")::(Int,Text))

    describe "maybeToPair'" $ do
      it "tranforms a Just value x into (x,default)" . property $
        \(a::String) ->
          maybeToPair' (1::Int) (Just a) == (a,1)
      it "transforms a Nothing value into (default,mempty) (string)" $
        maybeToPair' (1::Int) Nothing == (("",1)::(String,Int))
      it "transforms a Nothing value into (default,mempty) (Text)" $
        maybeToPair' (1::Int) Nothing == (("",1)::(Text,Int))

    describe "pairToMonoid" $ do
      it "finds the first non-empty monoid in the pair, and returns it"
        . property $ \((a,b)::(String,String)) ->
          pairToMonoid (a,b) == if isEmpty a then b else a

    describe "pairToMonoid'" $ do
      it "finds the last non-empty monoid in the pair, and returns it"
        . property $ \((a,b)::(String,String)) ->
          pairToMonoid' (a,b) == if isEmpty b then a else b

------------------------------------------------------------------------------
-- Monoid --------------------------------------------------------------------
------------------------------------------------------------------------------

  describe "Monoid" $ do
    describe "monoid" $ do
      it "returns the result of the provided function if the value is not empty"
        . property $ \(s::String) -> isNotEmpty s ==>
        monoid (-1) Prelude.length s == Prelude.length s

      it "returns the default value on an empty value" $
        monoid (-1) Prelude.length (""::String) `shouldBe` (-1)

    describe "isNotEmpty" $ do
      it "it checks a monoit is not empty" $ do
        property $ \(s::String) -> (not . null) s ==> isNotEmpty s
        property $ \(s::Text  ) -> (not . T.null) s ==> isNotEmpty s
        property $ \(s::[Int] ) -> (not . null) s ==> isNotEmpty s
      it "it returns False on an empty monoid" $ do
        (""::String) `shouldSatisfy` null
        (""::Text  ) `shouldSatisfy` T.null
        ([]::[Int] ) `shouldSatisfy` null

    describe "notEmpty" .
      it "it matches an equivalent implementation" . property
      $ \(s::Text) -> notEmpty s == isNotEmpty s

    describe "isEmpty" $ do
      it "matches an equivalent implementation" $ do
        property $ \(s::Text)   -> isEmpty s == T.null s
        property $ \(s::String) -> isEmpty s == null s
        property $ \(s::[Int])  -> isEmpty s == null s
      it "is the logical negation of isNotEmpty" $ do
        property $ \(s::Text)   -> isEmpty s == (not . isNotEmpty) s

    describe "fromNotEmptyNote" $ do
      it "returns the value from an not empty monoit" $ do
        property $ \(s::Text)   -> isNotEmpty s ==> fromNotEmptyNote "!?" s == s
        property $ \(s::String) -> isNotEmpty s ==> fromNotEmptyNote "!?" s == s
        property $ \(s::[Int])  -> isNotEmpty s ==> fromNotEmptyNote "!?" s == s

      it "raises the specified error on an empty monoid" $ do
        evaluate (fromNotEmptyNote "error" (""::String))
          `shouldThrow` errorCall "error"
        evaluate (fromNotEmptyNote "error" (""::Text))
          `shouldThrow` errorCall "error"

    describe "fromMonoid" $ do
      it "returns the value from an not empty monoid" $ do
        property $ \(s::Text)   -> isNotEmpty s ==> fromMonoid "!?" s == s
        property $ \(s::String) -> isNotEmpty s ==> fromMonoid "!?" s == s
        property $ \(s::[Int])  -> isNotEmpty s ==> fromMonoid [1] s == s
      it "returns the provided default value on an empty monoid" $ do
        fromMonoid "!?" (""::Text  ) `shouldBe` "!?"
        fromMonoid "!?" (""::String) `shouldBe` "!?"
        fromMonoid [1]  ([]::[Int])  `shouldBe` [1]

    describe "?+" $ do
      it "returns the value from an not empty monoid" $ do
        property $ \(s::Text)   -> isNotEmpty s ==> (s ?+ "!?") == s
        property $ \(s::String) -> isNotEmpty s ==> (s ?+ "!?") == s
        property $ \(s::[Int])  -> isNotEmpty s ==> (s ?+ [1] ) == s
      it "returns the provided default value on an empty monoid" $ do
        ((""::Text  ) ?+ "!?") `shouldBe` "!?"
        ((""::String) ?+ "!?") `shouldBe` "!?"
        (([]::[Int])  ?+ [1] ) `shouldBe` [1]

    describe "<!>" $ do
      it "returns the first non empty monoid value" $ do
        let x = "abc" :: Text
            y = ""    :: Text
            z = "def" :: Text
        x <!> y <!> z `shouldBe` x
        y <!> z       `shouldBe` z
        z <!> y       `shouldBe` z
        x <!> z <!> y `shouldBe` x
        y <!> y       `shouldBe` mempty

    describe "listToMonoid" $ do
      it "extracts the first element from a monoid list" $ do
        listToMonoid (["abc","def"]::[String]) `shouldBe` "abc"
        listToMonoid (["abc","def"]::[Text  ]) `shouldBe` "abc"
        listToMonoid ([[1,2,3],[5,6]]::[[Int]])`shouldBe` [1,2,3]
      it "returns mempty on an empty list" $ do
        listToMonoid ([]::[String]) `shouldBe` ""
        listToMonoid ([]::[Text  ]) `shouldBe` ""
        listToMonoid ([]::[[Int] ]) `shouldBe` []

    describe "monoidToList" $ do
      it "converts a monoid value into a singleton list featuring that monoid" $ do
        monoidToList ("abc"::String) `shouldBe` ["abc"]
        monoidToList ("abc"::Text  ) `shouldBe` ["abc"]
        monoidToList ([1,2]::[Int] ) `shouldBe` [[1,2]]
      it "converts an empty monoid value into an empty list" $ do
        monoidToList (""::String) `shouldBe` []
        monoidToList (""::Text  ) `shouldBe` []
        monoidToList ([]::[Int] ) `shouldBe` []


    describe "catMonoids" $ do
      it "matches an equivalent implementation" . property $
        \(sLst :: [String]) -> catMonoids sLst == filter (not.null) sLst
      it "returns an empty list on a list of empty monoids" $
        catMonoids (["","",""]::[Text]) `shouldBe` []

    describe "nonEmpty" $ do
      it "matches an equivalent implementations" . property $
        \(tLst :: [Text]) -> nonEmpty tLst == filter (not . T.null) tLst

    describe "getFirst'" $ do
      it "gets the first non-empty element from a list featuring non-empty elements" $ do
        getFirst' (["abc","def"] :: [String]) `shouldBe` "abc"
        getFirst' (["","def"] :: [String])    `shouldBe` "def"
        getFirst' (["abc"] :: [String])       `shouldBe` "abc"

      it "returns mempty for an empty list" $ do
        getFirst' ([] :: [String]) `shouldBe` ""

      it "returns mempty for an list of empty values" $ do
        getFirst' (["",""] :: [String]) `shouldBe` ""

      it "is lazy, does not crash on infinite lists" $ do
        getFirst' (fmap (const ("abc"::String)) ([1..]::[Int])) `shouldBe` "abc"

    describe "getLast'" $ do
      it "gets the last non-empty element from a list featuring non-empty elements" $ do
        getLast' (["abc","def"] :: [String]) `shouldBe` "def"
        getLast' (["def",""] :: [String])    `shouldBe` "def"
        getLast' (["abc"] :: [String])       `shouldBe` "abc"

      it "returns mempty for an empty list" $ do
        getLast' ([] :: [String]) `shouldBe` ""

      it "returns mempty for an list of empty values" $ do
        getLast' (["",""] :: [String]) `shouldBe` ""

    describe "headF" $ do
      it "returns the first element of a foldable structure of monoids" $ do
        headF (["abc","def"]::[String])     `shouldBe` "abc"
        headF (["","def"   ]::[Text  ])     `shouldBe` ""
        headF ((Just "abc") ::Maybe String) `shouldBe` "abc"

      it "returns mempty if the structure is empty" $ do
        headF ([]::[String]) `shouldBe` ""
        headF ([]::[Text])    `shouldBe` ""
        headF (Nothing :: Maybe String) `shouldBe` ""

      it "is lazy, does not crash on infinite lists" $ do
        headF (fmap (const ("abc"::String)) ([1..]::[Int])) `shouldBe` "abc"

  describe "lastF" $ do
    it "returns the last element of a foldable structure of monoids" $ do
      lastF (["abc","def"]::[String])   `shouldBe` "def"
      lastF (["def",""]   ::[Text])     `shouldBe` ""
      lastF ((Just "abc") ::Maybe Text) `shouldBe` "abc"

    it "returns mempty if the structure is empty" $ do
      lastF ([]::[String]) `shouldBe` ""
      lastF ([]::[Text])    `shouldBe` ""
      lastF (Nothing :: Maybe String) `shouldBe` ""

  describe "atF" $ do
    it "returns the element at the provided index for appropriate size foldable structures of monoids" $ do
      (["0","1","2"]::[String]) `atF` 0 `shouldBe` "0"
      (["0","1","2"]::[Text  ]) `atF` 1 `shouldBe` "1"
      (Just "abc"::Maybe String)`atF` 0 `shouldBe` "abc"

    it "returns mempty for an empty structure" $ do
      ([]::[String]) `atF` 0 `shouldBe` ""
      ([]::[Text  ]) `atF` 0 `shouldBe` ""
      (Nothing::Maybe Text) `atF` 0 `shouldBe` ""

    it "handles invalid indexes" $ do
      (["1"]::[String]) `atF` 1    `shouldBe` ""
      (["1"]::[String]) `atF` (-1) `shouldBe` ""

  describe "@@" $ do
    it "returns the element at the provided index for appropriate size foldable structures of monoids" $ do
      (["0","1","2"]::[String])  @@ 0 `shouldBe` "0"
      (["0","1","2"]::[Text  ])  @@ 1 `shouldBe` "1"
      (Just "abc"::Maybe String) @@ 0 `shouldBe` "abc"

    it "returns mempty for an empty structure" $ do
      ([]::[String]) @@ 0 `shouldBe` ""
      ([]::[Text  ]) @@ 0 `shouldBe` ""
      (Nothing::Maybe Text) @@ 0 `shouldBe` ""


    it "handles invalid indexes" $ do
      (["1"]::[String]) `atF` 1    `shouldBe` ""
      (["1"]::[String]) `atF` (-1) `shouldBe` ""

------------------------------------------------------------------------------
-- Boolean -------------------------------------------------------------------
------------------------------------------------------------------------------

  describe "Bool" $ do
    describe "fromBool" $ do
      it "returns the provided value if the boolean is true" $ do
        fromBool 'a' True  'z' `shouldBe` 'z'
      it "returns the default value if the boolean is False" $ do
        fromBool 'a' False 'z' `shouldBe` 'a'

    describe "fromBoolC" $ do
      it "returns the provided value if the bool returning function returns True once aplied to the value" $ do
        fromBoolC 'z' isDigit '0' `shouldBe` '0'
      it "returns the default value if the bool returning function returns False once aplied to the value" $ do
        fromBoolC 'z' isSpace '0' `shouldBe` 'z'

    describe "catBools" $ do
      it "returns a list with the True values. Mostly useless" $
        catBools [True,False,False,True,False] `shouldBe` [True,True]

    describe "?" $ do
      it "behaves like if" . property $
        \(s::String) -> (isNull s ? "!?" $ s) == if isNull s then "!?" else s

    describe "?$" $ do
      it "behaves like if" . property $
        \(s::String) -> (notEmpty ?$ s $ "!?") == if notEmpty s then s else "!?"

    describe "?|" $ do
      it "behaves like if" . property $
        \(s::String) -> (s ?| notEmpty $ "!?") == if notEmpty s then s else "!?"

    describe "boolToMaybe" $ do
      it "keeps a value in a Just for a True boolean" $ do
        boolToMaybe (1::Int) True `shouldBe` Just 1
      it "returns Nothing for a False boolean" $ do
        boolToMaybe (1::Int) False `shouldBe` Nothing

    describe "ifToMaybe" $ do
      it "keeps a value in a Just for a True boolean" $ do
        ifToMaybe True (1::Int) `shouldBe` Just 1
      it "returns Nothing for a False boolean" $ do
        ifToMaybe False (1::Int) `shouldBe` Nothing

    describe "boolCToMaybe" $ do
      it "keeps a value in a Just for if the provided value test returns True" $ do
        boolCToMaybe (1::Int) (>0) `shouldBe` Just 1
      it "returns Nothing if the provided value test returns False" $ do
        boolCToMaybe (1::Int) (<0) `shouldBe` Nothing

    describe "ifCToMaybe" $ do
      it "keeps a value in a Just for if the provided value test returns True" $ do
        ifCToMaybe (>0) (1::Int) `shouldBe` Just 1
      it "returns Nothing if the provided value test returns False" $ do
        ifCToMaybe (<0) (1::Int) `shouldBe` Nothing

    describe "boolToEither" $ do
      it "Returns the second value if the boolean value is True" $
        boolToEither ("aaa"::Text) (1::Int) True  `shouldBe` Right 1
      it "Returns the first  value if the boolean value is False" $
        boolToEither ("aaa"::Text) (1::Int) False `shouldBe` Left "aaa"

    describe "boolCToEither" $ do
      it "Returns the second value if the boolean test on the second value returns True" $
        boolCToEither ("aaa"::Text) (1::Int) (>0) `shouldBe` Right 1
      it "Returns the first  value if the boolean test on the second value returns False" $
        boolCToEither ("aaa"::Text) (1::Int)  (<0) `shouldBe` Left "aaa"

    describe "boolToList" $ do
      it "Returns a singleton list if the boolean value is True" $
        boolToList ("aaa"::Text) True `shouldBe` ["aaa"]
      it "Returns an empty list if the boolean value is False" $
        boolToList ("aaa"::Text) False `shouldBe` []

    describe "boolCToList" $ do
      it "Returns a singleton list if the boolean test on the value returns True" $
        boolCToList ("aaa"::Text) isNotEmpty `shouldBe` ["aaa"]
      it "Returns an empty list if the boolean test on the value returns False" $
        boolCToList ("aaa"::Text) isEmpty    `shouldBe`[]

    describe "boolToMonoid" $ do
      it "Returns the provided value if boolean value is True" $
        boolToMonoid ("aaa"::Text) True `shouldBe` "aaa"
      it "Returns an empty value if the boolean value is False" $
        boolToMonoid ("aaa"::Text) False `shouldBe` ""

    describe "boolCToMonoid" $ do
      it "Returns a singleton list if the boolean test on the value returns True" $
        boolCToMonoid ("aaa"::Text) isNotEmpty `shouldBe` "aaa"
      it "Returns an empty list if the boolean test on the value returns False" $
        boolCToMonoid ("aaa"::Text) isEmpty    `shouldBe` ""

    describe "?&&" $ do
      it "handles `pseudo-composition` of boolean values" $ do
        (("aaa"::String) ?&& True ?&& True  ?&& True) `shouldBe` "aaa"
        (("aaa"::String) ?&& True ?&& False ?&& True) `shouldBe` ""

    describe "?&&\\" $ do
      it "handles `pseudo-composition` of tests of provided value" $ do
        (("123"::String) ?&&\ not . null ?&&\ (\s -> length s > 2)) `shouldBe` "123"
        (("12" ::String) ?&&\ not . null ?&&\ (\s -> length s > 2)) `shouldBe` ""

    describe "allCond" $ do
      it "returns True if all the tests on a variable return true" $ do
        allCond '1' [isAlphaNum,isDigit,isPrint] `shouldBe` True
      it "returns False if one of the tests on the variable fails" $ do
        allCond '1' [isAlphaNum,isSpace,isPrint] `shouldBe` False


    describe "anyCond" $ do
      it "returns True if any of the tests on the variable succeeds" $ do
        anyCond '1' [isSpace,isDigit,isAlpha] `shouldBe` True
      it "returns False if none of the tests on the variable succeeds" $ do
        anyCond '1' [isSpace,isLower,isAlpha] `shouldBe` False

------------------------------------------------------------------------------
-- Functor -------------------------------------------------------------------
------------------------------------------------------------------------------

  describe "Functor" $ do
    describe "for" $ do
      it "behaves like flipped for" . property $
        \(iLst::[Int]) -> fmap (+1) iLst == for iLst (+1)






