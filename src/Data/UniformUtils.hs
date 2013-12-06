{-# LANGUAGE NoMonomorphismRestriction #-}
-- | UniformUtils aims to make either, list, tuple, monoid and bool counterparts
-- to the functions originally defined in "Data.Maybe", whenever applicable.
--
-- Most functions of "Data.Maybe" are re-exported, so you may import just this
-- module instead. The only exception(s) are partial functions such as
-- fromJust. Here, the safer alternatives from the "safe" package are
-- prefered instead.
--
-- Related functions always follow the unwritten convention of providing
-- the default value first, except where dully noted.
--
-- This module also adds some extra functions I found usefull, and that
-- can be found in otherwise disperse packages or pages. A relevant link
-- will be included whenever appropriate.
--
-- Some choices have been made, and I am open to discussion whether they
-- are adequate or not. Please contribute and help me make this a (even)
-- more sane module.
module Data.UniformUtils
  ( -- * Module exports
    module Data.Maybe
  , module Data.Either
  , module Data.Either.Combinators
  , module Data.Tuple
  , module Data.Ord
  , module Data.Function
  , module Safe

  -- * Additional functions

  -- ** Maybe
  -- | Since this module maps the "Data.Maybe" functions to other data types,
  -- we mainly just import (and re-export) this module.
  -- The extra functions are dedicated to conversions to other types.
  , maybeToMonoid
  , monoidToMaybe

  -- ** Either
  -- | Many of the functions are already defined in either "Data.Either" or
  -- "Data.Either.Combinators" from the "either" package.
  , fromRightNote
  , fromLeftNote
  , fromEither
  , listToEither
  , eitherToList
  , maybeToEither
  , eitherToMaybe
  , catEithers
  , mapEither
  , eitherToMonoid
  , monoidToEither

  -- ** List
  , list
  , isFilled
  , notNull
  , isNull
  , fromHeadNote
  , fromList
  , catLists
  , mapList
  , singleton
  , mapF
  , nubSort
  , nubSort'

  -- ** Tuple Pairs
  -- | Monoid class restriction will be used when applicable.
  , pair
  , pairS
  , isFstTrue
  , isFstFalse
  , isSndTrue
  , isSndFalse
  , fromFst
  , fromSnd
  , fromPair
  , listToPairNote
  , listToPairs
  , group2
  , pairToList
  , catPairs
  , mapPair
  , pairToEither
  , pairToEither'
  , eitherToPair
  , eitherToPair'
  , pairToMaybe
  , pairToMaybe'
  , maybeToPair
  , maybeToPair'

  -- ** Tuple Triples
  -- | Monoid class restriction will be used when applicable.
  -- /Note/: uses ' to distinguish from tuple pairs. This clearly doesn't scale.
  -- But if you intent to use bigger tuples, you probably should be using a better
  -- library than this, no? See <http://hackage.haskell.org/package/lens>.
  , triple
  , tripleS
  , isFst'True
  , isFst'False
  , isSnd'True
  , isSnd'False
  , isTrd'True
  , isTrd'False
  , fromFst'
  , fromSnd'
  , fromTrd'
  , trd'
  , fromTriple
  , listToTripleNote
  , listToTriples
  , group3
  , tripleToList
  , catTriples
  , mapTriple
  , toFstPairToTriple
  , toSndPairToTriple
  , toTrdPairToTriple
  , pairToTriple
  , dropFstTripleToPair
  , dropSndTripleToPair
  , dropTrdTripleToPair
  , tripleToPair

  -- ** Monoid
  -- | The monoid version of the functions
  -- deviate slightly from the others, in the sense no value is extracted
  -- from or promoted to a monoid. Instead, the value is just checked against
  -- mempty, and kept|discarded|operated on accordingly.
  -- See "monoid-subclasses" module on hackage for a perhaps saner approach.
  , monoid
  , isNotEmpty
  , isEmpty
  , fromNotEmptyNote
  , fromMonoid
  , (?+)
  , listToMonoid
  , monoidToList
  , catMonoids
  , nonEmpty
  , mapMonoid
  , getFirst'
  , getLast'

  -- ** Bool
  -- | Some extra functions included, namely the simplified ternary operator
  -- modified from what is seen in <https://gist.github.com/Burgestrand/218987>
  --
  -- /Note/: This is probably not considered good practice.
  -- Use the standard @if-then-else@ instead, its almost always clearer.
  -- /You have been warned./
  --
  , fromBool
  , fromBoolC
  , catBools
  , (?)
  , (?$)
  , (?|)
  , boolToMaybe
  , ifToMaybe
  , boolCToMaybe
  , ifCToMaybe
  , allCond
  , anyCond
  ) where

import Data.Maybe
import Data.Either
import Data.Either.Combinators
import Data.Tuple
import Data.Ord
import Data.Function
import Data.Monoid
import Safe
import qualified Data.List as L
import qualified Data.Set as Set

------------------------------------------------------------------------------
-- Maybe ---------------------------------------------------------------------
------------------------------------------------------------------------------
-- | Maybe to monoid conversion
maybeToMonoid :: (Monoid a) => Maybe a -> a
maybeToMonoid Nothing = mempty
maybeToMonoid (Just x) = x

-- | Convert a monoid value into a maybe value (Nothing if mempty).
--
-- > monoidToMaybe = monoid Nothing Just
monoidToMaybe :: (Eq a, Monoid a) => a -> Maybe a
monoidToMaybe = monoid Nothing Just


------------------------------------------------------------------------------
-- Either --------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Force a right value, or otherwise fail with provided error message
fromRightNote :: String -> Either a b -> b
fromRightNote err = either (error err) id

-- | Force a left value, or otherwise fail with provided error message
fromLeftNote :: String -> Either a b -> a
fromLeftNote err = either id (error err)

-- | Force a right value, providing a default value if the Either is Left
fromEither :: b -> Either a b -> b
fromEither = fromRight

-- | Extract the first element of a list as a Right value, or else use the
-- default value provided as a Left value
listToEither :: a -> [b] -> Either a b
listToEither def = maybeToEither def . listToMaybe

-- | Extracts the right value of an either to a singleton list, or an
-- empty list if the Either value is a Left
eitherToList :: Either a b -> [b]
eitherToList = either (const []) singleton

-- | Convert a Maybe value to an Either value, with the provided default used
-- as Left value if the Maybe value is Nothing
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither def = maybe (Left def) Right

-- | Convert an Either value to a Maybe value
--
-- This function is provided with a different name convention on
-- @Data.Either.Combinators@:
--
-- @
-- 'eitherToMaybe' = 'rightToMaybe'
-- @
--
eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = rightToMaybe

-- | The 'catEithers' function takes a list of 'Either's and returns
-- a list of all the 'Right' values.
--
-- This is just an alias for 'rights', defined in @Data.Either@
--
-- @
-- 'catEithers' = 'rights'
-- @
--
catEithers :: [Either a b] -> [b]
catEithers = rights

-- | The 'mapEither' function is a version of 'map' which can throw
-- out elements.  In particular, the functional argument returns
-- something of type @'Either' a b@.  If this is 'Left a', no element
-- is added on to the result list.  If it just @'Right' b@, then @b@ is
-- included in the result list.
mapEither :: (a -> Either b c) -> [a] -> [c]
mapEither f = catEithers . map f

-- | eitherToMonoid extract the right sided monoid into a single monoid
-- value, or mempty in the case of a left value.
--
-- > eitherToMonoid = either mempty id
eitherToMonoid :: (Monoid b) => Either a b -> b
eitherToMonoid = either mempty id

-- | monoidToEither extracts a non-empty value to the right side, or
-- otherwise fills the 'Left' side with the provided value.
monoidToEither :: (Eq b, Monoid b) => a -> b -> Either a b
monoidToEither def = monoid (Left def) Right

------------------------------------------------------------------------------
-- Lists ---------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Apply a function to a non-empty list, and retrieve its result
-- or the default provided value if the list is empty.
list :: b -> ([a] -> b) -> [a] -> b
list def f lst = if null lst then def else f lst
-- could be list = monoid, but would have to add Eq restriction


-- | Alias for 'not''.''null'
isFilled :: [a] -> Bool
isFilled = not . null

-- | Alias for 'not'.'null'. Yeah, it saves 3 characters.
notNull :: [a] -> Bool
notNull = not . null

-- | Alias for null
isNull :: [a] -> Bool
isNull = null

-- | Alias for headNote
fromHeadNote :: String -> [a] -> a
fromHeadNote = headNote

-- | Returns the first value of a list if not empty, or the
-- provided default value if the list is empty
fromList :: a -> [a] -> a
fromList = headDef

-- | Alias for catMonoid
catLists :: (Eq a) => [[a]] -> [[a]]
catLists = catMonoids

-- | Alias for concatMap
mapList :: (a -> [b]) -> [a] -> [b]
mapList = concatMap

-- | Insert a single value into a list
--
-- @
-- singleton = return
-- @
--
singleton :: a -> [a]
singleton = return

-- | map a value over a list of functions, and return a list of values
--
-- /See/: <http://www.haskell.org/pipermail/haskell-cafe/2007-February/022694.html>
--
-- @
-- /Alternative 1/: mapF value = map ($ value)
-- @
--
-- @
-- /Alternative 2/: mapF value lst = sequence lst value
-- @
--
mapF :: a -> [a -> b] -> [b]
mapF value = map ($ value)

-- | Sort and nub (remove duplicates) from a list.
-- Specially for large lists, this is much more efficient than @nub . sort@.
--
-- /Note/: You shold probably be using "Data.Set".
--
-- > nubSort = Set.toAscList . Set.fromList
--
nubSort :: (Eq a, Ord a) => [a] -> [a]
nubSort = Set.toAscList . Set.fromList

-- | Sort, nub (remove duplicates) and remove initial empty value, if it
-- exists. See 'nubSort'.
nubSort' :: (Eq a, Ord a, Monoid a) => [a] -> [a]
nubSort' [] = []
nubSort' (x:xs) = if isEmpty x then xs else x:xs


------------------------------------------------------------------------------
-- Tuple Pair ----------------------------------------------------------------
-- Monoid class restriction will be used when applicable ---------------------
------------------------------------------------------------------------------

-- | Case evaluation for a tuple pair, 'reducing' it to a single value
pair :: (Monoid c) => (a -> c) -> (b -> c) -> (a,b) -> c
pair f g (a,b) = f a `mappend` g b

-- | Case evaluation for single type tuple pairs, simplification of 'pair'.
pairS :: (Monoid b) => (a -> b) -> (a,a) -> b
pairS f (a,b) = f a `mappend` f b

-- | Very stupid function. It does not make sense.
-- You should not be using tuples for this. See 'Maybe' and/or 'Either'
--
-- > isFstTrue = fst
isFstTrue :: (Bool, a) -> Bool
isFstTrue = fst

-- | Very stupid function. It does not make sense.
-- You should not be using tuples for this. See 'Maybe' and/or 'Either'
--
-- > isFstFalse = not . fst
isFstFalse :: (Bool, a) -> Bool
isFstFalse = not . fst

-- | Very stupid function. It does not make sense.
-- You should not be using tuples for this. See 'Maybe' and/or 'Either'
--
-- > isSndTrue = fst
isSndTrue :: (a, Bool) -> Bool
isSndTrue = snd

-- | Very stupid function. It does not make sense.
-- You should not be using tuples for this. See 'Maybe' and/or 'Either'
--
-- > isSndFalse = not . fst
isSndFalse :: (a, Bool) -> Bool
isSndFalse = not . snd


-- | Longer (??) alias for 'fst'.
fromFst :: (a,b) -> a
fromFst = fst

-- | Longer (??) alias for 'snd'.
fromSnd :: (a,b) -> b
fromSnd = snd

-- | 'mappend' the two monoid elements of a pair
fromPair :: (Monoid a) => (a,a) -> a
fromPair (a,b) = a `mappend` b

-- | listToPair grabs the two first elements of a list, and inserts them
-- into a tuple. If not enough elements are available, raise the provided
-- error.
listToPairNote :: String -> [a] -> (a,a)
listToPairNote err []     = error err
listToPairNote err (_:[]) = error err
listToPairNote _ (x:y:_)  = (x,y)

-- | Groups the elements of a list two by two, also returning the (possible)
-- unpaired item not grouped.
listToPairs :: [a] -> ([(a,a)],[a])
listToPairs [] = ([],[])
listToPairs (x:[]) = ([],[x])
listToPairs (x0:x1:xs) = ( (x0,x1):(fst . listToPairs $ xs)
                          , snd . listToPairs $ xs)

-- | Similar to 'listToPairs', but discards the (possible) unpaired item.
group2 :: [a] -> [(a,a)]
group2 = fst . listToPairs

-- | Convert a single type pair into a two elements list
pairToList :: (a,a) -> [a]
pairToList (a,b) = [a,b]

-- | 'mappend' each pair in a list into a single value, and filter out
-- 'mempty' values
catPairs :: (Eq a, Monoid a) => [(a,a)] -> [a]
catPairs = mapMonoid fromPair

-- | Apply the provided function to each pair, and keep only the non-empty
-- monoids
-- /Note/: alias for 'mapMonoid'
mapPair :: (Eq b, Monoid b) => (a -> b) -> [a] -> [b]
mapPair = mapMonoid

-- | Transform a pair into an either.
-- We adopt the convention that the second value is the one of interest.
-- It is matched against @'mempty'@, and if equal the first value is returned
-- as a @'Left'@ value.
pairToEither :: (Eq b, Monoid b) => (a,b) -> Either a b
pairToEither (a,b) = if isEmpty b then Left a else Right b

-- | Transform a pair into an either.
-- The same as 'pairToEither', but the first tuple element is considered.
pairToEither' :: (Eq a, Monoid a) => (a,b) -> Either b a
pairToEither' (a,b) = if isEmpty a then Left b else Right a

-- | Transform an @'Either'@ value into a pair. This follows the same
-- convention as @'pairToEither'@, and thus transforms a @'Left' value@
-- into a @('Left' value,'mempty')@, and a @'Right' value@ into a @(def, value)@.
eitherToPair :: Monoid b => a -> Either a b -> (a, b)
eitherToPair def = either (\lft -> (,) lft mempty) (\rgt -> (,) def rgt)

-- | Transform an @'Either'@ value into a pair. This follows the same
-- convention as @'pairToEither''@, and thus transforms a @'Left' value@
-- into a @('mempty', 'Left' value)@, and a @'Right' value@ into a @(value, def)@.
eitherToPair' :: Monoid a => b -> Either b a -> (a, b)
eitherToPair' def = either (\lft -> (,) mempty lft) (\rgt -> (,) rgt def)

-- | Transform a pair into an @'Maybe'@.
-- We adopt the convention that the second value is the one of interest.
-- It is matched against @'mempty'@, and if equal the first value is returned
-- as a @'Just'@ value, or @'Nothing'@ otherwise.
--
-- > pairToMaybe = monoitToMaybe . snd
pairToMaybe :: (Eq b, Monoid b) => (a,b) -> Maybe b
pairToMaybe = monoidToMaybe . snd

-- | Transform a pair into an @'Maybe'@.
-- The same as 'pairToMaybe', but the first tuple element is considered.
--
-- > pairToMaybe' = monoitToMaybe . fst
pairToMaybe' :: (Eq a, Monoid a) => (a,b) -> Maybe a
pairToMaybe' = monoidToMaybe . fst

-- | Transform an @'Maybe'@ value into a pair. This follows the same
-- convention as @'pairToMaybe'@, and thus transforms a @'Nothing'@
-- into a @(def, 'mempty')@, and a @'Just' value@ into a @(def,value)@.
maybeToPair :: Monoid b => a -> Maybe b -> (a, b)
maybeToPair def = maybe (def, mempty) (\jst -> (,) def jst)

-- | Transform an @'Maybe'@ value into a pair. This follows the same
-- convention as @'pairToMaybe''@, and thus transforms a @'Nothing' value@
-- into a @('mempty', def), and a @'Just' value@ into a @(value,def)@.
maybeToPair' :: Monoid a => b -> Maybe a -> (a, b)
maybeToPair' def = maybe (mempty,def) (\jst -> (,) jst def)


------------------------------------------------------------------------------
-- Tuple Triple --------------------------------------------------------------
-- Monoid class restriction will be used when applicable ---------------------
------------------------------------------------------------------------------

-- | Case evaluation for a tuple triple, 'reducing' it to a single value
triple :: (Monoid d) => (a -> d) -> (b -> d) -> (c -> d) -> (a,b,c) -> d
triple f g h (a,b,c) = mconcat [f a, g b, h c]

-- | Case evaluation for single type tuple triples, simplification of 'triple'.
tripleS :: (Monoid b) => (a -> b) -> (a,a,a) -> b
tripleS f (a,b,c) = mconcat [f a, f b, f c]

-- | Very stupid function. It does not make sense.
-- You should not be using tuples for this. See 'Maybe' and/or 'Either'.
--
-- > isFst'True (a,_,_) = a
isFst'True :: (Bool,a, a) -> Bool
isFst'True (a,_,_) = a

-- | Very stupid function. It does not make sense.
-- You should not be using tuples for this. See 'Maybe' and/or 'Either'
--
-- > isFst'False = not . isFst'True
isFst'False :: (Bool, a, a) -> Bool
isFst'False = not . isFst'True

-- | Very stupid function. It does not make sense.
-- You should not be using tuples for this. See 'Maybe' and/or 'Either'
--
-- > isSnd'True (_,a,_) -> a
isSnd'True :: (a, Bool, a) -> Bool
isSnd'True (_,a,_) = a

-- | Very stupid function. It does not make sense.
-- You should not be using tuples for this. See 'Maybe' and/or 'Either'
--
-- > isSnd'False = not . isSnd'True
isSnd'False :: (a, Bool, a) -> Bool
isSnd'False = not . isSnd'True

-- | Very stupid function. It does not make sense.
-- You should not be using tuples for this. See 'Maybe' and/or 'Either'
--
-- > isTrd'True (_,_,a) -> a
isTrd'True :: (a, a, Bool) -> Bool
isTrd'True (_,_,a) = a

-- | Very stupid function. It does not make sense.
-- You should not be using tuples for this. See 'Maybe' and/or 'Either'
--
-- > isTrd'False = not . isSnd'True
isTrd'False :: (a, a, Bool) -> Bool
isTrd'False = not . isTrd'True


-- | Extract the first element from a triple
fromFst' :: (a,b,c) -> a
fromFst' (a,_,_) = a

-- | Extract the second element from a triple
fromSnd' :: (a,b,c) -> b
fromSnd' (_,a,_) = a

-- | Extract the third element from a triple
fromTrd' :: (a,b,c) -> c
fromTrd' (_,_,a) = a

-- | Alias for 'fromTrd''
trd' :: (a,b,c) -> c
trd' = fromTrd'

-- | 'mappend' the three monoid elements of a triple
fromTriple :: (Monoid a) => (a,a,a) -> a
fromTriple (a,b,c) = mconcat [a,b,c]

-- | listToTriple grabs the two three elements of a list, and inserts them
-- into a triple tuple. If not enough elements are available, raise the
-- provided error.
listToTripleNote :: String -> [a] -> (a,a,a)
listToTripleNote err []      = error err
listToTripleNote err (_:[])  = error err
listToTripleNote err (_:_:[])= error err
listToTripleNote _ (x:y:z:_) = (x,y,z)

-- | Groups the elements of a list three by three, also returning the
-- (possible) remaining item(s) (not grouped).
listToTriples :: [a] -> ([(a,a,a)],[a])
listToTriples []        = ([],[])
listToTriples (x:[])    = ([],[x])
listToTriples (x0:x1:[])= ([],[x0,x1])
listToTriples (x0:x1:x2:xs)
  = ( (x0,x1,x2):(fst . listToTriples $ xs)
    , snd . listToTriples $ xs)

-- | Similar to 'listToTriples', but discards the (possible) remaining item(s).
group3 :: [a] -> [(a,a,a)]
group3 = fst . listToTriples

-- | Convert a single type triple tuple into a three elements list
tripleToList :: (a,a,a) -> [a]
tripleToList (a,b,c) = [a,b,c]

-- | 'mappend' each triple in a list into a single value, and filter out
-- 'mempty' values
catTriples :: (Eq a, Monoid a) => [(a,a,a)] -> [a]
catTriples = mapMonoid fromTriple

-- | Apply the provided function to each triple, and keep only the non-empty
-- monoids.
-- /Note/: alias for 'mapMonoid'
mapTriple :: (Eq b, Monoid b) => (a -> b) -> [a] -> [b]
mapTriple = mapMonoid

-- | Pair to Triple, inserting the missing element in first place
--
-- > toFstPairToTriple x (y,z) = (x,y,z)
toFstPairToTriple :: a -> (b, c) -> (a, b, c)
toFstPairToTriple x (y, z) = (x, y, z)

-- | Pair to Triple, inserting the missing element in second place
--
-- > toSndPairToTriple y (x, z) = (x, y, z)
toSndPairToTriple :: b -> (a, c) -> (a, b, c)
toSndPairToTriple y (x, z) = (x, y, z)

-- | Pair to Triple, inserting the missing element in third place
--
-- > toTrdPairToTriple z (x, y) = (x, y, z)
toTrdPairToTriple :: c -> (a, b) -> (a, b, c)
toTrdPairToTriple z (x, y) = (x, y, z)

-- | Alias for toTrdPairToTriple
pairToTriple :: c -> (a, b) -> (a, b, c)
pairToTriple = toTrdPairToTriple

-- | Triple to pair, removing the first element.
--
-- > \(_,y,z) -> (y,z)
dropFstTripleToPair :: (a,b,c) -> (b,c)
dropFstTripleToPair (_,y,z) = (y,z)

-- | Triple to pair, removing the second element.
--
-- > \(x,_,z) -> (x,z)
dropSndTripleToPair :: (a,b,c) -> (a,c)
dropSndTripleToPair (x,_,z) = (x,z)

-- | Triple to pair, removing the third element.
--
-- > \(x,y,_) -> (x,y)
dropTrdTripleToPair :: (a,b,c) -> (a,b)
dropTrdTripleToPair (x,y,_) = (x,y)

-- | Alias for 'dropTrdTripleToPair'.
tripleToPair :: (a, b, c) -> (a, b)
tripleToPair = dropTrdTripleToPair


------------------------------------------------------------------------------
-- Monoid --------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Apply a function to a non-empty monoid, and retrieve its result
-- or the default provided value if the monoid is mempty.
monoid :: (Monoid a, Eq a) => b -> (a -> b) -> a -> b
monoid def f mon = if isEmpty mon then def else f mon

-- | Check it is not mempty
isNotEmpty :: (Monoid a, Eq a) => a -> Bool
isNotEmpty = (/=) mempty

-- | Check it is mempty
isEmpty :: (Monoid a, Eq a) => a -> Bool
isEmpty = (==) mempty

-- | fromNotEmptyNote keeps the monoid value if it is not empty,
-- otherwise it raises an error with the provided message.
--
-- /Note/: This differs from @fromJust@ in the sense it is not possible
-- to extract values from monoid
fromNotEmptyNote :: (Eq a, Monoid a) => String -> a -> a
fromNotEmptyNote err mon = if isEmpty mon then error err else mon

-- | fromMonoid keeps the monoid value if it is not empty,
-- otherwise it replaces it with the provided default value
--
-- /Note/: No check is made to see if default value is itself mempty
--
-- /Note/: This differs from @fromMaybe@ in the sense it is not possible
-- to extract values from monoid
--
fromMonoid :: (Eq a, Monoid a) => a -> a -> a
fromMonoid def mon = if isEmpty mon then def else mon

-- | Infix fromMonoid. Equivalent to higher order ternary operator,
-- similar to python @if@ in expressions
infixr 1 ?+
(?+) :: (Eq a, Monoid a) => a -> a -> a
mon ?+ def = fromMonoid def mon


-- | listToMonoid extracts the first element from a monoid list
-- into a single monoid, or returns mempty if the list is empty
--
-- /Note/: This differs from @listToMaybe@ in the sense it is not possible
-- to promote values into a monoid
--
-- > listToMonoid = headDef mempty
listToMonoid :: (Eq a, Monoid a) => [a] -> a
listToMonoid = headDef mempty

-- | monoidToList convert an empty monoid into an empty list,
-- otherwise it creates a singleton list with the monoid inside
--
-- /Note/: This differs from @maybeToList@ in the sense it is not possible
-- to extract the value from a monoid
--
-- > monoidToList = monoid [] singleton
monoidToList :: (Eq a, Monoid a) => a -> [a]
monoidToList = monoid [] singleton

-- | Filter out all empty monoids from list
-- > catMonoid = filter isNotEmpty
catMonoids :: (Eq a, Monoid a) => [a] -> [a]
catMonoids = filter isNotEmpty

-- | Alias for catMonoid
nonEmpty :: (Eq a, Monoid a) => [a] -> [a]
nonEmpty = catMonoids

-- | Apply a function that returns a monoid to all elements of a list
-- and return a list with only those who are not mempty.
--
-- /Note/: This differs from @mapMaybe@ in the sense it is not possible
-- to extract the value from a monoid.
mapMonoid :: (Eq b, Monoid b) => (a -> b) -> [a] -> [b]
mapMonoid _ [] = []
mapMonoid f (x:xs) =
  let rs = mapMonoid f xs in
  if isEmpty (f x) then rs else f x:rs

-- | Get the first non-empty element from a list. If all elements are 'mempty',
-- or the list is empty, it returns 'mempty'.
-- /Note/: A newtype based solution as done by maybe in "Data.Monoid" will
-- always be more efficient than this, so this is not really recommend.
-- However, it might come handy in some non-critical code.
getFirst' :: (Eq a, Monoid a) => [a] -> a
getFirst' = maybeToMonoid . L.find isNotEmpty

-- | Get the last non-empty element from a list. If all elements are 'mempty',
-- or the list is empty, it returns 'mempty'.
-- /Note/: A newtype based solution as done by maybe in "Data.Monoid" will
-- always be more efficient than this, so this is not really recommend.
-- However, it might come handy in some non-critical code.
getLast' :: (Eq a, Monoid a) => [a] -> a
getLast' = foldr (\x a -> if isNotEmpty a then a else x) mempty


------------------------------------------------------------------------------
-- Boolean -------------------------------------------------------------------
------------------------------------------------------------------------------

-- | fromBool is a 'if' rewrite following the call convention of fromMaybe.
fromBool :: a -> Bool -> a -> a
fromBool def b value = if b then value else def

-- | fromBoolC is similar to 'fromBool', but it takes a condition rather
-- than a simple boolean value
fromBoolC :: a -> (a -> Bool) -> a -> a
fromBoolC def f value = if f value then value else def

-- | Cat bools. Filter out False values from a list. Probably useless.
--
-- > catBools = filter id
catBools :: [Bool] -> [Bool]
catBools = filter id

-- | Ternary operator. Use like this:
--
-- > (i > 0) ? i $ 1
--
-- /Note/: this is non-idiomatic haskell. Use at your own risk.
infixr 1 ?
(?) :: Bool -> a -> a -> a
True ? x = const x
False ? _ = id

-- | Higher order ternary operator. Use like this:
--
-- > (not . null) ?$ "" $ "default value"
--
-- /Note/: this is non-idiomatic haskell. Use at your own risk.
infixr 1 ?$
(?$) :: (a -> Bool) -> a -> a -> a
f ?$ x = f x ? x

-- | Higher order ternary operator, similar to python @if@ in expressions.
-- Use like this:
--
-- > "" ?| (not . null) $ "default value"
--
-- /Note/: this is non-idiomatic haskell. Use at your own risk.
infixr 1 ?|
(?|) :: a -> (a -> Bool) -> a -> a
x ?| f = f x ? x

-- | Provided a default value, apply it to a maybe if the predicate holds
boolToMaybe :: a -> Bool -> Maybe a
boolToMaybe value cond = if cond then Just value else Nothing

-- | Same as boolToMaybe, but with a more familiar 'if-like' syntax
ifToMaybe :: Bool -> a -> Maybe a
ifToMaybe = flip boolToMaybe

-- | Test a value with a function returning a Bool, and apply it to a Maybe
--   accordingly.
boolCToMaybe :: a -> (a -> Bool) -> Maybe a
boolCToMaybe value f = if f value then Just value else Nothing

-- | Same as boolCToMaybe, but with a more familiar 'if-like' syntax
ifCToMaybe :: (a -> Bool) -> a -> Maybe a
ifCToMaybe = flip boolCToMaybe

-- | Apply a list of boolean checks/tests to a variable, and return (True)
-- if /all/ of them passed.
--
-- /Note/: See 'All' in "Data.Monoid" and 'all' in "Prelude" for reference.
allCond :: a -> [a -> Bool] -> Bool
allCond _ [] = False
allCond value lst = and . mapF value $ lst

-- | Apply a list of boolean checks/tests to a variable, and return (True)
-- if /any/ of them passed.
--
-- /Note/: See 'Any' in "Data.Monoid" and 'any' in "Prelude" for reference.
anyCond :: a -> [a -> Bool] -> Bool
anyCond _ [] = False
anyCond value lst = or . mapF value $ lst


