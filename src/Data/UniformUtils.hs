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
  , joinEitherMonoid

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
  -- | Monoid class restriction will be used in tuple elements whenever
  -- necessary to create the concept of \'valid\' value.
  --
  -- /Note/: if you need real heterogeneous lists, see the "HList" package.
  -- <http://hackage.haskell.org/package/HList>
   , pair
  , pairS
  , isPairNotEmpty
  , isPairEmpty
  , fromFst
  , fromSnd
  , fromPairNote
  , fromPair
  , listToPairNote
  , listToPairs
  , group2
  , pairToList
  , catPairs
  , mapPair
  , pairToEither
  , pairToEither'
  , pairBothToEither
  , eitherToPair
  , eitherToPair'
  , pairToMaybe
  , pairToMaybe'
  , pairFstToMaybe
  , pairSndToMaybe
  , maybeToPair
  , maybeToPair'
  , pairToMonoid
  , pairToMonoid'

  -- ** Tuple Triples
  -- | Monoid class restriction will be used in tuple elements whenever
  -- necessary to create the concept of \'valid\' value.
  --
  -- /Note/: if you need real heterogeneous lists, see the "HList" package.
  -- <http://hackage.haskell.org/package/HList>
  --
  -- /Note/: we use the postfix ' to distinguish from tuple pairs.
  -- This clearly doesn't scale to bigger tuples.
  -- If you need those, you probably should be using a better
  -- library than this, no? See <http://hackage.haskell.org/package/lens>.
  , triple
  , tripleS
  , isTripleNotEmpty
  , isTripleEmpty
  , fromFst'
  , fst'
  , fromSnd'
  , snd'
  , fromTrd'
  , trd'
  , fromTripleNote
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
  , tripleToMaybe
  , tripleToMaybe'
  , tripleToMonoid
  , tripleToMonoid'

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
  , headF
  , lastF
  , atF

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
  , boolToEither
  , boolCToEither
  , boolToList
  , boolCToList
  , boolToMonoid
  , (?&&)
  , (?$&&)
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
import qualified Data.Foldable as F

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

-- | Case analysis for a either monoid. If the right side of the monoid
-- is @'mempty'@, then the value is transformed to a left value, using
-- the provided function.
joinEitherMonoid :: (Eq b, Monoid b) => a -> Either a b -> Either a b
joinEitherMonoid emptErr = either Left (monoidToEither emptErr)

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

-- | Is the pair tuple \'valid\', i.e., does it have at least one
-- non-empty (monoid) value?
isPairNotEmpty :: (Eq a, Monoid a, Eq b, Monoid b) => (a, b) -> Bool
isPairNotEmpty (x,y) = isNotEmpty x || isNotEmpty y
--
-- | Is the pair tuple \'invalid\', i.e., are both (monoid) elements
-- 'mempty'?
isPairEmpty :: (Eq a, Monoid a, Eq b, Monoid b) => (a, b) -> Bool
isPairEmpty (x,y) = isEmpty x && isEmpty y

-- | Longer (??) alias for 'fst'.
fromFst :: (a,b) -> a
fromFst = fst

-- | Longer (??) alias for 'snd'.
fromSnd :: (a,b) -> b
fromSnd = snd

-- | 'mappend' the two monoid elements of a pair
fromPairNote :: (Eq a, Monoid a) => String -> (a,a) -> a
fromPairNote err (a,b) = fromMonoid (error err) (a `mappend` b)

-- | 'mappend' the two monoid elements of a pair
fromPair :: (Eq a, Monoid a) => a -> (a,a) -> a
fromPair def (a,b) = fromMonoid def (a `mappend` b)

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
catPairs = mapMonoid (fromPair mempty)

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

-- | Transform a pair into an either.
-- Both values are checked for a valid monoid (non-empty).
-- The first to be found is returned as a Right value.
-- If none is found, a default value is returned.
pairBothToEither :: (Eq a, Monoid a) => b -> (a,a) -> Either b a
pairBothToEither def (a,b) = monoid (monoid (Left def) Right b) Right a


-- | Transform an @'Either'@ value into a pair. This follows the same
-- convention as @'pairToEither'@, and thus transforms a @'Left' value@
-- into a @('Left' value,'mempty')@, and a @'Right' value@ into a @(def, value)@.
eitherToPair :: Monoid b => a -> Either a b -> (a, b)
eitherToPair def = either (\lft -> (,) lft mempty) ((,) def)

-- | Transform an @'Either'@ value into a pair. This follows the same
-- convention as @'pairToEither''@, and thus transforms a @'Left' value@
-- into a @('mempty', 'Left' value)@, and a @'Right' value@ into a @(value, def)@.
eitherToPair' :: Monoid a => b -> Either b a -> (a, b)
eitherToPair' def = either ((,) mempty) (\rgt -> (,) rgt def)

-- | Transform a pair onto a @'Maybe'@
-- If both the values are non-empty, the first one is returned wrapped in
-- a Just. If just one value is not-empty, that value is returned,
-- irrespectively if it is the first or second.
-- Otherwise, this function returns Nothing.
--
-- /Note/: the reciprocal of this function is @'pairToMaybe''@.
--
-- > pairToMaybe = monoid (monoid Nothing Just b) Just a
pairToMaybe :: (Eq a, Monoid a) => (a,a) -> Maybe a
pairToMaybe (a,b) = monoid (monoid Nothing Just b) Just a

-- | Transform a pair onto a @'Maybe'@
-- If both the values are non-empty, the second one is returned wrapped in
-- a Just. If just one value is not-empty, that value is returned,
-- irrespectively if it is the first or second.
-- Otherwise, this function returns Nothing.
--
-- /Note/: the reciprocal of this function is @'pairToMaybe'@.
--
-- > pairToMaybe = monoid (monoid Nothing Just b) Just a
pairToMaybe' :: (Eq a, Monoid a) => (a,a) -> Maybe a
pairToMaybe' (a,b) = monoid (monoid Nothing Just a) Just b


-- | Transform the first element of a pair (if it is a monoid) into an @'Maybe'@.
-- Reciprocal to @'pairSndToMaybe'@.
--
-- > pairToMaybe' = monoitToMaybe . fst
pairFstToMaybe :: (Eq a, Monoid a) => (a,b) -> Maybe a
pairFstToMaybe = monoidToMaybe . fst


-- | Transform the second element of a pair (if it is a monoid) into a @'Maybe'@.
-- Reciprocal to @'pairFstToMaybe'@.
--
-- > pairToMaybe = monoitToMaybe . snd
pairSndToMaybe :: (Eq b, Monoid b) => (a,b) -> Maybe b
pairSndToMaybe = monoidToMaybe . snd


-- | Transform a @'Maybe'@ value into a pair. This follows the same
-- convention as @'pairToMaybe'@, and thus transforms a @'Nothing'@
-- into a @('mempty', def)@, and a @'Just' value@ into a @(value,def)@.
--
maybeToPair :: Monoid a => b -> Maybe a -> (a, b)
maybeToPair def = maybe (mempty,def) (\jst -> (,) jst def)

-- | Transform a @'Maybe'@ value into a pair. This follows the same
-- convention as @'pairToMaybe''@, and thus transforms a @'Nothing'@
-- into a @(def, 'mempty')@, and a @'Just' value@ into a @(def, value)@.
--
maybeToPair' :: Monoid b => a -> Maybe b -> (a, b)
maybeToPair' def = maybe (def,mempty) ((,) def)

-- | Finds the first non-empty monoid in a pair, and returns it.
-- If none found, returns @'mempty'@.
--
-- /Note/: reciprocal to @'pairToMonoid''@
pairToMonoid :: (Eq a, Monoid a) => (a,a) -> a
pairToMonoid (a,b) = fromMonoid (fromMonoid mempty b) a

-- | Finds the last non-empty monoid in a pair, and returns it.
-- If none found, returns @'mempty'@.
pairToMonoid' :: (Eq a, Monoid a) => (a,a) -> a
pairToMonoid' (a,b) = fromMonoid (fromMonoid mempty a) b

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

-- | Is the triple tuple \'valid\', i.e., does it have at least one
-- non-empty (monoid) value?
isTripleNotEmpty
  :: (Eq a, Monoid a, Eq b, Monoid b, Eq c, Monoid c)
  => (a, b, c)
  -> Bool
isTripleNotEmpty (x,y,z)
  = isNotEmpty x || isNotEmpty y || isNotEmpty z
--
-- | Is the pair tuple \'invalid\', i.e., are both (monoid) elements
-- 'mempty'?
isTripleEmpty
  :: (Eq a, Monoid a, Eq b, Monoid b, Eq c, Monoid c)
  => (a, b, c)
  -> Bool
isTripleEmpty (x,y,z)
  = isEmpty x && isEmpty y && isEmpty z

-- | Extract the first element from a triple
fromFst' :: (a,b,c) -> a
fromFst' (a,_,_) = a

-- | Alias for fromFst' (extract the first element of a triple).
fst' :: (a, b, c) -> a
fst' = fromFst'

-- | Extract the second element from a triple
fromSnd' :: (a,b,c) -> b
fromSnd' (_,a,_) = a

-- | Alias for fromSnd' (extract the second element of a triple).
snd' :: (a, b, c) -> b
snd' = fromSnd'

-- | Extract the third element from a triple
fromTrd' :: (a,b,c) -> c
fromTrd' (_,_,a) = a

-- | Alias for 'fromTrd''
trd' :: (a,b,c) -> c
trd' = fromTrd'

-- | 'mappend' the two monoid elements of a pair
fromTripleNote :: (Eq a, Monoid a) => String -> (a,a,a) -> a
fromTripleNote err (a,b,c) = fromMonoid (error err)
                                        (a `mappend` b `mappend` c)

-- | 'mappend' the three monoid elements of a triple
fromTriple :: (Eq a, Monoid a) => a -> (a,a,a) -> a
fromTriple def (a,b,c) = fromMonoid def (mconcat [a,b,c])

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
catTriples = mapMonoid (fromTriple mempty)
-- TODO

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

-- | Triple to Maybe. Analogous to @'pairToMaybe'@, it keeps the first
-- non-empty monoid value.
tripleToMaybe :: (Eq a, Monoid a) => (a,a,a) -> Maybe a
tripleToMaybe (a,b,c)
  = monoid (monoid (monoid Nothing Just c) Just b) Just a

-- | Triple to Maybe. Analogous to @'pairToMaybe''@, it keeps the last
-- non-empty monoid value.
tripleToMaybe' :: (Eq a, Monoid a) => (a,a,a) -> Maybe a
tripleToMaybe' (a,b,c)
  = monoid (monoid (monoid Nothing Just a) Just b) Just c

-- | Triple to Monoid. Analogous to @'pairToMonoid'@, it keeps the first
-- non-empty monoid value.
tripleToMonoid :: (Eq a, Monoid a) => (a,a,a) -> a
tripleToMonoid (a,b,c)
  = fromMonoid (fromMonoid (fromMonoid mempty c) b) a

-- | Triple to Maybe. Analogous to @'pairToMonoid''@, it keeps the last
-- non-empty monoid value.
tripleToMonoid' :: (Eq a, Monoid a) => (a,a,a) -> a
tripleToMonoid' (a,b,c)
  = fromMonoid (fromMonoid (fromMonoid mempty a) b) c


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

-- | Filter out all empty monoids from a list.
--
-- > catMonoids = filter isNotEmpty
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

-- | A @'head'@ that fails returning @'mempty'@.
-- Gets the first element of a foldable stucture of monoids.
--
-- Returns @'mempty'@ if the structure is empty.
headF :: (F.Foldable t, Monoid a) => t a -> a
headF = headDef mempty . F.toList

-- | A @'last'@ that fails returning @'mempty'@.
-- Gets the last element of a foldable stucture of monoids.
-- Returns @'mempty'@ if the structure is empty.
--
-- /Note/: this function starts by mapping the foldable structure to a list...
lastF :: (F.Foldable t, Monoid a) => t a -> a
lastF = lastDef mempty . F.toList

-- | A @'(!!)'@ that fails returning @'mempty'@.
--
-- /Note/: this function starts by mapping the foldable structure to a list...
atF :: (F.Foldable t, Monoid a) => t a -> Int -> a
atF lst index = flip (atDef mempty) index . F.toList $ lst
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

-- | Provided two values, choose amongst them based on a 'Bool' value.
--
-- > \l r b = if b then Left l else Right r
boolToEither :: a -> b -> Bool -> Either a b
boolToEither l r b = if b then Left l else Right r

-- | Provided two values, choose amongst them based on a the provided
-- test on the second value.
--
-- > \l r f = if f r then Left l else Right r
boolCToEither :: a -> b -> (b -> Bool) -> Either a b
boolCToEither l r f = if f r then Left l else Right r

-- | Insert the provided value into a list if the 'Bool' value is 'True',
-- otherwise return an empty list.
boolToList :: a -> Bool -> [a]
boolToList value b = fromBool [] b [value]

-- | Insert the provided value into a list if the provided condition is
-- 'True', otherwise return an empty list.
boolCToList :: a -> (a -> Bool) -> [a]
boolCToList value f = if f value then [value] else []


-- | Keep the provided value if the 'Bool' value is 'True', 'mempty'
-- otherwise.
boolToMonoid :: (Monoid a) => a -> Bool -> a
boolToMonoid value b = if b then value else mempty

-- | Emulates @and@,@&&@ and @or@,@||@ from scripting languages like python,
-- in the sense you can mix booleans with a value to get the value when
-- the boolean is true (or 'mempty' otherwise).
--
-- However, in order to allow several @'?&&'@ in a row, the order
-- is not the one normally used in languages like bash, where the test comes
-- first.
--
-- /Usage/:
--
-- > value ?&& bool1 ?&& bool2 ?&& ...
--
-- /Note/: this is non-idiomatic haskell. Use at your own risk.
-- You should instead use the following code :
--
-- > if bool1 && bool2 && ...  then value else mempty
--
-- Or better yet:
--
-- > if and [bool1,bool2,...] then value else mempty
infixl 1 ?&&
(?&&) :: (Monoid a) => a -> Bool -> a
(?&&) = boolToMonoid

-- | Emulates @and@,@&&@ and @or@,@||@ from scripting languages like python,
-- in the sense you can mix boolean tests with a value to get the original
-- value when all the tests return true (or 'mempty' otherwise).
--
-- However, in order to allow several @'??&&'@ in a row, the order
-- is not the one normally used in languages like bash, where the test comes
-- first.
--
-- /Usage/:
--
-- > value ?$&& condition1 ?$&& condition2 ?$&& ...
--
-- /Note/: this is non-idiomatic haskell. Use at your own risk.
infixl 1 ?$&&
(?$&&) :: (Monoid a) => a -> (a -> Bool) -> a
(?$&&) value f = if f value then value else mempty

-- ?||
-- Just use: ?&& value (bool1 || bool2 || ... )
--       or: ?&& value $ or [bool1,bool2,...]

-- ?$||
-- Just use: anyCond value [cond1, cond2,...]

-- | Apply a list of boolean checks/tests to a variable, and return (True)
-- if /all/ of them passed.
--
-- /Note/: See 'All' in "Data.Monoid" and 'all' in "Prelude" for reference.
--
-- /See/: <http://www.haskell.org/pipermail/haskell-cafe/2007-February/022694.html>
allCond :: a -> [a -> Bool] -> Bool
allCond _ [] = False
allCond value lst = and . mapF value $ lst

-- | Apply a list of boolean checks/tests to a variable, and return (True)
-- if /any/ of them passed.
--
-- /Note/: See 'Any' in "Data.Monoid" and 'any' in "Prelude" for reference.
--
-- /See/: <http://www.haskell.org/pipermail/haskell-cafe/2007-February/022694.html>
anyCond :: a -> [a -> Bool] -> Bool
anyCond _ [] = False
anyCond value lst = or . mapF value $ lst


