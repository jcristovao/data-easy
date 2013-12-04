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
  , module Safe

  -- * Additional functions
  -- ** Maybe
  , boolToMaybe
  , ifToMaybe
  , boolCToMaybe
  , ifCToMaybe

  -- ** Either
  , fromRightNote
  , fromLeftNote
  , fromEither
  , listToEither
  , eitherToList
  , maybeToEither
  , eitherToMaybe
  , catEithers
  , mapEither

  -- ** List
  , list
  , isFilled
  , isNull
  , singleton

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
  , catMonoid
  , nonEmpty

  -- ** Bool
  -- | Some extra functions included, namely the simplified ternary operator
  -- modified from what is seen in <https://gist.github.com/Burgestrand/218987>
  --
  -- /Note/: This is probably not considered good practice.
  -- Use the standard @if-then-else@ instead, its almost always clearer.
  -- /You have been warned./
  --
  , (?)
  , (?$)
  , (?|)
  ) where

import Data.Maybe
import Data.Either
import Data.Either.Combinators
import Data.Tuple
import Data.Monoid
import Safe

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

-- | Apply a function to a non-empty list, and retrieve its result
-- or the default provided value if the list is empty.
list :: b -> ([a] -> b) -> [a] -> b
list def f lst = if null lst then def else f lst
-- could be list = monoid, but would have to add Eq restriction


-- | Alias for 'not''.''null'
isFilled :: [a] -> Bool
isFilled = not . null

-- | Alias for null
isNull :: [a] -> Bool
isNull = null

-- | Alias for headNote
{-fromList-}

-- | Insert a single value into a list
--
-- @
-- singleton = return
-- @
--
singleton :: a -> [a]
singleton = return

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
catMonoid :: (Eq a, Monoid a) => [a] -> [a]
catMonoid = filter isNotEmpty

-- | Alias for catMonoid
nonEmpty :: (Eq a, Monoid a) => [a] -> [a]
nonEmpty = catMonoid


------------------------------------------------------------------------------
-- Boolean -------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Ternary operator. Use like this:
--
-- > (i > 0) ? i $ 1
infixr 1 ?
(?) :: Bool -> a -> a -> a
True ? x = const x
False ? _ = id

-- | Higher order ternary operator. Use like this:
--
-- > (not . null) ?$ "" $ "default value"
infixr 1 ?$
(?$) :: (a -> Bool) -> a -> a -> a
f ?$ x = f x ? x

-- | Higher order ternary operator, similar to python @if@ in expressions.
-- Use like this:
--
-- > "" ?| (not . null) $ "default value"
infixr 1 ?|
(?|) :: a -> (a -> Bool) -> a -> a
x ?| f = f x ? x
