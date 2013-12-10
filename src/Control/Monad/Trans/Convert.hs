{-# LANGUAGE NoMonomorphismRestriction #-}

module Control.Monad.Trans.Convert
  ( -- ** to MaybeT
    mMaybeToMaybeT
  , mMToMT
  , ioMaybeToMaybeT
  , ioMToMT

  , mEitherToMaybeT
  , mEToMT
  , ioEitherToMaybeT
  , ioEToMT

  , mPairToMaybeT
  , mPairToMaybeT'
  , mPairFstToMaybeT
  , mPairSndToMaybeT

  , mMonoidToMaybeT
  , mOToMT

  , mBoolToMaybeT
  , mBToMT

  -- ** toEitherT
  , mMaybeToEitherT
  , mMToET
  , ioMaybeToEitherT
  , ioMToET

  , mEitherToEitherT
  , mEToET
  , ioEitherToEitherT
  , ioEToET

  , mPairToEitherT
  , mPairToEitherT'
  , mPairBothToEitherT

  , mMonoidToEitherT
  , mOToET

  , mBoolToEitherT
  , mBToET

  , tryIoMonoidToMaybeT
  , tryIoMonoidToEitherT
  , tryIoET
  , tryIoET'
  , fmapLeftT
  , fmapRightT


  ) where

import Data.UniformUtils
import Data.Monoid

import Control.Applicative

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Either

import Control.Exception
import System.IO.Error

------------------------------------------------------------------------------
-- Maybe ---------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Lift a 'Maybe' to the 'MaybeT' monad
-- Shamelessly copied from "Control.Error.Util"
hoistMaybe :: (Monad m) => Maybe b -> MaybeT m b
hoistMaybe = MaybeT . return

-- | Analogous to 'Just' and equivalent to 'return'
just :: (Monad m) => a -> MaybeT m a
just a = MaybeT (return (Just a))

-- | Analogous to 'Nothing' and equivalent to 'mzero'
nothing :: (Monad m) => MaybeT m a
nothing = MaybeT (return Nothing)

-- | Transform a maybe value encapsulated in a @Monad m@ into the equivalent
-- MaybeT m monad transformer.
--
-- /NOTE/: this is not equivalent to either @lift@ or @hoistMaybe@ alone.
-- Check the types carefully.
mMaybeToMaybeT :: (Monad m) => m (Maybe a) -> MaybeT m a
mMaybeToMaybeT mF = lift mF >>= hoistMaybe

-- | Shorter alias for mMaybeToMaybeT
mMToMT :: (Monad m) => m (Maybe a) -> MaybeT m a
mMToMT = mMaybeToMaybeT

-- | Transform a IO (Maybe a) value into a MaybeT IO a value
ioMaybeToMaybeT :: IO (Maybe a) -> MaybeT IO a
ioMaybeToMaybeT = mMaybeToMaybeT

-- | Shorter alias for ioMaybeToMaybeT
ioMToMT :: IO (Maybe a) -> MaybeT IO a
ioMToMT = ioMaybeToMaybeT

-- | Transform a either value encapsulated in a @Monad m@ into the equivalent
-- MaybeT m monad transformer.
--
-- /Note/: The left value is silently discarded.
mEitherToMaybeT :: (Functor m, Monad m) => m (Either a b) -> MaybeT m b
mEitherToMaybeT eF = eitherToMaybe <$> lift eF >>= hoistMaybe

-- | Shorter alias for 'mEitherToMaybeT'.
mEToMT :: (Functor m, Monad m) => m (Either a b) -> MaybeT m b
mEToMT = mEitherToMaybeT

-- | Transform a either value encapsulated in a IO monad into the equivalent
-- MaybeT IO monad transformer.
--
-- /Note/: The left value is silently discarded.
ioEitherToMaybeT :: IO (Either a b) -> MaybeT IO b
ioEitherToMaybeT = mEitherToMaybeT

-- | Shorter alias for 'mEitherToMaybeT'.
ioEToMT :: IO (Either a b) -> MaybeT IO b
ioEToMT = mEitherToMaybeT

-- | Case analysis of a pair of monoid values returned by a monad into
-- a MaybeT value.
-- The value conversion is done by @'pairToMaybe'@.
mPairToMaybeT :: (Eq a, Monoid a, Functor m, Monad m) => m (a,a) -> MaybeT m a
mPairToMaybeT pF = pairToMaybe <$> lift pF >>= hoistMaybe

-- | Case analysis of a pair of monoid values returned by a monad into
-- a MaybeT value.
-- The value conversion is done by @'pairToMaybe''@.
mPairToMaybeT' :: (Eq a, Monoid a, Functor m, Monad m) => m (a,a) -> MaybeT m a
mPairToMaybeT' pF = pairToMaybe' <$> lift pF >>= hoistMaybe

-- | Case analysis of a pair of monoid values returned by a monad into
-- a MaybeT value.
-- The value conversion is done by @'pairFstToMaybe'@.
mPairFstToMaybeT :: (Eq a, Monoid a, Functor m, Monad m) => m (a,b) -> MaybeT m a
mPairFstToMaybeT pF = pairFstToMaybe <$> lift pF >>= hoistMaybe

-- | Case analysis of a pair of monoid values returned by a monad into
-- a MaybeT value.
-- The value conversion is done by @'pairSndToMaybe'@.
mPairSndToMaybeT :: (Eq b, Monoid b, Functor m, Monad m) => m (a,b) -> MaybeT m b
mPairSndToMaybeT pF = pairSndToMaybe <$> lift pF >>= hoistMaybe

-- | Transform a monoid value encapsulated in a @Monad m@ into the equivalent
-- MaybeT m monad transformer (@'mempty'@ -> @'Nothing'@).
mMonoidToMaybeT :: (Eq o, Monoid o, Functor m, Monad m) => m o -> MaybeT m o
mMonoidToMaybeT oF = monoidToMaybe <$> lift oF >>= hoistMaybe

-- | Shorter alias for 'mMonoidToMaybeT'
mOToMT :: (Eq o, Monoid o, Functor m, Monad m) => m o -> MaybeT m o
mOToMT = mMonoidToMaybeT

-- | Transform a boolean value encapsulated in a @Monad m@ into the equivalent
-- MaybeT m monad transformer (@'True'@ -> @Provided Default Value@).
mBoolToMaybeT :: (Functor m, Monad m) => a -> m Bool -> MaybeT m a
mBoolToMaybeT def bF = boolToMaybe def <$> lift bF >>= hoistMaybe

-- | Shorter alias for @'mBoolToMaybeT'@.
mBToMT :: (Functor m, Monad m) => a -> m Bool -> MaybeT m a
mBToMT = mBoolToMaybeT

------------------------------------------------------------------------------
-- Either --------------------------------------------------------------------
------------------------------------------------------------------------------
{-ioMaybeToEitherT :: b -> IO (Maybe a) -> EitherT b IO a-}
{-ioMaybeToEitherT err ioF = maybeToEither err <$> liftIO ioF >>= hoistEither-}

-- | Transform a maybe value encapsulated in a @Monad m@ into the equivalent
-- EitherT b m monad transformer.
--
-- /NOTE/: this is not equivalent to either @lift@ or @hoistEither@ alone.
-- Check the types carefully.
mMaybeToEitherT :: (Monad m) => b -> m (Maybe a) -> EitherT b m a
mMaybeToEitherT err mF = maybeToEither err <$> lift mF >>= hoistEither

-- | Shorter alias for mMaybeToEitherT
mMToET :: (Monad m) => b -> m (Maybe a) -> EitherT b m a
mMToET = mMaybeToEitherT

-- | Transform a IO (Maybe a) value into a EitherT b IO a value
ioMaybeToEitherT :: b -> IO (Maybe a) -> EitherT b IO a
ioMaybeToEitherT = mMaybeToEitherT

-- | Shorter alias for ioMaybeToEitherT
ioMToET :: b -> IO (Maybe a) -> EitherT b IO a
ioMToET = ioMaybeToEitherT

-- | Transform a either value encapsulated in a @Monad m@ into the equivalent
-- EitherT e m monad transformer.
mEitherToEitherT :: Monad m => m (Either b a) -> EitherT b m a
mEitherToEitherT eF = lift eF >>= hoistEither

-- | Shorter alias for 'mEitherToEitherT'.
mEToET :: Monad m => m (Either b a) -> EitherT b m a
mEToET = mEitherToEitherT

-- | Transform a either value encapsulated in a IO monad into the equivalent
-- EitherT b IO monad transformer.
ioEitherToEitherT :: IO (Either b a) -> EitherT b IO a
ioEitherToEitherT = mEitherToEitherT

-- | Shorter alias for 'mEitherToEitherT'.
ioEToET :: IO (Either b a) -> EitherT b IO a
ioEToET = mEitherToEitherT

-- | Case analysis of a pair of monoid values returned by a monad into
-- a EitherT value.
-- The value conversion is done by @'pairToEither'@.
mPairToEitherT
  :: (Eq a, Monoid a, Functor m, Monad m)
  => m (b,a)
  -> EitherT b m a
mPairToEitherT pF = pairToEither <$> lift pF >>= hoistEither

-- | Case analysis of a pair of monoid values returned by a monad into
-- a EitherT value.
-- The value conversion is done by @'pairToEither''@.
mPairToEitherT'
  :: (Eq b, Monoid b, Functor m, Monad m)
  => m (b,a)
  -> EitherT a m b
mPairToEitherT' pF = pairToEither' <$> lift pF >>= hoistEither

-- | Case analysis of a pair of monoid values returned by a monad into
-- a EitherT value.
-- The value conversion is done by @'pairBothToEither'@.
mPairBothToEitherT
  :: (Eq a, Monoid a, Functor m, Monad m)
  => b
  -> m (a,a)
  -> EitherT b m a
mPairBothToEitherT def pF = pairBothToEither def <$> lift pF >>= hoistEither

-- | Transform a monoid value encapsulated in a @Monad m@ into the equivalent
-- EitherT e m monad transformer (@'mempty'@ -> @'Nothing'@).
mMonoidToEitherT :: (Eq a, Monoid a, Functor m, Monad m)
  => b
  -> m a
  -> EitherT b m a
mMonoidToEitherT def oF = monoidToEither def <$> lift oF >>= hoistEither

-- | Shorter alias for 'mMonoidToEitherT'
mOToET :: (Eq a, Monoid a, Functor m, Monad m)
  => b
  -> m a
  -> EitherT b m a
mOToET = mMonoidToEitherT

-- | Transform a boolean value encapsulated in a @Monad m@ into the equivalent
-- Either m monad transformer. Uses @'boolToEither'@.
mBoolToEitherT
  :: (Functor m, Monad m)
  => b
  -> a
  -> m Bool
  -> EitherT b m a
mBoolToEitherT l r bF = boolToEither l r <$> lift bF >>= hoistEither

-- | Shorter alias for @'mBoolToEitherT'@.
mBToET
  :: (Functor m, Monad m)
  => b
  -> a
  -> m Bool
  -> EitherT b m a
mBToET = mBoolToEitherT

{-eIoTry :: IO a -> IO (Either IOException a)-}
{-eIoTry = try-}

-- /Note/: Since try is restricted to IO, this function is also restricted to
-- monad tranformers where the last monad is IO.
tryIoMonoidToMaybeT
  :: (Eq a, Monoid a)
  => IO a
  -> MaybeT IO a
tryIoMonoidToMaybeT ioF =   join . fmap monoidToMaybe . eitherToMaybe
                        <$> (lift . tryIOError) ioF
                        >>= hoistMaybe
  {-res <- lift $ tryIOError ioF-}
  {-case res of-}
    {-Left _  -> nothing-}
    {-Right a -> hoistMaybe $ monoidToMaybe a-}

-- This function executes a IO action that returns a monoid value, or raises
-- a @'IOException'@. If a @'IOException'@ is raised, or the return value
-- is an empty monoid, the function returns a left value in the
-- @EitherT IOException IO@ monad transformer. In the later case (empty monoid),
-- the exception will be of @'userErrorType'@ type, with the default string
-- \"empty monoid\". If you wish to specify your own message, see @'tryIoET''@.
--
-- /Note/: if a different type of exception is raised (@/= IOEXCEPTION@),
-- then that exception is re-thrown.
tryIoMonoidToEitherT
  :: (Eq a, Monoid a)
  => IO a
  -> EitherT IOException IO a
tryIoMonoidToEitherT ioF
  =   joinEitherMonoid (userError "empty monoid") <$> (lift . tryIOError) ioF
  >>= hoistEither

-- | This function executes a IO action that returns a monoid value, or raises
-- an @'IOException'@. If a @'IOException'@ is raised, or the returned value
-- is an empty monoid, the function returns a left value in the
-- @EitherT IOException IO@ monad transformer. In the later case (empty monoid),
-- the exception will be of @'userErrorType'@ type, with the provided string
-- text.
tryIoET'
  :: (Eq a, Monoid a)
  => String
  -> IO a
  -> EitherT IOException IO a
tryIoET' err ioF
  =   joinEitherMonoid (userError err) <$> (lift . tryIOError) ioF
  >>= hoistEither

-- | This function executes a IO action that could raise an @'IOException'@.
-- If a @'IOException'@ is raised, the function returns a left value in the
-- @EitherT IOException IO@ monad transformer.
tryIoET :: IO a -> EitherT IOException IO a
tryIoET ioF = (lift . tryIOError) ioF >>= hoistEither

-- | Change the left type using the provided conversion function.
-- This is a specialization of @'Control.Monad.Trans.Either.bimapEitherT'@.
--
-- > fmapLeftT f = bimapEitherT f id
--
-- or using the "errors" "Data.EitherR"
--
-- > fmapLeftT = fmapLT
fmapLeftT
  :: Functor m
  => (a -> c)
  -> EitherT a m b
  -> EitherT c m b
fmapLeftT f = bimapEitherT f id

-- | Change the right type using the provided conversion function.
-- This is a specialization of @'Control.Monad.Trans.Either.bimapEitherT'@.
--
fmapRightT
  :: Functor m
  => (b -> c)
  -> EitherT a m b
  -> EitherT a m c
fmapRightT = bimapEitherT id
