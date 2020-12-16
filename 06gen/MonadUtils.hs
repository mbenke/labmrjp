module MonadUtils 
    ( module MonadUtils
    , (<$>), (<*>)
    )
    where
-- mostly borrowed from Agda
import Data.Monoid
import Data.Maybe

import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import qualified Control.Monad.State.Strict as SS
import Control.Monad.Writer
import Control.Applicative
import Data.Traversable
import Data.Foldable

fromMaybeM :: Monad m => m a -> m (Maybe a) -> m a
fromMaybeM m mm = maybe m return =<< mm

infixl 8 <.>

(<.>) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
f <.> g = \x -> f =<< g x

whenM :: Monad m => m Bool -> m () -> m ()
whenM c m = do	b <- c
		when b m

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM c m = do    b <- c
		    unless b m

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM c m m' =
    do	b <- c
	if b then m else m'

forgetM :: Applicative m => m a -> m ()
forgetM m = const () <$> m

-- | Depending on the monad you have to look at the result for
--   the force to be effective. For the 'IO' monad you do.
forceM :: Monad m => [a] -> m ()
forceM xs = do () <- length xs `seq` return ()
	       return ()
