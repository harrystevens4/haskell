module Main where

import Control.Monad
import Prelude hiding (Maybe (..))

data Maybe a = Just a | Nothing deriving (Show)

instance Monad Maybe where
    (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
    Just a >>= f = f a
    return :: a -> Maybe a
    return = pure

instance Applicative Maybe where
    pure x = Just x
    (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
    Just f <*> a = f <$> a
    Nothing <*> a = Nothing

instance Functor Maybe where
    fmap f (Just a) = Just $ f a
    fmap f (Nothing) = Nothing

data StateMonad s a = StateMonad (s -> (s, a))

stateMonadFrom :: s -> a -> StateMonad s a
stateMonadFrom s a = StateMonad (\x -> (s, a))

instance Monad (StateMonad s) where
    (>>=) :: StateMonad s a -> (a -> StateMonad s b) -> StateMonad s b
    StateMonad oldTransition >>= newMonadFactory =
        StateMonad newTransition
      where
        newState x = fst . oldTransition
        value x = snd . oldTransition
        newMonad x = newMonadFactory (value x x)
        newTransition x = getStateTransition (newMonad x) x

    -- ( \x ->
    --     -- mapFst (fst . (getStateTransition (newMonadFactory (snd . oldTransition $ x)))) $ oldTransition x
    --     (getStateTransition (newMonadFactory (snd . oldTransition $ x))) x
    -- )

    return :: a -> StateMonad s a
    return = pure

-- get old transition
-- pass new parameter to old transition
-- that gives us the new state
-- ignore snd
-- use the new result to pass to our given monad factory

mapFst :: (a -> a') -> (a, b) -> (a', b)
mapFst f (a, b) = (f a, b)

getStateTransition :: StateMonad s a -> (s -> (s, a))
getStateTransition (StateMonad f) = f

mapState :: (s -> s) -> (s -> (s, a)) -> (s -> (s, a))
mapState f sf = \x -> mapFst f (sf x)

-- given
-- (\s -> (s', x)) and (\s' -> (s'', x))
-- produce
-- (\s -> (s'', x))
--
-- >>> :t fst . (\s -> (s,1))
-- fst . (\s -> (s,1)) :: c -> c

instance Applicative (StateMonad s) where
    pure :: a -> StateMonad s a
    pure a = StateMonad (\x -> (x, a))

-- (<*>) :: StateMonad s (a -> b) -> StateMonad s a -> StateMonad s b
-- StateMonad f <*> StateMonad a =

instance Functor (StateMonad s)

-- fmap :: (a -> b) -> StateMonad s a -> StateMonad s b
-- fmap f (StateMonad x) = StateMonad $ f x

get :: StateMonad a a
get = StateMonad (\x -> (x, x))

set :: s -> StateMonad s ()
set s = StateMonad (\x -> (s, ()))

evalState :: s -> (StateMonad s () -> StateMonad s a) -> s
evalState s f = fst $ (getStateTransition $ f (StateMonad (\s -> (s, ())))) s

main =
    print $
        evalState
            [1, 2, 3]
            ( \x -> do
                x
                current <- get
                set (0 : current)
                current' <- get
                set ((-1) : current')
            )

-- >>> main
