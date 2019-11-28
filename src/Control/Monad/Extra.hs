module Control.Monad.Extra where

whenM :: Monad m => m Bool -> m () -> m ()
whenM mb a = ifM mb a (pure ())

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mb = ifM mb (pure ())

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM mb t f = mb >>= \b -> if b then t else f
{-# INLINE ifM #-}

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust (Just x) f = f x
whenJust Nothing _  = pure ()
