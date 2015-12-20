module Free where

data Free f a = Pure a | Free (f (Free f a))

instance Functor f => Monad (Free f) where
    return = Pure
    Free f >>= fn = Free $ fmap (>>= fn) f
    Pure x >>= fn = fn x

instance Functor f => Applicative (Free f) where
    pure = return
    ff <*> fx = do { f <- ff; x <- fx; return $ f x }

instance Functor f => Functor (Free f) where
    fmap f fx = do { x <- fx; return $ f x }

liftF :: Functor f => f a -> Free f a
liftF fa = Free $ fmap return fa
