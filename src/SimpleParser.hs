{-# LANGUAGE InstanceSigs #-}

module SimpleParser where

data Parser a = P { runP :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (P p) = P $ \s -> (\(a, s1) -> (f a, s1)) <$> p s

instance Applicative Parser where
    pure :: a -> Parser a
    pure x = P $ \s -> Just (x, s)

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    P pf <*> P px = P $ \s -> pf s >>=
                        (\(f, s1) -> px s1 >>= (\(x, s2) -> Just (f x, s2)))

instance Monoid (Parser a) where
    mempty = P $ const Nothing
    mappend (P p1) (P p2) = P $ \s -> case p1 s of
                                        Just a -> Just a
                                        _      -> p2 s

