{-# LANGUAGE InstanceSigs #-}

module SimpleParser where

import Data.Monoid

data Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = Parser $ \s -> (\(a, s1) -> (f a, s1)) <$> runParser p s

instance Applicative Parser where
    pure :: a -> Parser a
    pure x = Parser $ \s -> Just (x, s)

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    pf <*> px = Parser $ \s -> runParser pf s  >>= (\(f, s1) ->
                               runParser px s1 >>= (\(x, s2) ->
                                   return (f x, s2)))

instance Monoid (Parser a) where
    mempty = Parser $ const Nothing
    mappend p1 p2 = Parser $ \s -> case runParser p1 s of
                                     Just a -> Just a
                                     _      -> runParser p2 s

skipMany :: Parser a -> Parser ()
skipMany p = (p *> skipMany p) <> pure ()

noneOf :: [Char] -> Parser Char
noneOf cs = satisfy (`notElem` cs)

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of (c:cs) -> if f c then Just (c, cs)
                                                      else Nothing
                                     _      -> Nothing
chrToken :: (Char -> Maybe a) -> Parser a
chrToken matchTkn = Parser $ \s -> case s of (c:cs) -> (\tkn -> (tkn, cs))
                                                 <$> matchTkn c
                                             _      -> Nothing

eof :: Parser ()
eof = Parser $ \s -> case s of "" -> Just ((), s)
                               _  -> Nothing
