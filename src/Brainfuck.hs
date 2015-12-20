{-# LANGUAGE DeriveFunctor #-}

module Brainfuck where

import qualified Data.Vector.Unboxed as V
import Data.Char
import Data.List
import System.IO

-- This code works with either my own parser and free implementations or the
-- premade ones.

--import Text.Parsec
--import Text.Parsec.String
--import Control.Monad.Free

import SimpleParser
import Free

type Program = Free Brainfuck ()

data Brainfuck next = IncPtr next
                    | DecPtr next
                    | IncByte next
                    | DecByte next
                    | ByteOut next
                    | ByteIn next
                    | Loop Program next
                    deriving Functor

incPtr :: Program
incPtr = liftF (IncPtr ())
decPtr :: Program
decPtr = liftF (DecPtr ())
incByte :: Program
incByte = liftF (IncByte ())
decByte :: Program
decByte = liftF (DecByte ())
byteOut :: Program
byteOut = liftF (ByteOut ())
byteIn :: Program
byteIn = liftF (ByteIn ())
loop :: Program -> Program
loop body = liftF (Loop body ())

-- A simple example of a Brainfuck program:
-- ask the user for two characters, increment them by one and print them out
-- in reverse order
simpleExample :: Program
simpleExample = do
    byteIn
    incByte
    incPtr
    byteIn
    incByte
    byteOut
    decPtr
    byteOut

-- The same program as a string
simpleExampleStr :: String
simpleExampleStr = ",+>,+.<."

brainfuck :: Parser Program
brainfuck = skip *> parseBf
    where parseBf = (>>) <$> ((cmd <|> lp) <* skip)
                         <*> (parseBf <|> pure (Pure ()))
          lp = loop <$> between (char '[') (char ']') brainfuck
          cmd = foldl1' (<|>) $ map (\(c, f) -> f <$ char c) [
              ('>', incPtr),  ('<', decPtr),
              ('+', incByte), ('-', decByte),
              ('.', byteOut), (',', byteIn) ]
          skip = skipMany (noneOf "><+-.,[]")

runBrainfuck :: Program -> (Int, Int, V.Vector Int) -> IO ()
runBrainfuck (Free (ByteOut next)) st@(_, ptr, mem) =
    putChar (chr $ mem V.! ptr) >> hFlush stdout >> runBrainfuck next st
runBrainfuck (Free (ByteIn next)) (n, ptr, mem) =
    getChar >>= \c -> runBrainfuck next (n, ptr, mem V.// [(ptr, ord c)])
runBrainfuck (Free (IncPtr next)) (n, ptr, mem) = runBrainfuck next
    (n, min (n-1) (ptr+1), mem)
runBrainfuck (Free (DecPtr next)) (n, ptr, mem) = runBrainfuck next
    (n, max 0 (ptr-1), mem)
runBrainfuck (Free (IncByte next)) (n, ptr, mem) = runBrainfuck next
    (n, ptr, mem V.// [(ptr, (mem V.! ptr) + 1)])
runBrainfuck (Free (DecByte next)) (n, ptr, mem) = runBrainfuck next
    (n, ptr, mem V.// [(ptr, (mem V.! ptr) - 1)])
runBrainfuck lp@(Free (Loop body next)) st@(_, ptr, mem) =
    if mem V.! ptr == 0
        then runBrainfuck next st
        else runBrainfuck (body >> lp) st
runBrainfuck (Pure _) _ = return ()

execBrainfuck :: Int -> Program -> IO ()
execBrainfuck n prg = runBrainfuck prg (n, 0, V.replicate n 0)

parseAndRun :: String -> IO ()
parseAndRun str = case parse brainfuck "" str of
                    Right prg -> execBrainfuck 30000 prg
                    Left  err -> print err
