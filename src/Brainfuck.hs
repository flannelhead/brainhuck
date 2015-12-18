module Brainfuck where

import Data.Monoid

import SimpleParser

-- data Brainfuck next = IncDataPtr next
--                     | DecDataPtr next
--                     | IncByte next
--                     | DecByte next
--                     | ByteOut next
--                     | ByteIn (Char -> next)
data Brainfuck = IncPtr
               | DecPtr
               | IncByte
               | DecByte
               | ByteOut
               | ByteIn
               | LoopStart
               | LoopEnd
               deriving Show

type Program = [Brainfuck]

helloWorld :: String
helloWorld = "a++++++  ++[>++++[>++>def  +++>+++>+<<<<-]>+>+>-g hi>>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."

symbols :: String
symbols = "><+-.,[]"

brainfuck :: Parser Program
brainfuck = pure (:) <*> (skipMany (noneOf symbols) *> chrToken chrToBf)
                     <*> (eof *> pure []) <> brainfuck where
    chrToBf '>' = Just IncPtr
    chrToBf '<' = Just DecPtr
    chrToBf '+' = Just IncByte
    chrToBf '-' = Just DecByte
    chrToBf '.' = Just ByteOut
    chrToBf ',' = Just ByteIn
    chrToBf '[' = Just LoopStart
    chrToBf ']' = Just LoopEnd
    chrToBf _   = Nothing
