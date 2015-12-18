module Brainfuck where

--import Text.Parsec
--import Text.Parsec.String
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
               | Loop [Brainfuck]
               deriving Show

type Program = [Brainfuck]

helloWorld :: String
helloWorld = "a++++  ++++[>++++[>++>def  +++>+++>+<<<<-]>+>+>-g hi>>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.a"

brainfuck :: Parser Program
brainfuck = skip *> parseBf
    where parseBf = (:) <$> ((bfSymbol <|> loop) <* skip)
                        <*> (parseBf <|> pure [])
          loop = Loop <$> between (char '[') (char ']') brainfuck
          bfSymbol = foldl1 (<|>) $ map (\(c, f) -> char c *> pure f)
              [('>', IncPtr), ('<', DecPtr), ('+', IncByte), ('-', DecByte),
               ('.', ByteOut), (',', ByteIn)]
          skip = skipMany (noneOf "><+-.,[]")
