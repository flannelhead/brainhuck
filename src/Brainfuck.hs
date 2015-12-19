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

simplePrgm :: String
simplePrgm = ",."

brainfuck :: Parser Program
brainfuck = skip *> parseBf
    where parseBf = (:) <$> ((cmd <|> loop) <* skip)
                        <*> (parseBf <|> pure [])
          loop = Loop <$> between (char '[') (char ']') brainfuck
          cmd = foldl1 (<|>) $ map (\(c, f) -> f <$ char c) [
              ('>', IncPtr),  ('<', DecPtr),
              ('+', IncByte), ('-', DecByte),
              ('.', ByteOut), (',', ByteIn) ]
          skip = skipMany (noneOf "><+-.,[]")
