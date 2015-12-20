# Brainhug

## What?
A Brainfuck interpreter. There's also an implementation of a simple parser and
the `Free` monad. The interpreter can also be built against `Control.Monad.Free`
and `Parsec`.

## Why?
This was an exercise project for a functional programming course. Free monads
and applicatives are fun and expressive for implementing parsers and
interpreters. Also, hugs are nicer than curse words.

## Building and running
Clone the repo, build with `stack build` and run with
```
$ stack exec brainhug filename.b
```
