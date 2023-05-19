---
author: Benjamin Bellick
title: Argument-Free Commands in Optparse-Applicative
---

## Problem
I was recently working on a project of my own which uses [Optparse-Applicative](https://hackage.haskell.org/package/optparse-applicative) for command-line parsing of options. However, I needed some way to call commands which have no arguments. 
Given my program conveniently titled `program` and commands titled `cmdA` and `cmdB`, I wanted to be able to call the following:
```console
program cmdA
program cmdB
```
This is with no hidden *optional* arguments whose result we throw out. I wanted the real thing, and I found myself unable to figure it out from the documentation.

I will proceed by showing the simplest sample program which accomplishes this, along with an explanation of why it works.

## Answer
```haskell
import Options.Applicative

data Command = CmdA | CmdB

cmdParser :: Parser Command
cmdParser = subparser
  (  command "cmdA" (info cmdAParse mempty)
  <> command "cmdB" (info cmdBParse mempty)
  )

cmdAParse, cmdBParse :: Parser Command
cmdAParse = pure CmdA
cmdBParse = pure CmdB

main :: IO ()
main = greet =<< execParser opts
  where
    opts = info cmdParser fullDesc

greet :: Command -> IO ()
greet (CmdA) = putStrLn "running command A..."
greet (CmdB) = putStrLn "running command B..."
```
