---
author: Benjamin Bellick
title: Argument-Free Commands in Optparse-Applicative
---

## Problem
I was recently working on a project of my own which uses [Optparse-Applicative](https://hackage.haskell.org/package/optparse-applicative) for command-line parsing of options. However, I needed some way to call commands which have no arguments. 
Given my program conveniently titled `program` and commands titled `cmdA` and `cmdB`, I wanted to be able to call the following:
```shell
> program cmdA
> program cmdB
```
This is with no hidden *optional* arguments whose result we throw out. I wanted the real thing, and I found myself unable to figure it out from the documentation.

## Solution
The solution is to create a type constructor which takes no arguments, and then to lift the result via pure.
```Haskell
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
greet CmdA = putStrLn "running command A..."
greet CmdB = putStrLn "running command B..."
```

If it is unclear why this works, recall that a `Parser` is an applicative (it is a well-named library after all). 
As a reminder of what an applicative is, look no further than below:
``` Haskell
class (Functor f) => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```
So, `pure` lifts the value `CmdA` of type `Command` to be a full-fledged instance of `Parser Command`.

One valid question to ask is how does the parser know to parse for "cmdA" or "cmdB"? The answer is the first argument passed into the `command` function.
If we instead defined `cmdParser` as below, our program's commands would be "commandABC" and "commandDEF":
```Haskell
cmdParser :: Parser Command
cmdParser = subparser
  (  command "commandABC" (info cmdAParse mempty)
  <> command "commandDEF" (info cmdBParse mempty)
  )
```

I hope that was useful. Please contact me with any questions or comments!
<!---
<details>

<summary><a>Click Here for Dumb Rabbit Hole</a></summary>
## Extra
What if we have an unruly amount of commands? We can make our lives a little more convenient with the following implementation.
``` Haskell
import Options.Applicative

data Command = CmdA
             | CmdB
             | CmdC
             | CmdD
             | CmdE
             | CmdF
             | CmdG deriving (Show)
             

cmdParser :: Parser Command
cmdParser = subparser $ foldl (\comP comV -> comP <> mkCommandSubP comV)  mempty commands
	where mkCommandSubP comV = command (show comV) (info (pure comV) mempty)
		  commands = [CmdA, CmdB, CmdC, CmdE, CmdF, CmdG]
main :: IO ()
main = greet =<< execParser opts
  where
    opts = info cmdParser fullDesc

greet :: Command -> IO ()
greet cmd = putStrLn $ "running command " ++ show cmd ++ "..."
```
</details>
-->

<!--  LocalWords:  Optparse
 -->
