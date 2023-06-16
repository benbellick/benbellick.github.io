---
author: Benjamin Bellick
title: If It Quacks Like a Monad- CounterExamples
subtitle: Why the Monad Laws Matter
---
<!--- Alternative title: Illegal monads? -->

# Summary

The ubiquitous monad... surely another tutorial is not needed. 
Though here, I would like to do something slightly-different. 
When a mathematical definition is presented that initially seems kooky, a good practice is to *stress test* the definition by dropping bits and pieces and seeing that each component of the definition truly is required. 
Thus, here you will find a few examples which show that monad instances which *break the laws* are not the kinds of monads you'd want to use. 

# Refresher
Just as a reminder, here is the definition of a monad:
```Haskell
class Monad' m where
  (>>=)  :: m a -> (  a -> m b) -> m b
  (>>)   :: m a ->  m b         -> m b --this can be defined in terms of the other two, and thus can be ignored
  return ::   a                 -> m a
```
For simplicity, the `Applicative` constraint has been removed, as has the declaration of
`(>>)`. This latter choice is okay because it would have default definition:
`m >> k = m >>= \_ -> k`
and here are the corresponding monad laws:

1. `m >>= return     =  m`                        -- right unit
2. `return x >>= f   =  f x`                      -- left unit
3. `(m >>= f) >>= g  =  m >>= (\x -> f x >>= g)`  -- associativity

# Counterexamples
These counterexamples will be _evil_ as the goal is to artificially construct examples which break our intuition. 
Lets consider the following datatype:
```haskell
data State = Empty | Full deriving Show
data MonadA a = M a State deriving Show

instance Monad' MonadA where
  (M a Empty) >>= f = M b Full
    where M b _ = f a
  (M a Full)  >>= f = M b Full
    where M b _ = f a
  return a = M a Empty

```
We can think of this as a monad which maintains a context that is either 
`Empty` or `Full`.
The context initially starts off `Empty` and then is made `Full` once an additional 
piece of data is added.

Now here is the kicker: _this monad is illegal because it breaks *only* the first law._



## unit breaker 
