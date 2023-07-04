---
author: Benjamin Bellick
title: "Breaking the Law (Pt. 1): Illegal Functors"
subtitle: Why the Monad Laws Matter
---
<!--- Alternative title: Illegal monads? -->

## Summary

The ubiquitous monad... surely another tutorial is not needed. 
Though here, I would like to do something slightly-different. 
When a mathematical definition is presented that initially seems kooky, a good practice is to *stress test* the definition by dropping bits and pieces and seeing that each component of the definition truly is required. 
Thus, here you will find a few examples which show that monad instances which *break the laws* are not the kinds of monads you'd want to use. 

## Refresher
Just as a reminder, here is the [definition of a monad](https://wiki.haskell.org/Monad):
```Haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```
Now, any proper functor must satisfy the following laws:

1. `fmap id = id`
2. `fmap (g . h) = fmap g . fmap h

## Counterexamples
These counterexamples will be _evil_ as the goal is to artificially construct examples which break our intuition. 

### Identity Disrupter
Lets consider the following datatype:
```haskell
data State = Empty | Full deriving Show
data FuncA a = Fa a State deriving Show

instance Functor FuncA where
  fmap f (Fa a _) = Fa (f a) Full
```

#### Law 1 
It is easy to see that the first law will be broken. 
`fmap id (Fa 0 Empty) = Fa 0 Full`, so `fmap id /= id`.

#### Law 2
The second law does indeed hold, though it may be slightly more involved to show this.
```
fmap (f . g) (Fa a s) = Fa ((f . g) a) Full
fmap f . fmap g (Fa a s) = fmap f $ Fa (g a) Full = Fa (f (g a)) Full
```

#### Discussion
What makes this functor _bad_? 
Functors can be thought of as values inside of boxes, and `fmap` allows us to reach into those boxes and 
alter the contents of the box without changing the shape of the box. 
This implementation of a `Functor` breaks this rule by changing the contents of the box. 

With this counterexample, you hopefully have a little more of a feel for 
what constitutes a proper `Functor` instance.


### Distributivity Destroyer
Now consider the following datatype:
```haskell
data FuncB a = Fb [a]

instance Functor FuncB where
  fmap f (Fb (x:xs)) = Fb (ys <> [y])
	where Fb ys = fmap f (Fb xs)
		  y = fmap f x
  fmap f (Fb []) = Fb []
```
If it is not immediately obvious what is happening here, 
let me give a brief explanation. 
This instance is essentially just like the traditional list functor
except that it reverses the list as well.
So, `f` is applied to each element of the list, and the list is returned wrapped in the functor, but backwards.
