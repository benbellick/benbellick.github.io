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
Just as a reminder, here is the [definition of a functor](https://wiki.haskell.org/Functor):
```Haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```
Now, any proper functor must satisfy the following laws:

1. `fmap id = id`
2. `fmap (g . h) = fmap g . fmap h`

## Counterexamples
These counterexamples will be _evil_ as the goal is to artificially construct examples which break our intuition. 

### Dual Demolition
To begin with, here is an example which breaks both laws.
Below is its definition:
```haskell
data Func1 a = F1 [a]

instance Functor Func1 where
  fmap f (F1 (x:xs)) = F1 (ys <> [y])
	where F1 ys = fmap f (F1 xs)
		  y = fmap f x
  fmap f (F1 []) = F1 []
```
If it is not immediately obvious what is happening here, 
let me give a brief explanation. 
This instance is essentially just like the traditional list functor
except that it reverses the list as well.
So, `f` is applied to each element of the list, and the list is returned wrapped in the functor, but backwards.

#### Law 1
`fmap id (F1 [1,2]) = F1 [2,1]`, so the first law is broken.

#### Law 2
If you take both `g` and `h` as `id`, the same counter example applies:
`(fmap id . fmap id) (F1 [1,2]) = F1 [1,2] \= F1 [2,1] = fmap id (F1 [1,2])`.

#### Discussion
What makes this functor _bad_? 
Functors can be thought of as values inside of boxes, and `fmap` allows us to reach into those boxes and 
alter the contents of the box without changing the shape of the box. 
This implementation of a `Functor` breaks this rule by changing the order of the contents of the box.


### Identity Disrupter
Now, lets take a look at an instance of `Functor` which breaks the 
first law, but not the second.
Lets consider the following datatype:
```haskell
data State = Empty | Full deriving Show
data Func2 a = F2 a State deriving Show

instance Functor Func2 where
  fmap f (F2 a _) = F2 (f a) Full
```

#### Law 1 
It is easy to see that the first law will be broken. 
`fmap id (F2 0 Empty) = F2 0 Full`, so `fmap id /= id`.

#### Law 2
The second law does indeed hold, though it may be slightly more involved to show this.
```
fmap (f . g) (F2 a s) = F2 ((f . g) a) Full
fmap f . fmap g (F2 a s) = fmap f $ F2 (g a) Full = F2 (f (g a)) Full
```

#### Discussion
Now, what makes this functor _bad_? 
The problem here is similar. 
Our implementation of `fmap` alters the box. 

### Distributivity Destroyer
And now, is there a functor instance that breaks the second law but not the first law? 
After a ceremonious _beating-of-thy-head-against-the-wall_, 
I learned that... it is impossible. \

**What?** \

Yes, impossible. \
Or better to say that it is not possible in the way I imagined. 

The problem is that the first functor law actually **implies** the second[^1].
How can this be so? 
The above referenced post goes into detail, but I will summarize the argument here.
The answer is the so called "free theorem" for `fmap`[^2].
For details, see the reference, but in summary, we can generate
a theorem about a function given its type. 
To derive it yourself, read the paper, 
but to derive it automatically, just ask [lambdabot](https://wiki.haskell.org/Lambdabot)!
```console
<me> @free fmap :: (a -> b) -> (F a -> F b)
<lambdabot> g . h = k . f => $map_F g . fmap h = fmap k . $map_F f
```
Now, supposed the first law holds, i.e. `fmap id = id`.
Let `h = id` and `f = id`. 
Then the above theorem tells us that
`g = k => $map_F g = fmap k => $map_F g = fmap g => $map_F = fmap`.
Okay, so now we can drop the use of `$map_F` and just use `fmap`.
Let `f = id`, and `k = g . h`. 
Then our theorem preciesely tells us that 
`fmap g . fmap h = fmap (g . h)`, i.e. the second functor law!

### Indulgent Inquiry
<a-custom-element>
<p class="deemphasize">
Note: This section is optional and will be slightly more advanced
</p>
</a-custom-element>

Okay, so now we have found that no instance of `Functor` could possibly break the second
law while maintaining the first. 
But there is one last interesting exploration to consider. 
Is there a functor such that an application of `fmap` _collapses_ elements while still following
both the functor laws? 
Our intuition so far is that the functor leaves the box unchanged while alterting its contents.
What if our "box" were a set? 
Then an application of `fmap` with a function `f` that is **not** injective would allow a 
box-change.





[^1]: [Edward Kmett's relevant blog post](https://www.schoolofhaskell.com/user/edwardk/snippets/fmap)
[^2]: [Theorems for Free by P. Wadler](https://people.mpi-sws.org/~dreyer/tor/papers/wadler.pdf)
[^3]: [Categorical Programming for Data Types with
Restricted Parametricity by D. Orchard & A. Mycroft](https://www.cs.kent.ac.uk/people/staff/dao7/drafts/tfp-structures-orchard12.pdf)
