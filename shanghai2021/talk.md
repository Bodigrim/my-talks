# QuickCheck tips and tricks

Hi everyone, thanks for inviting me today. My name is Andrew and I'm gonna talk about effective usage of QuickCheck.

I'm a professional Haskell developer and a board member of Haskell Foundation. I contribute and comaintain many open-source projects, ranging from core libraries such as `bytestring`, `text`, `unix`, `random` and `vector` to heavy-weight mathematical packages for number theory and discrete mathematics. Throughout my career I've written a lot of tests and learned in a hard way that not tested means non-working, even in Haskell world.

While even badly organised test suite is still magnitudes better than no tests at all, I think that too many test suites in the wild underutilise capabilities of QuickCheck framework, leading to suboptimal experience for maintainers and contributors.

I deal a lot with first-time contributors, and you can imagine how frustrating it is when you made a small change and see that some tests became red, but you have little clue what it is caused by. Navigating an unfamiliar codebase, trying to understand what exactly went wrong, is a bad experience.

Today we'll discuss how to make your work with QuickCheck property testing more efficient and fruitful.

Imagine we are writing our own function to reverse lists and would like to test it.

```haskell
module Main where

import Prelude hiding (reverse)
import qualified Prelude
import Control.DeepSeq
import Test.Tasty
import Test.Tasty.QuickCheck

reverse :: [Int] -> [Int]
reverse = undefined

main :: IO ()
main = defaultMain $ testGroup "All"
  [ testProperty "total"      property1
  , testProperty "length"     property2
  , testProperty "head_last"  property3
  , testProperty "involution" property4
  ]

property1 :: [Int] -> Bool
property1 xs = rnf (reverse xs) `seq` True

property2 :: [Int] -> Bool
property2 xs = length xs == length (reverse xs)

property3 :: [Int] -> Bool
property3 xs = head xs == last (reverse xs) && last xs == head (reverse xs)

property4 :: [Int] -> Bool
property4 xs = xs == reverse (reverse xs)
```

Just a quick introduction for uninitiated. QuickCheck is a framework for property testing: instead of writing unit tests, which check only a single case, you specify several properties which must hold for any input of your function. Next QuickCheck generates a ton of random inputs and checks them one by one. When it finds an input, which breaks the suggested property, it makes several attempts to shrink it to provide a minimal counterexample.

This is a pretty simple test suite. We test four properties of `reverse` function:
* First checks that the result is actually a fully-defined value and not an `error`, `undefined` or other form of bottom.
* Then we check that `reverse` preseves the `length`.
* Then that `reverse` is not just an identity: it must swap the first and the last elements of a list.
* Finally, ensure that `reverse` of `reverse` is identity.

This is a pretty standard test suite representation. Now, if we start adding new properties
we need take care not to forget adding them to the `main` as well:

```haskell
  , testProperty "palindrom" property5

property5 :: [Int] -> Bool
property5 xs = let ys = xs ++ reverse xs in ys == reverse ys
```

When there are more than a handful of properties this continual jumping back and force becomes very annoying and - you guess it - error prone. Now there are several solutions here, mostly depending on some sort of `TemplateHaskell` magic to collect test declarations automatically. But note that you also need to invent names for properties: as you can see from this snippet I was too lazy to bother, because life is short and writing tests is boring enough already, right?

There is a simple low-tech solution to this problem, however, which I cannot recommend enough. Just cut the middleman:

```haskell
module Main where

import Prelude hiding (reverse)
import qualified Prelude
import Control.DeepSeq
import Test.Tasty
import Test.Tasty.QuickCheck

reverse :: [Int] -> [Int]
reverse = undefined

main :: IO ()
main = defaultMain $ testGroup "All"
  [ testProperty "total" $
    \xs -> rnf (reverse xs) `seq` True
  , testProperty "length" $
    \xs -> length xs == length (reverse xs)
  , testProperty "head/last" $
    \xs -> head xs == last (reverse xs) && last xs == head (reverse xs)
  , testProperty "involution" $
    \xs -> xs == reverse (reverse xs)
  , testProperty "palindrom" $
    \xs -> let ys = xs ++ reverse xs in ys == reverse ys
  ]
```

Now let's go through our properties one by one to identify areas of improvement.
The first one is mostly fine:

```
$ cabal run -v0 test-reverse -- -p total
All
  total: FAIL
    *** Failed! (after 1 test):
    Exception:
      Prelude.undefined
      CallStack (from HasCallStack):
        error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
        undefined, called at Reverse.hs:10:11 in main:Main
    []
    Use --quickcheck-replay=605708 to reproduce.
```

(Note that I do not use `cabal test`, because I do not want to escape `tasty` arguments)

The only thing to improve is using a less low-level incantation: QuickCheck actually provides `total` helper for this common task, so we can write `\xs -> total (reverse xs)` or even simply `total . reverse`. Let's fix this test by setting `reverse _ = []`.

Now let's run the second property:

```
$ cabal run -v0 test-reverse -- -p length
All
  length: FAIL
    *** Failed! Falsified (after 2 tests and 2 shrinks):
    [0]
    Use --quickcheck-replay=78700 to reproduce.
```

To be honest, I find this output not particularly illuminating. Very well, it says that the test failed for an argument `[0]`. But what exactly went wrong? Like, obviously this is a simple case, and we know what exactly is being tested and how, so can do a mental exercise in equational reasoning to grasp what's up. But in a real world this is one failing test out of thousand, written ten years ago by people long gone.

I'd like to have more insight. One way to do so is adding a trace:

```haskell
import Debug.Trace

  , testProperty "length" $
    \xs -> let lhs = length xs; rhs = length (reverse xs) in
      if lhs == rhs then True else traceShow (xs, lhs, rhs) False
```

The output however is a bit ugly:

```
$ cabal run -v0 test-reverse -- -p length
([0,2],2,0)
([2],1,0)
([0],1,0)
All
  length: FAIL
    *** Failed! Falsified (after 3 tests and 2 shrinks):
    [0]
    Use --quickcheck-replay=436177 to reproduce.
```

You see, when QuickCheck finds a first input, which fails the property, it tries to shrink it to a simpler form and reevaluate the property, so that it can report users the very simplest argument, which fails the test. Since the property is evaluated multiple times, trace is also printed more than once. Traces are printed at undetermined moments of time,
so they can easily interleave with other console output. This is no way to proceed in a non-trivial test suite!

Instead let's look at the type of `testProperty :: Testable a => TestName -> a -> TestTree`. What exactly is `Testable`? Let's check: https://hackage.haskell.org/package/QuickCheck-2.14.2/docs/Test-QuickCheck.html#t:Testable
The most interesting instance is `Testable Property`, where `Property` is a very rich type, provided by QuickCheck exactly for the purpose of storing more information about testing outcomes. There is a set of combinators to work with `Property`, and we'll start with the simplest one, `(===)`. I was initially worried about it, but it appeared that typing triple equality does not turn you into a JavaScript developer, so now I use it without any fear.

```haskell
  , testProperty "length" $
    \xs -> length xs === length (reverse xs)
```

Now the output is much more enlightening:

```
$ cabal run -v0 test-reverse -- -p length
All
  length: FAIL
    *** Failed! Falsified (after 2 tests and 1 shrink):
    [0]
    1 /= 0
    Use --quickcheck-replay=597369 to reproduce.
```

It says that the property failed on `[0]` and what exactly went wrong: namely the left-hand side evaluated to 1, while the right-hand side evaluated to 0. Nice! Let's fix this test: `reverse = id`.

Moving to the next property, who can guess what would happen if we run `$ cabal run -v0 test-reverse -- -p head/last`? Ah, right, our property forgot about the existence of empty lists. QuickCheck provides a special wrapper for this case, `NonEmpty`. Now the property is correct, but its output is quite unhelpful:

```
$ cabal run -v0 test-reverse -- -p head_last
All
  head_last: FAIL
    *** Failed! Falsified (after 2 tests and 3 shrinks):
    NonEmpty {getNonEmpty = [0,1]}
    Use --quickcheck-replay=142635 to reproduce.
```

We'd like to apply our previous trick with `(===)`, but it does not type check out of the box: `Property` is not an instance of `Bits`, so we cannot `(&&)`. Instead QuickCheck provides a dedicated combinator `(.&&.)`:

```haskell
  , testProperty "head_last" $
    \(NonEmpty xs) -> head xs === last (reverse xs) .&&. last xs === head (reverse xs)
```

Now the output is more useful, pointing out what is unequal:

```
$ cabal run -v0 test-reverse -- -p head_last
All
  head_last: FAIL
    *** Failed! Falsified (after 6 tests and 7 shrinks):
    NonEmpty {getNonEmpty = [0,1]}
    0 /= 1
    Use --quickcheck-replay=657704 to reproduce.
```

This is still not ideal, because it does not say us which one of two equalities does not hold. We'll get back to annotating subproperties in a minute, but in this particular case it would be wiser to split this property into two more atomic. This is a good strategy in general: make your properties as simple and as atomic as possible, they should be checking only one thing at a time:

```haskell
  , testProperty "head" $
    \(NonEmpty xs) -> head xs === last (reverse xs)
  , testProperty "last" $
    \(NonEmpty xs) -> last xs === head (reverse xs)
```

All right, these tests cover the behaviour of `reverse` on `NonEmpty` inputs. Let's check that empty lists do not cause troubles. Since this is a test for a fixed inputs, many people feel it like a good moment to reach out for `tasty-hunit` or other framework for unit testing. No need! An argument of `testProperty` does not necessarily need to be a function, check `class Testable` which we looked at earlier. QuickCheck is perfectly capable to cover your unit tests as well:

```haskell
  , testProperty "empty" $
    reverse [] == []
```

Let's modify our definition `reverse` to pass all tests discussed so far:

```haskell
reverse :: [Int] -> [Int]
reverse [] = []
reverse xs = last xs : replicate (length xs - 1) (head xs)
```

Moving further, to the next property, we obtain

```
$ cabal run -v0 test-reverse -- -p involution
All
  reverse: FAIL
    *** Failed! Falsified (after 5 tests and 5 shrinks):
    [0,0,1]
    Use --quickcheck-replay=775922 to reproduce.
```

Again, this is not very helpful. Let's use triple equality:

```
$ cabal run -v0 test-reverse -- -p involution
All
  involution: FAIL
    *** Failed! Falsified (after 6 tests and 7 shrinks):
    [0,0,1]
    [0,0,1] /= [0,1,1]
    Use --quickcheck-replay=75645 to reproduce.
```

That's better. Since lists are small, we can easily spot a position at which they disagree. However, if we were testing a more complicated function with larger counterexamples, it could be unclear what exactly disagrees. Imagine that we switch between implementation, when lists become larger than some limit, so that a bug manifests itself only for sufficiently long lists. Could we make QuickCheck to point out the exact spot, where the discrepancy hides? First of all, we can check for pairwise equality
and check which elements mismatch:

```haskell
    \xs -> let ys = reverse (reverse xs) in
      conjoin $ zipWith (===) xs ys
```

```
$ cabal run -v0 test-reverse -- -p involution
All
  involution: FAIL
    *** Failed! Falsified (after 6 tests and 6 shrinks):
    [0,0,1]
    0 /= 1
    Use --quickcheck-replay=378792 to reproduce.

```

There are two issues, however. First of all, now we lost a high-level picture that `[0,0,1] /= [0,1,1]`. Secondly, it would be nice to say at which position in the list a mismatch occurs. Time to learn a way to annotate QuickCheck properties:

```haskell
    \xs -> let ys = reverse (reverse xs) in
      counterexample (show xs ++ " /= " ++ show ys ++ " because") $
      conjoin $ zipWith3 (\i x y -> counterexample ("at index " ++ show i) (x === y)) [0..] xs ys
```

Now it is much more satisfying:

```
$ cabal run -v0 test-reverse -- -p involution
All
  involution: FAIL
    *** Failed! Falsified (after 5 tests and 8 shrinks):
    [0,0,1]
    [0,0,1] /= [0,1,1] because
    at index 1
    0 /= 1
    Use --quickcheck-replay=424967 to reproduce.
```

Our last property for palindroms also checks equality of lists, so let's abstract this check into a separate helper:

```haskell
  , testProperty "involution" $
    \xs -> xs `areEqualLists` reverse (reverse xs)
  , testProperty "palindrom" $
    \xs -> let ys = xs ++ reverse xs in ys `areEqualLists` reverse ys
  ]

areEqualLists :: (Eq a, Show a) => [a] -> [a] -> Property
areEqualLists xs ys =
  counterexample (show xs ++ " /= " ++ show ys ++ " because") $
    equalElems
  where
    equalElems = conjoin $ zipWith3 equalElem [0..] xs ys
    equalElem i x y = counterexample ("at index " ++ show i) (x === y)
```

There is a slight infidelity in `areEqualLists`: while we check that elements are pairwise equal, we forgot to check that lengths of lists are equal as well; `zipWith3` silently hides such mismatch from us. Let's fix this:

```haskell
areEqualLists :: (Eq a, Show a) => [a] -> [a] -> Property
areEqualLists xs ys =
  counterexample (show xs ++ " /= " ++ show ys ++ " because") $
    equalLength .&&. equalElems
  where
    equalLength = counterexample "length are unequal" (length xs === length ys)
    equalElems = conjoin $ zipWith3 equalElem [0..] xs ys
    equalElem i x y = counterexample ("at index " ++ show i) (x === y)
```

Now if I put `reverse = take 10 . Prelude.reverse`, I'll get a pretty error message:

```
$ cabal run -v0 test-reverse -- -p palindrom
All
  palindrom: FAIL
    *** Failed! Falsified (after 9 tests and 10 shrinks):
    [0,0,0,0,0,0]
    [0,0,0,0,0,0,0,0,0,0,0,0] /= [0,0,0,0,0,0,0,0,0,0] because
    length are unequal
    12 /= 10
    Use --quickcheck-replay=389401 to reproduce.
```

Let's finally write a correct definition of `reverse` and check that all tests are passing:

```haskell
reverse :: [Int] -> [Int]
reverse [] = []
reverse (x : xs) = reverse xs ++ [x]
```

```
$ cabal run -v0 test-reverse
All
  total:      OK
    +++ OK, passed 100 tests.
  length:     OK
    +++ OK, passed 100 tests.
  head:       OK
    +++ OK, passed 100 tests.
  last:       OK
    +++ OK, passed 100 tests.
  empty:      OK
    +++ OK, passed 1 test.
  involution: OK
    +++ OK, passed 100 tests.
  palindrom:  OK
    +++ OK, passed 100 tests.

All 7 tests passed (0.02s)
```

## Testing functions

Imagine that we wish to check whether `map f . map g == map (f . g)` holds for `ByteString`:

```haskell
module Main where

import Test.Tasty
import Test.Tasty.QuickCheck
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = defaultMain $ testProperty "map . map" $
  \f g xs -> let bs = B.pack xs in
    B.map f (B.map g bs) === B.map (f . g) bs
```

But as you can see this does not even compile. The thing is that in order to report a counterexample QuickCheck needs to show all inputs, but since `f` and `g` are functions, there is no `instance Show (Char -> Char)`.
The most brutal way to fix it is to import an orphan instance, which just renders any function as a string `<function>`:

```haskell
import Text.Show.Functions
```

Here are results:

```
$ cabal run -v0 test-mapmap
map . map: FAIL
  *** Failed! Falsified (after 5 tests and 2 shrinks):
  <function>
  <function>
  "A"
  "1" /= "s"
  Use --quickcheck-replay=7556 to reproduce.
```

To a certain degree of surprise, the test fails. There is a slightly better way to achieve the same results without orphans, with a newtype wrapper `Blind`:

```
$ cabal run -v0 test-mapmap
map . map: FAIL
  *** Failed! Falsified (after 9 tests and 2 shrinks):
  (*)
  (*)
  "a"
  "\185" /= "0"
  Use --quickcheck-replay=197864 to reproduce.
```

Anyway, the crucial piece of a puzzle is missing: what are these mysterious functions `f` and `g`, which do not compose? It appears that QuickCheck provides a way to generate functions, which can be introspected:

```haskell
{-# LANGUAGE ViewPatterns #-}

  \(applyFun -> f) (applyFun -> g) xs -> let bs = B.pack xs in
    B.map f (B.map g bs) === B.map (f . g) bs
```

This does provide a very specific answer:

```
$ cabal run -v0 test-mapmap
map . map: FAIL
  *** Failed! Falsified (after 3 tests and 28 shrinks):
  {'\139'->'a', _->'b'}
  {'\v'->'\168843', _->'a'}
  "\v"
  "a" /= "b"
  Use --quickcheck-replay=67720 to reproduce.
```

For those who are curiuos why this test fails: `f . g` on `\v` gives `b` indeed. But when we save intermediate results to a `ByteString`, a truncation of all symbols to a byte range happens, and `\168843` turns into `\139`, eventually resulting in `a` this way.

## RTS options

Last, but not the least: run tests in parallel. Running tests should be as fast as possible, otherwise you'll unconsiously try to avoid it. And limit available RAM, so that when things go awry, swapping does not make you system unresponsive:

```cabal
  ghc-options:      -threaded -rtsopts "-with-rtsopts=-N -M2G"
```
