# Program Fusion

This repository demonstrates two approaches to program fusion:

  1. Stream Fusion, an example of compile-time metaprogramming, and
  2. Transducers, an example of runtime metaprogramming.

## Stream Fusion - GHC Rewrite rules

The first implementation is a set of GHC rewrite rules that makes up
an implementation of [Stream Fusion](http://citeseer.ist.psu.edu/viewdoc/summary?doi=10.1.1.104.7401).

The high level idea of Stream Fusion is to transform nested operations
over lists into a single pass with no intermediate data structures.
For example

```haskell
filter even $ map (+1) [1, 2, 3 ,4]
```

must build an intermediate representation for the result of the inner `map`.

Note that Haskell implements the call-by-need evaluation strategy, which means that the `filter` and `map`
operations are only realized as the caller requires them.
So Stream Fusion does not (need to) avoid redundant passes over lists in Haskell, but
in a call-by-value language with eager implementations of `filter` and `map`, this approach
could provide considerable speedup over large lists.


### From Lists, to Streams ...

Stream Fusion first converts list operations to be over Streams.

```haskell
data Stream a = forall s. Stream (s -> Step a s) s
data Step a s = Done
              | Yield a s
              | Skip s
```

For our purposes, `s` is a type of list and `a` is one of its elements.

Streams are converted back and forth from lists via `stream` and `unstream`.

```haskell
stream :: [a] -> Stream a
stream xs0 = (Stream next xs0)
  where
    next []       = Done
    next (x : xs) = Yield x xs

unstream :: Stream a -> [a]
unstream (Stream next0 s0) = (unfold s0)
  where
    unfold s = case next0 s of
      Done       -> []
      Skip    s' ->     unfold s'
      Yield x s' -> x : unfold s'
```

We now define `map` and `filter` in terms of these primitives via static rewrite rules.

```clojure
{-# RULES
"map -> fusible" [1]
   forall f xs. map f xs = (unstream (mapS f (stream xs)))
"filter -> fusible" [1]
   forall f xs. filter f xs = (unstream (filterS f (stream xs)))
  #-}
```

### ... to Nothing at all

But now our running example is pointlessly converting betwenn intermediate *stream* data structures.
By our rewrite rules, we have now inlined:

```haskell
--                        vvvvvvvvvvvvvvvvv
unstream . filterS even . stream . unstream . mapS . stream
```

One extra rewrite rule can fix this for us.

```haskell
{-# RULES
"stream/unstream fusion" [0]
   forall s. stream (unstream s) = s
  #-}
```

This removes any such redundant operations.

```haskell
unstream . filterS even . mapS . stream
```

### Inlining

Haskell functions do their own inlining automatically.
This interacts in unpredictable ways with GHC rewrite rules.

In order to ensure some rules fired, I had to suppress inlining
for some functions. For example, to get the `"stream/unstream fusion"`
rule firing, these directives were necessary.

```haskell
{-# NOINLINE [1] stream #-}
{-# NOINLINE [1] unstream #-}
```

### Evaluation

I failed to install Criterion via Cabal, and didn't complete any benchmarking.

To enable rewrite rules, I used `-fenable-rewrite-rules`.

To ensure the rewrite rules fired, I did some crude testing debugging flags.

```bash
-ddump-simpl-stats -ddump-rule-firings -dppr-debug
```

I'm fairly confident the `"map -> fusible"` rule is firing, however I'm unsure 
about `"stream/unstream fusion"`. 

## Transducers - Racket

Transducers are a way to build composable data transformations designed to work over `foldl`/`reduce`.

They were [first introduced](http://clojure.org/transducers) by Rich Hickey as a Clojure 1.7 feature.

## Basics

A *reducing function* is a function passed to `reduce`, that takes an accumulator and a value, then
returns another accumulator.

`conj` is a reducing function (argument order like `snoc` for Schemers).

```racket
; (: conj (All (a b) (-> (List a) a (List a))))
(define conj (lambda (r v) (append r `(,v))))
```

Notice the type of conj: the first parameter and return types are identical, and it takes some input
as second parameter. It is a reducing function.

```racket
(conj '() 1)
;=> '(1)

(conj '(1) 2)
;=> '(1 2)
```

We can use this over `reduce`:

```racket
(reduce (lambda (a v)
          (conj a v))
        '() '(1 2 3 4))
;=> '(1 2 3 4)
```

### Mapping transducer

Say we want to `map` `add` over this data before reducing, like:

```racket
(reduce (lambda (a v)
          (conj a v))
        '() (map add1 '(1 2 3 4)))
;=> '(2 3 4 5)
```

To save walking the list twice, we can push the `map` into the reducing
function.

```racket
(reduce (lambda (a v)
          (conj a (add1 v)))
        '() '(1 2 3 4))
;=> '(2 3 4 5)
```

Transducers have a pattern for this shape of reducing function via a *mapping transducer*.

```racket
(reduce ((Tmap add1) conj)
        '() '(1 2 3 4))
;=> '(2 3 4 5)
```

`transduce` expresses this fold more readably. It has the same signature as `reduce` but
takes a transducer as the first argument.

```racket
(transduce (Tmap add1) conj
           '() '(1 2 3 4))
;= > '(2 3 4 5)
```

### What *is* a transducer?

A transducer is a function that takes a reducing function and returns a reducing function.

For example, `(Tmap add1)` is a transducer. Applying it to a reducing function
as `((Tmap add1) conj)` results in another reducing function.

### Filtering transducer

Let's implement a `filter` over `even?` as a `reduce`.

```racket
(reduce (lambda (a v)
          (if (even? v)
            (conj a v)
            a))
        '() '(1 2 3 4))
;=> '(2 4)
```

This can be expressed with a filtering transducer.

```racket
(reduce ((Tfilter even?) conj)
        '() '(1 2 3 4))
;=> '(2 4)
```

### Composing transducers

Let's express a pipeline of transducers 

```racket

### Benchmarks
