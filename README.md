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

Say we want to `map` `add1` over this data before reducing, like:

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

`Tmap` is a transducer generator that generates mapping transducers.
It takes a function that can perform one "step" of the mapping (here `add1`)
and returns a transducer.

There isn't any trickery here: this reducing function is simply used in every step of the
reduce. Here is each step of the `reduce` explicitly.

```racket
(define trans ((Tmap add1) conj))

(trans '() 1)
;=> '(2)

(trans '(2) 2)
;=> '(2 3)

(trans '(2 3) 3)
;=> '(2 3 4)

(trans '(2 3 4) 4)
;=> '(2 3 4 5)
```

Notice the how if we defined `trans` as follows, we would get the same answers.

```racket
(define trans (lambda (a v)
                (conj a (add1 v))))
```

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

The trick that `filter` requires is to *skip* processing a member based on a predicate.
Notice that the filtering transducer achieves this.

```racket
(define Ftrans ((Tfilter even?) conj))

(Ftrans '() 1)
;=> '()

(Ftrans '() 2)
;=> '(2)

(Ftrans '(2) 3)
;=> '(2)

(Ftrans '(2) 4)
;=> '(2 4)
```

### Composing transducers

Consider this reduction whose input has already been walked twice.

```racket
(reduce (lambda (a v)
          (conj a v))
        '() (filter even? (map sub1 '(1 2 3 4))))
;=> '(0 2)
```

We can manually push both the `map` and `filter` inside
the reducing function, saving execution time and intermediate
lists.

```racket
(reduce (lambda (a v)
          (let ([v (sub1 v)])
            (if (even? v)
              (conj a v)
              a)))
        '() '(1 2 3 4))
;=> '(0 2)
```

We can *compose* transducers to build a transducing pipeline.

```racket
(reduce ((compose (Tmap sub1) (Tfilter even?))
         conj)
        '() '(1 2 3 4))
;=> '(0 2)
```

This is much nicer with `transduce`.

```racket
(transduce (compose (Tmap sub1) (Tfilter even?))
           conj
           '() '(1 2 3 4))
;=> '(0 2)
```

Given `compose` composes right-to-left, this is surprising!

```racket
(define subfilter
  (compose (curry filter even?)
           (curry map sub1)))

(subfilter '(1 2 3 4))
;=> '(0 2)
```

Transducers compose *backwards*.

```racket
(define Tsubfilter
  ((compose (Tmap sub1)
            (Tfilter even?))
   conj))

(Tsubfilter '() 1)
;=> '(0)

(Tsubfilter '(0) 2)
;=> '(0)

(Tsubfilter '(0) 3)
;=> '(0 2)

(Tsubfilter '(0 2) 4)
;=> '(0 2)
```

To understand this, we first expand the definitions of `(Tmap sub1)`
and `(Tfilter even?)` independently.

#### Expanding (Tmap sub1)

This is the expansion of a mapping transducer. It's just a function that takes
a reducing function and returns a reducing function.

```racket
(Tmap sub1)
;--->
  (lambda ([rf : ((Listof Number) Number -> (Listof Number))])
    (lambda ([result : (Listof Number)]
             [input : Number])
      (rf result (sub1 input))))
```

If we actually apply the mapping transducer to a reducing function, we get back
a new reducing function. Notice this does exactly what we want for a single step
of a `map`: perform the operation (subtraction), then append to the list.

```racket
((Tmap sub1) conj)
;--->
  (lambda ([result : (Listof Number)]
           [input : Number])
    (conj result (sub1 input)))
```

#### Expanding (Tfilter sub1)

The filtering transducer is similar: it takes a reducing function and returns a reducing
function that might call the passed in function if the predicate passes.

```racket
(Tfilter even?)
;--->
  (lambda ([rf : ((Listof Number) Number -> (Listof Number))])
    (lambda ([result : (Listof Number)]
             [input : Number])
      (if (even? input)
          (rf result input)
          result)))
```

Providing a reducing function as `conj` gives us the stepper function for a `filter` defined
with `reduce`.

```racket
((Tfilter even?) conj)
;--->
    (lambda ([result : (Listof Number)]
             [input : Number])
      (if (even? input)
          (conj result input)
          result))
```

#### Expanding (compose (Tmap sub1) (Tfilter even?))

Here's where it gets interesting.
Composing transducers works out left-to-right, so we should end up
with a reducing function that first performs a step of `(Tmap sub1)`
then a step of `(Tfilter even?)`.

By composing two transducers we get another transducer: again, simply a function
that takes a reducing function and returns one.
(The definition of `compose` is inlined).

```racket
(compose (Tmap sub1) (Tfilter even?))
;---->
    (lambda ([rf : ((Listof Number) Number -> (Listof Number))])
      ((Tmap sub1)
        ((Tfilter even?)
         rf)))
```

Observe what happens when we apply a reducing function.

```racket
((compose (Tmap sub1) (Tfilter even?))
 conj)
;---->
    ((Tmap sub1)
      ((Tfilter even?)
       conj))
;---->
;; expand Tfilter
    ((Tmap sub1)
     (lambda ([result : (Listof Number)]
              [input : Number])
       (if (even? input)
           (conj result input)
           result)))
;---->
;; expand Tmap
    ((lambda ([rf : ((Listof Number) Number -> (Listof Number))])
       (lambda ([result : (Listof Number)]
                [input : Number])
         (rf result (sub1 input))))
     (lambda ([result : (Listof Number)]
              [input : Number])
       (if (even? input)
           (conj result input)
           result)))
;---->
;; beta reduction
    (lambda ([result : (Listof Number)]
             [input : Number])
       ((lambda ([result : (Listof Number)]
                 [input : Number])
         (if (even? input)
           (conj result input)
           result))
       result (sub1 input)))
;---->
;; convert inner lambda -> let
    (lambda ([result : (Listof Number)]
             [input : Number])
      (let ([input (sub1 input)])
        (if (even? input)
          (conj result input)
          result)))
```

Wow! The exact reducing function that we want appears.

Now we can follow exactly why

```racket
(reduce ((compose (Tmap sub1)
                  (Tfilter even?))
         conj)
        '() '(1 2 3 4))
```

is equivalent to 

```racket
(reduce (lambda ([result : (Listof Number)]
                 [input : Number])
          (let ([input (sub1 input)])
            (if (even? input)
              (conj result input)
              result)))
        '() '(1 2 3 4))
```

### Benchmarks

I performed some microbenchmarks. I consistently observed speedup as I moved nested list operations
into a reducing function.

This is unsurprising, since `map` and `filter` are eager in Racket, so for large lists we are saving
considerable time from traversing them multiple times.

I ran each benchmark over 100 iterations, with a 100,000 element list, on a Ubuntu VM with 1gb RAM
on 2.4GHz Intel Core i5, 2012 MacBook Pro.

#### All transducers

All transformations are performed in one pass.

```racket
(transduce (compose (Tmap add1) (Tfilter even?)) + 0 big-list)
;; cpu time: 1300 real time: 1274 gc time: 100
```

#### Partial transducers

Moving the map outside means one extra pass.

```racket
(transduce (Tfilter even?) + 0 (map add1 big-list))
cpu time: 1624 real time: 1573 gc time: 116
```

This was 300ms slower than if `map` was a transducer.

#### No transducers

Moving the map and filter outside means two extra passes.

```racket
(reduce + 0 (filter even? (map add1 big-list)))
cpu time: 2008 real time: 1963 gc time: 92
```

This was 600ms slower than if `map` and `filter were transducers.

## Takeaways

Metaprogramming comes in all shapes and sizes, depending on the context and
the goals of a project.

Stream Fusion was designed to leverage existing idioms by statically rewriting
common list operations to fuse them together without intermediate data structures.

Transducers were first invented for Clojure as a new way of compositing data transformations
generically. This form of runtime metaprogramming did not leverage existing idioms. Outside
of downstream operations internally using transducers, there is no automatic speedup for user-code.
