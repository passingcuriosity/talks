---
title: So you think you can map [safely]
subtitle: "Or: a Fix for Free"
subsubtitle: "Or: Functional Programming with Structured Graphs"
author: Thomas Sutton
date: 25 July 2018
---

# Introduction

- Algebraic data types...
- ...for trees.
- ...for graphs.
- Problems.
- Solutions.

# Algebraic Data Types

Most of us are familiar with algebraic data types: data types composed of
sums of products, probably with labels on the various bits and pieces so we
can easily tell them apart.

I'll use Haskell syntax because it's objectively better (and the GADT syntax
too):

```haskell
data Operation a where
  Dab     :: Operation a
  Krump   :: Int -> Operation a
  Shuffle :: [a] -> Operation a
  Robot   :: Repr a => a -> Operation a
```

## Recursive *types* give trees

We're also familiar with recursive types to define structures which can contain
other instances of themselves: lists, trees, etc.

```haskell
data List a where
  Nil  :: List a
  Cons :: a -> List a -> List a
```

These define trees. "*Inside*" a `Cons` value is an `a` and another ("smaller")
`List a` value.

## Recursive *values* give graphs

But many languages have a constructs which allow us to construct recursive
values: values which are defined *in terms of themselves*. This shouldn't be
surprising (indeed, recursive type definitions work the same way).

- `letrec` in Scheme, Racket, etc.
- `let rec` in Ocaml, `val rec` in SML.
- `fix` in various languages.
- pretty much every binding form in Haskell.

## "Infinite" lists

```haskell
ones :: List Int
ones = Cons 1 ones
```

## "Infinite" binary trees

```haskell
data Tree a where
  Leaf :: Tree a
  Branch :: Tree a -> Tree a -> Tree a

tree :: Tree
tree = Branch tree tree
```

## Self-referential definitions

```haskell
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
```

## Tying the knot

How do you have two-way links without mutable references? Fixed points!

```haskell
data Tree where
  Tree :: Maybe Tree -> Maybe Tree -> Maybe Tree

tree = Tree Nothing (Just left) (Just right)
  where
    left = Tree (Just parent) Nothing Nothing
    right = Tree (Just parent) Nothing Nothing
```

# Problems

## What does this do?

```haskell
let foo = ... :: [Int]
in map f foo
```

When we "fully evaluate" `bar`:

1. How many times will we apply `f`?

2. How much memory will we allocate?

3. What shape is the resulting data structure?

4. Will it even terminate?

5. Can you tell before hand?

## Who knows?

Even when we know that `foo` is in normal form, that `map` is just `map`, and
that `f` is just the identity function, we don't know whether `map id foo` will
terminate, whether the result (in normal form) will fit in memory, etc.

Similar problems occur with `fold`s and various other recursive functions.

## Then answer

If we think about it a little bit, the answer to most of the questions we
asked is not related to the "size" of `foo` -- to the number of `Int`s in
memory --  but the number of *paths* from `foo`, following pointers, to
`Int`s.

1. The answer is not "`f` is called once per `String` value in memory".

2. The answer is not "`bar` will use memory proportional to the memory used for `foo`".

3. The answer is not "`bar` will be the same shape as `foo` with pointers to `Int`s in place of the pointers to `String`s".

4. The answer is not "`foo` is already completely evaluated and fits in finite memory, so of course it will terminate".

---

1. The answer is "once per path following pointers from `foo` to a `String` value".

2. The answer is "in proportion to the number of paths from `foo` to a `String` value".

3. The answer is "the same as `foo` but with all recursion unfolded".

4. The answer is "`bar` will definitely terminate when `foo` is finite and has no value recursion".

---

What?

---

* Haskell is a non-strict language.

* This means every definition is, potentially, self referential.

* This means every recursive *type* allows for recursive *value*s.

* But we can't, in general, *observe* this value-level recursion so we
  can't keep from breaking these structures when we map.

---

```haskell
ones :: [Int]
ones = 1 : ones
```

We have exactly one cons cell and exactly one `Int`. But we have value
recursion.

---

```haskell
twos :: [Int]
twos = map succ ones
```

Now we have one cons cell and one `Int` for each time `map` follows the `snd`
pointer. And `map` will follow the `snd` pointer an unbounded number of times.

---

So what? If we define infinite data structures we shouldn't expect to be able
to completely process them in finite time!

---

Let's stop "fully evaluating" then.

Maybe we're going to `zipWith (+) selectedSequence inputData` and one sequence
the users can select is `ones`.

No matter how many values are demanded from `ones`, at the end it'll have
allocated exactly one cons cell and exactly one `Int`.

If we do the same with `twos = map succ ones` it'll allocate a new cons cell
and a new `Int` value every time `zipWith` demands the next value.

Even when we stop "fully evaluating" recursive values, it's still displeasing
that a trivial operation on a tiny data structure can changed the time and/or
space complexity of using that structure.

---

Annoyingly there's no way to really deal with this in Haskell. If you didn't
want corecursive data you shouldn't have picked Haskell.

But the problem occurs in any language with one or more of:

* lazy values
* a fixed point operator
* mutable references

---

# Why am I talking about this?

Let's just not do that then!

We can (fast and loose) assume that we don't construct values that have these
problems. Done!

---

But what about recursive structures?

* Streams
* Trees
* Graphs

There are plenty of structures where we want to have the recursive behaviour we
just banned.

---

# Problem

The problem isn't *recursion*. It's *implicit* recursion.

References "back" into the the same structure aren't bad. We just need to be
able to distinguish between references "back" into structure we've already seen
and "forward" into new bits of the structure!

Ignoring mutable data this happens when we define a data structure using a
fixed point (either an explicit operator or implicitly in a language like
Haskell).

The problem arose when we encounter a fixed point in the data structure
and handle it as though it is any old point.

# Solution

So let's find a way to represent and manipulate fixed points in our data
structures appropriately.

We could do it by adding a `Loop` constructor to `Stream` and `Tree` and
`Graph` and ...

But that seems a bit boring (and laborious) so instead we'll use a generic
representation based on parametric higher order abstract syntax (PHOAS) to
handle the tricky bits together with some simple functors to specialise it
to represent streams, trees, graphs, etc.

# Free

Here is the `Free` data type from Haskell's standard library.

```haskell
data Free f a where
  Pure :: a -> Free f a
  Free :: f (Free f a) -> Free f a
```

(Don't worry, I'm only using GADT syntax to help make some things more
obvious.)

# Free the trees

When `f` is an algebraic data type and we obey our stricture about "not doing
that then", values of type `Free f a` are trees:

* The leaves have an `a` value wrapped in a `Pure` constructor.

* The nodes have an `f` of sub-trees wrapped in a `Free` constructor.

This gives us a "definitely not loopy" structure -- they are all tree-ish --
but not the one we're after.

# Free variables

It's worth thinking about what we are about. We want:

1. To describe data structures that are tree-ish; and

2. maybe carrying values at certain places in the structure; and

3. containing references to other parts of the structure; and

4. maintaining some structural invariants when making these references.

What does that last bit mean? We want references that **definitely** point
to another part of the structure. Which is to say it's impossible to "forge"
these recursive references.

# Adding recursion for Free

```haskell
fix :: (a -> a) -> a
mfix :: (a -> m a) -> m a
```

```haskell
data Free f a where
  Pure ::  a                  -> Free f a  -- ^ A leaf
  Free ::       f (Free f a)  -> Free f a  -- ^ An f of branches
  Fix  :: (a -> f (Free f a)) -> Free f a  -- ^ Given a 
```

But be careful. We've moved from 
