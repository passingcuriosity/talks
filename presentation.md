---
title: So you think you can map [safely]
subtitle: "And: a Fix for Free"
author: Thomas Sutton
date: 25 July 2018
---

# Question

Suppose you have a recursive algebraic data type with a single parameter:

```haskell
data Foo a = Nowt | Summat a (Foo a)
```

---

And it has an instance of `Functor` and, further more, it's the obvious
and "correct" instance:

```haskell
deriving instance Functor Foo
```

---

Suppose that `foo` is a value of this type and it's in normal form: we've
fully evaluated the whole data structure and it's sat there in memory.

```haskell
foo :: Foo String
foo = ...
```

---

What does this do (when fully evaluated)?

```haskell
map f foo
```

1. How many times will we apply `f`?

2. How much memory will we allocate?

3. What shape is the resulting data structure?

---

What does this do (when fully evaluated)?

```haskell
let bar = map f foo
```

1. How many times will we apply `f`?

2. How much memory will we allocate?

3. What shape is the resulting data structure?

4. Will it even terminate?

---

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

2. which contain values; and

3. references to other parts of the structure; and

4. the reference are *valid* w.r.t. to the structural invariants.

What does that last bit mean? We want references that **definitely** point
to another part of the structure.

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