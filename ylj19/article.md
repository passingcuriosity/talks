---
title: Yow Lambda Jam 2019 Roundup
author: Thomas Sutton
date: 22 May 2019
---

I went to Yow Lambda Jam 2019 in Melbourne last week and this is a short
roundup of the talks and workshops that I attended. Overall I thought it was
not quite so good a programme as last year, but there were a nice range of
talks on some interesting topics. These are some notes and observations on
my favourite talks on the programme.

## Bartosz Milewski - A taste of type theory

Some of us are familiar with Bartosz from his well known category theory
writing but his first visit to Lambda Jam was to talk about *type theory*.

He began discussing the notion of equality-by-isomorphism and the way it
breaks down in programming languages: `a` and `Unit * a` and `a * Unit` are
isomorphic but certainly aren't "equal" in a program context (if they were
we would expect to be able to substitute them for each other, etc.)

Many type theories see types described by *introduction forms*, *elimintation
forms*, *computation rules*, and uniqueness proofs. Bartosz described recursive
types and dependant types in these terms with reference to `Nat` and `Vec Nat t`
-- the natural numbers and length-indexed vectors. In the course of this we saw
the `rec` recursion and `ind` operators which eliminate these types.

```
rec : a -> (Nat -> a -> a) -> Nat -> a

ind : C Z -> ((n : Nat) -> C n -> C (S n)) -> (n : Nat) -> C n
```

After this short introduction to be meat of type theory the talk moved on to
Curry-Howard "propositions = types" and "true = inhabited". In this sense,
mathematical induction is the dependent elimination of natural numbers (`ind`
mentioned above).

The rest of the talk discussed equality types -- expressing propositions of
equality between terms. A term `p : Id(T, x, y)` is a proof (or witness) that
terms `x : T` and `y : T` are identical (equal).

The obvious way to get one of these is the reflexive constructor:

```
refl : {T} -> (a : T) -> Id(T, a, a)
```

`Id(T, x, y)` is a dependent type (it's a type, `x` and `y` are terms) so it's
elimination form will look a little like `ind`, but where `Nat` hasd two
constructors `Id` only has one.

```
ind : (forall T. (z : T) -> C (z, z, refl z))
    -> (x : A) -> (y : A) -> (p : Id(A, x, y)) -> C (x,y,p)
```

If you can provide a way of taking a value `a : T` and building whatever it is
you're interested in (in the family of types `C (a:T) (b:T) (p:Id(T,a,b))`),
then you can use `ind` to build a term in a type of that family from any two
terms which you can prove are equal.

If you don't really know what is meant by "type theory" or haven't encountered
equality types before, this is a good first introduction.

> The obvious question at the end of this is "so what"?
>
> You might be familiar with the idea of type theories being the "internal
> language" of certain [classes of] categories. This is what we mean when we
> the connection between Cartesian closed categories and the Lambda calculus.
>
> In some settings, the internal language has identity types that are *not*
> trivial. This is why the abstract mentioned *homotopy type theory*, but the
> talk didn't get that far.

## Philip Wadler - (Programming languages) in Agda = Programming (languages in Agda)

Philip Wadler is a name that should be familiar to typed functional programmers,
JVM people, and regular Lambda Jam attendees (because Monads, generics, and
speaking a few times now). This year he spoke about his new textbook with Wen
Kokke: [Programming Language Foundations in Agda](https://plfa.github.io/).
PLFA is something of a "Software Foundations in Agda instead of Coq", if that
helps any. 

This book is freely available on GitHub and uses Agda to teach programming
language theory (and to teach Agda itself on the way). In the talk and workshop
Phil worked through the first few chapters, solving exercises with natural
numbers and relations (this is more or less part of the *learning Adga* bit).

There were some interesting observations beyond just presenting the book.
Recommended if you like Phil, Agda, programming language theory, theorm proving,
Emacs, etc.

> Lambda Man made his regular appearance.

## Edward Kmett - Logic programming a la carte

Ed Kmett have a long, breakneck talk about things he's is doing in implementing
a scalable logic programming system. It touched -- very briefly and very, very
quickly -- on a lot of ground and linked back to his previous talks on
propagators and a bunch of other stuff.

The crux of it is that the recent cottage industry of logic programming embedded
into functional programming lanaguages (spawned by Oleg's work and popularised
in The Reasoned Schemer) is hamstrung due some the implementation techniques
used. Ed is using state of the art in a lot of fields to make a system that can
solve more problems faster.

Lots of stuff about logic programming over programs (program synthesis),
efficient implementation of logic variables, union-find implementation, etc.

If you've run out of areas of computer science and mathematics to read about,
this might be a good source of material to keep you busy for 12 months or so.

Apparently Ed's been doing lots of live coding on twitch.tv/ekmett

## Ben Lippmeier - Types (are / want to be) calling conventions

> "Salt" is what you get when you leave "C" out in the sun.

Ben gave a talk describing `Salt` -- the language he has designed to be a good
intermediate representation for compiling functional programming languages.
Types leave out a lot of detail that is important for compiling and emiting
machine code. The focus of this talk is calling conventions:

```
f1 : Int -> Int -> Int
f1 = \x. \y. x + y

f2 : Int -> Int -> Int
f2 x = \y. x + y

f3 : Int -> Int -> Int
f3 x y = x + y
```

All three terms have the same type but we (can? should?) emit different code
in each case:

1. a pointer to a function which returns a function
2. a procedure which returns a function
3. a procedure of two arguments

In Salt these are represented at the type level by using vectors of parameters
and returns:

```
f1 : [[Int] -> [[Int] -> [Int]]]
f1 = \x. \y. x + y

f2 : [Int] -> [[Int] -> [Int]]
f2 x = \y. x + y

f3 : [Int; Int] -> [Int]
f3 x y = x + y
```

This additional information makes it easier to communicate calling conventions
of individual objects between the frontend and backend of the compiler, making
it easier to choose the most efficient convention on a case-by-case basis.

```
int f3(int x, int y) { return x + y; }
```

So we can emit code that avoids unneccesary boxing (for heap allocated pointers
to functions), tupling (for multiple arguments), etc.

> Ben's talks always good. If you're interesting in functional programming,
> different type systems, or compilers give it a watch.

## David Laing - DMaps for delightful dynamism

David talked about the `DMaps` data structure from the `dependant-map` package.
This is a copy'n'paste of `Data.Map` which has been updated to have keys from
a Generalised Algebraic Data Type (GADT). This allows heterogeneous values in
the map without sacrificing type safety.

The interface is very flexible and David describes a number of ways to use it
to with validation, optional fields, etc. which might otherways result in having
multiple copies of data types (on the wire, validated, saved to the DB, etc)

> Recommended for the interesting data structure and real-world usecases.

### Jonathon Merritt and Luke Clifton - Haskell spaceflight workshop

Jonathon and Luke (both great developers, both still at CBA) presented a
workshop on doing space-flight related programing in Haskell. Essentially,
they dug out the details for a bunch of algorithms used in the planning and/or
execution of historical space flight missions and ran a workshop to implement
(parts of) them.

The first few exercises require you to fill in missing details of ODE solvers

- the Euler method on `Double`
- the Euler method using Conal Elliot's `vector-space` library
- 4th order Runge-Kutta on `vector-space`

The driver code that they provided was very nice -- if you use iTerm2 graphs of
the simulations executed with your implementation are displayed in the terminal.

The code is at https://github.com/lancelet/space-workshop

> This was a really great workshop.
>
> It's particularly interesting as an experiment with using libraries like
> `vector-space` and `units`. Is the extra effect required to use these sorts
> of libraries worth it? I'm still not sure of my answer.

### Ken Scambler - Applied category theory

Applied category theory is a relatively new emerging field where category
theory and its tools are deployed to solve "applied" problems. Ken presented
some of these tools by examining a few examples from the recent *Seven sketches
of compositionality* book.

> Ken is a good presenter and the topic is interesting. Maybe watch the talk
> to see if the book looks interesting.

### Dmitrii Kovanikov - `co-log`

`co-log` is a logging library for Haskell. It wraps logging actions (e.g.
`str => log.debug(str)`) in a newtype and provides a bunch of APIs to use them
in a structured way:

- Semigroup to combine actions (send a message to both loggers)
- Monoid to add a "do nothing" logger
- Contravariant apply formatting to messages sent to a logger
- a Monadic variant to apply effectful logging (e.g. adding the time to each
  message)
- Divisible to split messages and handle parts separately (e.g. exception
  message goes to stderr and a full stack trace to a log file.)
- Decidable to route messages to different handlers (e.g. log levels)

It has an interface *inspired by* comonad, but not an actual instance.

> This was a good talk about a practical, pragmatic library addressing concerns
> that almost all applications. Turns out functors are useful.

## All the blockchains

There were two talks (that I saw) about blockchain-y stuff.

Andrae Muys (Digital Asset) talked about DAML. This was a good description of
the system and the way it provides the guarantees it does.

Manuel Chakravarty (IOHK) talked about Plutus. Plutus is a language for "smart"
"contracts" on "blockchains". Unlike many such languages, it isn't an buggy,
underspecified, implementation dependenty piece of ad hoccery. At it's core,
it's $F_{\omega}$ with the added concept of "gas" (I guess because blockchainers
can't manage to learn new words).

> I have negative interest in blockchain stuff, but the talks were both good.
> I recommended Andrae's talk in particular for the architectural aspects and
> Manuel's for languagey stuff.

## Attila Egri-Nagy - Approaching the Yoneda Lemma

Last year Attila spoke about "semigroup programming". This time he attempted to
give an introduction to the Yoneda Lemma by way of an example from group theory.

> If you do know group theory but don't understand the Yoneda Lemma, this might
> help you.
>
> If you don't know group theory, this might be interesting for the group theory
> content.

## Andrew McMiddlin - GHC language extensions

Relatively little "Haskell" is actually Haskell as specific in the Haskell 2010
language report.

Andrew described a bunch of common GHC language extensions.

## Jon Pretty - Eleven little life-enhancing libraries for Scala

Jon gave a short introduction to eleven little Scala libraries he's written.

They're all little, single concern things around stuff like string
interpolation, deserialising CSV, using annotation, computing digests, etc.
