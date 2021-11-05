This is a collection of links I found to be useful
when learning about various
theoretical computer science and
abstract algebra concepts.
It's largely made for myself,
but I've structured it so that anybody should be able to make use of it.
They assume a basic knowledge of functional programming
(if you're at Edi, that implies you've taken inf1a-fp at least),
but I've tried to note where extra learning is necessary.

## Category Theory
You've probably heard that Haskell is based around Category Theory.
CT is a really abstract mathematical field,
and isn't usually taught at undergrad.
However, the underlying concepts are often deceptively simple,
and with the right frame of mind,
you could easily learn enough to massively improve your Haskell skills.

- [Bartosz Milewski's Category Theory for Programmers](https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/):
This was my introduction to Category Theory,
and it's an excellent followup from the FP course.
Examples are given throughout in diagrams, Haskell and C++ syntax
to best display the concepts being discussed,
and it gives excellent insights as to
why these concepts *matter* for programming.
Be sure to watch the accompanying video lectures,
and if you like it enough,
the hardback version is only £25 including shipping
(and is absolutely worth it)!
I'd recommend checking out his other blog posts too.

- [An Invitation to Applied Category Theory (by David Spivak)](https://www.cambridge.org/core/books/an-invitation-to-applied-category-theory/D4C5E5C2B019B2F9B8CE9A4E9E84D6BC):
This book uses more examples from mathematics proper,
and while technically an introduction to
*Applied* Category Theory,
which is a slightly different field,
it serves as a very good motivator
and gives great intuition
if you already have some experience
with abstract algebra.

- [The Catsters](https://www.youtube.com/user/TheCatsters):
small, old youtube channel with worryingly energetic hosts.
They give really good, if absurdly fast,
explanations of most common topics in category theory.
I watched these alongside the above video lectures,
and it's really interesting to see the same concepts
approached from a pure theoretical-algebra standpoint.

- [ncatlab](https://ncatlab.org/nlab/show/HomePage):
Once you're deep, and I mean *deep*
into your category theory
(I am not yet at this point),
you might find this wiki helpful.
It's a very comprehensive manual to
many different parts of abstract algebra,
mainly focusing on category theory.
Be sure to check out their
[CT textbook list](https://ncatlab.org/nlab/show/category+theory#TextBooks)
(you should be able to find most of these through DiscoverEd
or your uni's research portal,
and if not, `reverse "negbil"`)

## Lambda Calculus and other parts of Theoretical Computer Science
This section focuses mainly on topics related to the theory of computation.
Prior knowledge is basic/intermediate Haskell and logic notation
(again, if you passed inf1a at Edinburgh you're ready)

- [The `fix`point combinator](https://rebeccaskinner.net/posts/2021-06-09-getting-to-the-fixed-point.html):
This blog post is an *incredible* explanation of
exactly why the fixpoint combinator works,
and how it's implemented in Haskell.
It's a really good introduction to
the various different combinators and recursion schemes
that allow you to write *really* terse programs.

## Monads etc.
Ah, monads.
The "just trust me dude" of Haskell,
at least from the perspective of a beginner.
While I recommend learning general category theory first
(it's what I did!)
to fully understand them,
it's handy to know how to use them regardless.

- [Nottingham course on Advanced FP](https://www.youtube.com/watch?v=2u0T7z6O9jM&list=PLF1Z-APd9zK5uFc8FKr_di9bfsYv8-lbc):
A series of youtube lectures from the University of Nottingham.
If you've taken inf1a, I would skip the first five lectures here.
They're a great introduction to functors, applicatives and monads
from a practical viewpoint.

- [The Extended Functor Family - George Wilson](https://www.youtube.com/watch?v=JZPXzJ5tp9w) [21:56]:
An excellent introduction to contravariant functors and profunctors,
and their use in Haskell.
The linked talks/posts at the end are worth checking out too.


## Practical Haskell
As much as I like messing around with abstract concepts,
it's *occasionally* useful to compute things.
After all, what use is a computer if you can't use it to compute things?
The following is a collection of links
that should help you in writing more idiomatic Haskell
and in using GHC to your full advantage.

- [Tweag/Richard Eisenberg's videos](https://www.youtube.com/channel/UCI1Z201n-8OelkSg0DVOsng):
Richard has made an extensive series of videos
that cover many different areas in Haskell programming.
Richard (at the time of writing) works on GHC's internals,
and has a wealth of knowledge
which he shares through these videos.

- [forall Type Quantifier](https://github.com/wasp-lang/haskell-handbook/blob/master/forall.md):
Short github post that explains how the `forall` keyword
interacts with various different GHC extensions.
Contains some nifty tricks for
working with type-level programming.


## Type Theory
Type theory is a very active area of interest
in theoretical CS as of the time of writing.
It plays a fundamental role in the theory of computation
and can allow us to reason about code
in a very rigorous manner
(something that is very useful for compiler devs).
It's a natural link between Haskell,
Category Theory and the various logic algebras.

- [Homotopy Type Theory](https://homotopytypetheory.org/book/):
HoTT is a fairly recent development/discovery,
which links together the fields of homotopy and type theory.
It can be used as a base set of axioms
from which one can derive all of modern mathematics,
much like with set theory with ZFC.
The advantage is that it maps much more cleanly
to programming than set theory + ZFC does,
a fact that has led to it being used in
most modern theorem provers such as Agda and Coq.


## Topology, Homotopy and Homology
These closely related fields of maths
deal mainly with "manifolds",
or continuous surfaces in spaces.
There's many interesting results
that can be widely applied to different fields
(as exemplified by HoTT above).
These links should serve as a decent primer
into the various topics,
and provide good intuition for further study.

- [Clark Barwick](https://www.youtube.com/user/clarkbarwick):
YouTube channel of a lecturer from UoE!
He's an excellent educator
and has a very good (and rigorous)
series of lectures on introductory topology.

- [An Antipodal Abstract Topoligist](https://www.youtube.com/channel/UClI8OrDeDbsSgbYuZoZYLjw):
The YouTube channel of an aussie Ph.D student
studying algebraic topology and homotopy.


## Combinatorial Game Theory
- [HACKENBUSH: a window to a new world of math](https://www.youtube.com/watch?v=ZYj4NkeGPdM)
An excellent video that walks you through
the ideas that give rise to Combinatorial Game Theory,
and later on to the surreal number system
discovered by John Conway.
You can use some of the knowledge here to cheat in gambling ;)

- [Surreal Numbers (by Donald Knuth)](https://www.informit.com/store/surreal-numbers-9780201038125)
A different take on introducing the surreal numbers,
this novella features a couple
who discover a clay tablet with strange markings
while on an extended retreat...


## Fun Things
- [Sheaves in Minecraft](https://quoteme.github.io/posts/sheaves_in_minecraft)
A blog post explaining categorical sheaves
applied to minecraft chunk loading
and nearby entity checking.
