# AbstractMathTypes

This is my first F# project, in which I learn F# by implementing
the most basic data types to be used in a CAS.

I intend to gradually learn F# as I add more and more stuff here.

# Current state

Currently, I've implemented the following algebraic structures:

* AbstractAlgebra -- semigroups, rings, domains, fields
* Fractions -- quotient field construction
* Polynomials -- polynomial rings (with arbitrary coefficients) and domains (with coefficients from a field)
* FieldExtensions -- this currently only contains algebraics, because transcendentals are isomorphic to fields of fractions of polys
* DifferentialAlgebra -- derivatives for polynomials, quotients, and algebraic and transcendental extensions of differential fields

# Latest update: Transcendental Extensions (2021-04-25)

First of all, now we can construct $\mathbb{Q}(x)$ from $\mathbb{Q}$ as a Transcendental Extension of $\mathbb{Q}$ with zero derivation.
This carries much semantic weight, as $x$ may indeed be considered just a transcendental element over $\mathbb{Q}$.
This allows us to define the usual derivation in a trivial way, by saying $x'=1$ when building a Transcendental
Extension of $\mathbb{Q}$, which in turn we consider a Differential Field with zero derivation $D[a]=0 \forall a \in \mathbb{Q}$.

Of course, with this construction, derivation of rational functions of exponents, for example, comes for free
since we can just write 
```fs
let qDiffField = DifferentialField(rationals, fun _ -> rationals.Zero)
let ratFuncs = DifferentialTranscendentExtensionField(qDiffField, "x", [| rationals.One |])
let ratOfExps = DifferentialTranscendentalExtensionField(ratFuncs, "exp(x)", [| ratFuncs.Zero; ratFuncs.One |])
```
because $D[e^x]$ is indeed again $e^x$, and as a polynomial in $e^x$ with coefficients from $\mathbb{Q}(x)$, we
write it down as `[| ratFuncs.Zero; ratFuncs.One |]`, which is basically $0 + 1 e^x$

Check out Program.fs for examples of how the system works.

Don't hesitate to post an issue if you have anything to say.

Don't hesitate to make a pull request if you have something to add.

# What's the plan now?

* Matrices. This will be of utmost importance, as we will soon need stuff like resultants.
* Squarefree decomposition for polys. This sounds like somewhat trivial over fields, but I still consider it a step worth mentioning
* Basics needed for Risch integration. Think of Hermite reduction
* Eventually, rational function integration
* Eventually, hopefully, transcendental function integration
* ...I wonder if I do have what it takes to approach algebraics... Davenport's book is a difficult read.

# Implementation notes and issues

(*) CommutativeMonoid is supposed to be both Monoid and CommutativeSemigroup,
but due to F# not supporting multiple inheritance, I had to choose one, and chose to inherit Monoid. 
The constructor, however, does take a commutative operation as a parameter.

(**) IntegralDomain is supposed to be both Domain and CommutativeRing, but again,
due to F# not supporting multiple inheritance, I chose to inherit Domain.

This is not an optimal solution, as IntegralDomain technically fails to be a CommutativeRing,
which theoretically it always shall be. To overcome this, IntegralDomain is provided with a 
member AsCommutativeRing, which provides the CommutativeRing compatibility.

I am open to suggestions as to how this should be fixed to develop these types in best possible generality.

# F* on the horizon

I am currently considering rewriting this in F*. The problem is, the language is vastly different from F#,
and I'm currently only starting to grasp its basics. Can't even implement inheritance so far. 

*If anyone can and is willing help me with that, feel free to contact me... leave an issue here, or something :)*

F* would theoretically offer formal verification for quite a lot of things, such that we won't have to double-check
stuff like operation commutativity or associativity when defining basic abstract algebra types, or the axioms
for derivation ($D[a+b]=D[a]+D[b], D[ab]=D[a]b+aD[b]$, © Leibnitz and Newton, 17th century AD). Or even more 
interesting things, I'm not sure yet.

# Acknowledgments

I hereby express my heartfelt gratitude to akater, Ayrat Hudaygulov, Vladislav Khapin, and Friedrich fon Never, 
for their comments and helpful tips, and patience when I didn't understand them fast enough. You guys are the best.
