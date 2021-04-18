# AbstractMathTypes

This is my first F# project, in which I learn F# by implementing
the most basic data types to be used in a CAS.

I intend to gradually learn F# as I add more and more stuff here.

# Implemented algebraic structures

Currently, I've implemented the following algebraic structures:

* Semigroup(operation, comparison)
* Monoid(neutralElement, operation, comparison)
* CommutativeSemigroup(operation, comparison)
* CommutativeMonoid*(neutralElement, operation, comparison)
* Group(neutralElement, operation, inversion, comparison)
* CommutativeGroup(neutralElement, operation, inversion, comparison)
* Rng(additiveGroup, multiplicativeSemigroup)
* Ring(additiveGroup, multiplicativeMonoid)
* CommutativeRing(additiveGroup, multipliativeCommutativeMonoid)
* Domain(additiveGroup, multiplicativeMonoid)
* IntegralDomain**(additiveGroup, multipliativeCommutativeMonoid, unit(x), normal(x), unitInverse(u))
* UniqueFactorizationDomain(integralDomain, divRem(x, y))
* EuclideanDomain(uniqueFactorizationDomain, valuation)
* Field(euclideanDomain, division)
* QuotientField(integralDomain)
* DifferentialField(field, derivation)
* AlgebraicExtensionPolyField(field, minimalPoly)

# Latest update: Extended Euclidean Algorithm and AlgebraicExtensionPolyField 

Well, one leads to another. We basically construct the quotient ring for the minimal polynomial
of an algebraic number, and since it's a good, irreducible, well-behaved minimal polynomial with a 
perfect record and no crimes committed, the quotient ring is actually a field.

So the most important thing is that we can divide here. Indeed, let p(t) be the minimal polynomial,
then for any f(t), Extended Euclidean Algorithm gives us a(t) and b(t) such that p(t)a(t)+f(t)b(t)=1.
Then it is obvious that b(t) is the multiplicative inverse of f(t), because in our field, p(t)=0 (mod p(t)).

Program.fs currently contains two usage examples.

First one proves that if t=sqrt(-2), then

1/(1+t) = (1/3) + (-1/3)*t

Second one proves that if t=sqrt(1+x), then

1/(1+t) = (-1/x) + (1/x)*t

Note how the algebraic extension field type is implemented in full generality, as it does not care
whether the adjoining element is a simple radical, or just a root of a polynomial, that may not necessarily
be expressed by radicals.

# Differential Fields

So far, I've only started support for differential fields. There are lots of difficulties
in defining such stuff formally, so this would probably be a long run.

What I currently plan implementing is, 
* a mechanism for building larger differential fields from smaller ones by adjoining new elements
* at least the most basic integration algorithm that will handle purely transcendental extensions.

Don't hesitate to post an issue if you have anything to say.

Don't hesitate to make a pull request if you have something to add.

## Implementation notes and issues

(*) CommutativeMonoid is supposed to be both Monoid and CommutativeSemigroup,
but due to F# not supporting multiple inheritance, I had to choose one, and chose to inherit Monoid. 
The constructor, however, does take a commutative operation as a parameter.

(**) IntegralDomain is supposed to be both Domain and CommutativeRing, but again,
due to F# not supporting multiple inheritance, I chose to inherit Domain.

This is not an optimal solution, as IntegralDomain technically fails to be a CommutativeRing,
which theoretically it always shall be. To overcome this, IntegralDomain is provided with a 
member AsCommutativeRing, which provides the CommutativeRing compatibility.

I am open to suggestions as to how this should be fixed to develop these types in best possible generality.

# Acknowledgments

I hereby express my heartfelt gratitude to akater, Ayrat Hudaygulov, Vladislav Khapin, and Friedrich fon Never, 
for their comments and helpful tips, and patience when I didn't understand them fast enough. You guys are the best.
