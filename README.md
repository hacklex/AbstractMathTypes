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
* Domain(additiveGroup,  multiplicativeMonoid)
* IntegralDomain**(additiveGroup, multipliativeCommutativeMonoid)
* UniqueFactorizationDomain(integralDomain, unitPart(x), normalPart(x), divRem(x, y))
* EuclideanDomain(uniqueFactorizationDomain, valuation)
* Field(euclideanDomain, division)

## Notes 

(*) CommutativeMonoid is supposed to be both Monoid and CommutativeSemigroup,
but due to F# not supporting multiple inheritance, I had to choose one, and chose to inherit Monoid. 
The constructor, however, does take a commutative operation as a parameter.

(**) IntegralDomain is supposed to be both Domain and CommutativeRing, but again,
due to F# not supporting multiple inheritance, I chose to inherit Domain.

This is not an optimal solution, as IntegralDomain technically fails to be a CommutativeRing,
which theoretically it always shall be. To overcome this, IntegralDomain is provided with a 
member AsCommutativeRing, which provides the CommutativeRing compatibility.

I am open to suggestions as to how this should be fixed to develop these types in best possible generality.

# Testing

To test these types, I added a (rather simple and unoptimized) implementation of rational number field Q,
and wrote a test for polynomial division in Q[x]. Just check out program.fs to see how this works.

As a nice bonus, I added a simple literal support to rational numbers, so that one could write 
the constant polynomials in a convenient manner. For example, [| 5Q/3Q 1Q 2Q |] means 2x²+x+5/3.

# Acknowledgments

I hereby express my heartfelt gratitude to akater, Ayrat Hudaygulov, Vladislav Khapin, and Friedrich fon Never, 
for their comments and helpful tips, and patience when I didn't understand them fast enough. You guys are the best.
