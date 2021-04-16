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

# Last update: QuotientField

Yes, I have just implemented it. Now we can construct quotient fields from integral domains.
For example, as soon as we have constructed integers, we get rational numbers for free.
I also added support for simplifying the representations when it is possible, and it
is possible when the underlying domain is euclidean, by dividing numerator and denominator
by their GCD. Check out the QuotientField implementation.

By the way, as soon as we have the rationals, we instantly get Q[x], for free.
And since Q[x] is again an euclidean domain, we get Q(x), its quotient field, for free.

Check out program.fs for the demonstration.

Yes, the literals are currently quite ugly. But hey, you've got your polys algebra at no cost!

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
