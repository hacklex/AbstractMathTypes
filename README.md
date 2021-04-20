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
* AlgebraicExtensionField(field, minimalPoly)
* DifferentialPolyRing(polyRing, derivation)
* DifferentialField(field, derivation) 
* DifferentialQuotientField(baseDomain, derivationOnBaseDomain)
* DifferentialAlgebraicExtensionField(algebraicExtensionField, derivationOnPolysOverBaseField)

# Latest update: Refactoring and reorganizing (20-Apr-2021)

I refactored all the types away to the following modules:
* AbstractAlgebra -- semigroups, rings
* Fractions -- quotient field construction
* Polynomials -- polynomial rings (with arbitrary coefficients) and domains (with coefficients from a field)
* FieldExtensions -- currently, only algebraic extensions are supported
* DifferentialAlgebra -- derivatives for polynomials, quotients, and arbitrary algebraics

Check out Program.fs for examples of how the system works.

# Differential Fields

So far, I've only started support for differential fields. There are lots of difficulties
in defining such stuff formally, so this would probably be a long run.

What I currently plan implementing is, 
* a mechanism for building larger differential fields from smaller ones by adjoining new elements -- partially done, only transcendental extensions are not done yet.
* at least the most basic integration algorithm that will handle purely transcendental extensions -- this will probably take a long while...

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
