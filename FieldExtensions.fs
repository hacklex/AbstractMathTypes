module FieldExtensions
open AbstractAlgebra
open Polynomials


/// Multiplicative monoid that represents an algebraic extension of a field, 
/// given the minimal polynomial of the element to be adjoined 
let private AlgebraicElementPolyMonoid(coefficientField: 'TCoefficient Field, minimalPoly: 'TCoefficient[])=
  Construct.CommutativeMonoid<'TCoefficient[]>([| coefficientField.One |], 
    CommutativeBinaryOp(fun polyA polyB ->
      let simpleDomain = Construct.UnivariatePolynomialRing(coefficientField, "x")      
      (simpleDomain.Rem (simpleDomain.Multiply polyA polyB) minimalPoly).Value), 
    Construct.PolyEqualityChecker coefficientField) 

/// Algebraic extension of a field K by element t, constructed by its minimal polynomial p(x)
/// This is the most general construction for algebraic numbers, which includes any radicals
/// as well as elements that cannot be expressed by radicals.
/// Basically, it's a quotient ring of K[x]/(p(x)), that is a field if p(x) is the minimal, 
/// irreducible polynomial of an element that is algebraic over K
type 'TCoefficient AlgebraicExtensionField(coefficientField : 'TCoefficient Field, minimalPoly : 'TCoefficient[], variableName: string) = 
  inherit Construct.Field<'TCoefficient[]>(Construct.PolyAdditiveGroup(coefficientField),
                                 AlgebraicElementPolyMonoid(coefficientField, minimalPoly),
                                 Construct.PolyIntConstantMaker coefficientField,
                                 PolyUnitAndNormalParts coefficientField,                                 
                                 (fun polyA polyB ->                                     
                                    let regularPolyDomain = Construct.UnivariatePolynomialRing(coefficientField, variableName)
                                    if regularPolyDomain.IsZero(polyB) then None else
                                    let (pB, _) = regularPolyDomain.Eea polyB minimalPoly
                                    let algMonoid = AlgebraicElementPolyMonoid(coefficientField, minimalPoly)
                                    Some(algMonoid.Op polyA pB)), 
                                 PolyGetString coefficientField variableName)
  member _.MinimalPoly = minimalPoly
  member _.CoefficientField = coefficientField

