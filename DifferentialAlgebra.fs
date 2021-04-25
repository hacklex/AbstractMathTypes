module DifferentialAlgebra

open AbstractAlgebra
open Fractions
open Polynomials
open FieldExtensions

type DifferentialPolyRing<'T>(polyRing : 'T PolynomialRing, derivation: 'T[] -> 'T[]) = 
  member _.PolynomialRing = polyRing
  member _.Derive poly = derivation poly

type DifferentialField<'T>(field: 'T Field, derivation: 'T -> 'T) =
  member _.Field = field
  member _.Derive element = derivation element

let PolyDerive<'T> (coefficientField: 'T DifferentialField) (variableName) (elementDerivative: 'T[]) (poly: 'T[]) =   
  let polyRing = Construct.UnivariatePolynomialRing(coefficientField.Field, variableName)
  let mutable result = polyRing.Zero
  let monomial degree coefficient = Monomial(coefficientField.Field) degree coefficient
  let intConst n = coefficientField.Field.IntegerConstant n
  if poly.Length < 1 then result else
  if poly.Length < 2 then [| coefficientField.Derive poly.[0] |] else
  for i in 1..(poly.Length-1) do 
    let c = poly.[i] 
    let dC = coefficientField.Derive poly.[i]
    let dDeg = intConst i
    let dCtPlusCdt = polyRing.Add (monomial i dC)
                                  (polyRing.Multiply (monomial (i-1) (coefficientField.Field.Multiply c dDeg)) elementDerivative)
    result <- polyRing.Add result dCtPlusCdt   
  result  

type DifferentialQuotientField<'T>(polyDomain: EuclideanDomain<'T[]>, derivation: 'T[] -> 'T[]) = 
  inherit DifferentialField<Fraction<'T[]>>(QuotientField(polyDomain), (fun frac -> 
    match frac with 
    | N poly -> F(derivation poly, polyDomain.One)
    | F (num, den) -> let (dN, dD) = (derivation num, derivation den)
                      let ansNum = polyDomain.Subtract (polyDomain.Multiply dN den) (polyDomain.Multiply num dD)
                      let ansDen = polyDomain.Multiply den den
                      Construct.Fraction polyDomain ansNum ansDen
    | Zero -> N polyDomain.Zero
    | One -> N polyDomain.One                      
  ))

type DifferentialAlgebraicExtensionField<'T>(field: 'T AlgebraicExtensionField, polyDerivation: 'T -> 'T) = 
  inherit DifferentialField<'T[]>(field, (fun poly ->
     let minimalPoly = field.MinimalPoly
     let mutable result = field.Zero
     let mutable mNum = field.Zero
     let mutable mDen = field.Zero      
     let oneCoef = field.One.[0]
     let xPlusOne = [| oneCoef; oneCoef |]
     let justX = field.Subtract xPlusOne field.One
     let zeroCoef = justX.[0]

     let monomial deg coef = 
       let arr = Array.create (deg+1) zeroCoef 
       Array.set arr deg coef
       arr

     for i in 0..minimalPoly.Length-1 do       
        mNum <- field.Subtract mNum (monomial i (polyDerivation minimalPoly.[i]))
        if i>0 then 
            let polyCoef =  minimalPoly.[i] 
            let n = (field.IntegerConstant i)
            let nextMonomial = (monomial (i-1) polyCoef)            
            mDen <- field.Add mDen (field.Multiply nextMonomial n)
     let minPolyDerivative = (field.Divide mNum mDen).Value
     for i in 0..poly.Length-1 do
       let uPrimeV = monomial i (polyDerivation poly.[i])      
       let uVPrime = if i<1 then field.Zero else 
                     field.Multiply [| poly.[i] |] (field.Multiply minPolyDerivative
                       (monomial (i-1) (field.IntegerConstant i).[0]))
       result <- field.Add result (field.Add uPrimeV uVPrime)   
     result    
  ))

