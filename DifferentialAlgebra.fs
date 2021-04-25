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

type DifferentialTranscendentExtensionField<'T>(field: 'T DifferentialField, variableName, derivativeOfT : 'T[]) =   
  inherit DifferentialField<Fraction<'T[]>>(QuotientField(Construct.UnivariatePolynomialRing(field.Field, variableName)), fun  (frac : Fraction<'T[]>) ->    
    let polyRing = Construct.UnivariatePolynomialRing(field.Field, variableName)
    let (num, den) = FullFraction polyRing frac
    let polyDerive poly = PolyDerive field variableName derivativeOfT
    let numDer = (PolyDerive field variableName derivativeOfT num)
    let denDer = (PolyDerive field variableName derivativeOfT den)
    let sub = (polyRing.Subtract (polyRing.Multiply numDer den) (polyRing.Multiply num denDer))
    let totalDen = polyRing.Multiply den den
    Construct.Fraction polyRing sub totalDen
  )

type DifferentialAlgebraicExtensionField<'T>(field: 'T DifferentialField, minimalPoly : 'T[], variableName) as self = 
  inherit DifferentialField<'T[]>(AlgebraicExtensionField(field.Field, minimalPoly, variableName), (fun poly ->     
     let polyField = self.Field
     let coefField = field.Field
     let mutable result : 'T[] = [||]
     let mutable mNum = polyField.Zero
     let mutable mDen = polyField.Zero
     
     let coefficientDerive coef = field.Derive coef
     let monomial deg coef = Monomial coefField deg coef

     for i in 0..minimalPoly.Length-1 do       
        let polyCoef =  minimalPoly.[i] 
        mNum <- polyField.Subtract mNum (monomial i (coefficientDerive polyCoef))
        if i>0 then 
            let n = (polyField.IntegerConstant i)
            let nextMonomial = (monomial (i-1) polyCoef)            
            mDen <- polyField.Add mDen (polyField.Multiply nextMonomial n)
     let minPolyDerivative = (polyField.Divide mNum mDen).Value
     for i in 0..poly.Length-1 do
       let uPrimeV = monomial i (coefficientDerive poly.[i])      
       let uVPrime = if i<1 then polyField.Zero else 
                     polyField.Multiply [| poly.[i] |] (polyField.Multiply minPolyDerivative
                       (monomial (i-1) (polyField.IntegerConstant i).[0]))
       result <- polyField.Add result (polyField.Add uPrimeV uVPrime)   
     result    
  ))

