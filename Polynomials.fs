module Polynomials

open AbstractAlgebra
open CacheHelpers
open System

let private PadArrayWith array paddingLength paddingElement = 
  if paddingLength = 0 then array 
  else Array.concat [array; (Array.create paddingLength paddingElement)]

/// Shortcut to make A*x^n
let Monomial<'T> (ring: 'T Ring) degree (coefficient: 'T) = 
  [| yield! (Array.create degree ring.Zero); coefficient |]

/// This would probably be reused a lot of times...
let private PolyComparison<'T> (ring : 'T Ring) (polyA :'T[]) (polyB : 'T[]) = 
  (polyA.Length = polyB.Length) && Seq.forall2 ring.AreEqual polyA polyB

/// Truncates the polynomial, removing trailing zeros
let private CompactPoly<'T> (ring: 'T Ring) (poly: 'T[]) = 
  let lastNonZero = array.FindLastIndex(poly, System.Predicate(ring.IsNotZero))
  Array.sub poly.[..lastNonZero] 0 (lastNonZero + 1)

/// returns the integer degree of the input polynomial
let PolyDegree<'T> (ring: 'T Ring) (poly: 'T[]) = 
  if poly.Length < 1 then None else Some(array.FindLastIndex(poly, Predicate(ring.IsNotZero)))
  
let PolyDegreeBigInt<'T> (ring: 'T Ring) (poly: 'T[]) = 
  (PolyDegree ring poly) |> Option.map bigint

/// returns the leading coefficient of the input polynomial
let PolyLc<'T> (ring: 'T Ring) (poly: 'T[]) = 
  let lastNonZero = array.FindLastIndex(poly, Predicate(ring.IsNotZero))
  poly.[lastNonZero]

/// adds two polynomials and returns the result
let private PolyAdd<'T> (ring: 'T Ring) (polyA: 'T[]) (polyB: 'T[]) = 
  let zero = ring.Zero
  let l1 = if polyA.Length > polyB.Length then polyA else polyB
  let l2small = if polyA.Length > polyB.Length then polyB else polyA
  let lengthDiff = l1.Length - l2small.Length
  let l2 = PadArrayWith l2small lengthDiff zero                                                        
  let resultArray = Array.map2 ring.Add l1 l2
  CompactPoly ring resultArray

/// negates the input polynomial
let private PolyNegate<'T> (ring: 'T Ring) (poly: 'T[]) = 
  Array.map ring.Negate poly

/// subtracts polyB from polyA
let private PolySubtract<'T> (ring: 'T Ring) (polyA: 'T[]) (polyB: 'T[]) = 
  PolyAdd ring polyA (PolyNegate ring polyB)

/// multiplies polyA by polyB
let private PolyMultiply<'T> (ring: 'T Ring) (polyA: 'T[]) (polyB: 'T[]) =   
  let mul i j = ring.Multiply polyA.[i] polyB.[j] // to save horizontal screen space
  let resultingDegree = polyA.Length + polyB.Length
  if resultingDegree = 0 then [||] else
    let result = Array.create (resultingDegree-1) ring.Zero 
    for i in 0..polyA.Length-1 do 
      for j in 0..polyB.Length-1 do
      Array.set result (i+j) (ring.Add result.[i+j] (mul i j))
  // This is needed for polys over non-domains, where the product of the coefficients
  // may become zero without either of the coefficients being such.
  // For example, (2x)(3x+1) is just 2x in Z6[x].
    CompactPoly ring result

/// Performs the polynomial division. Requires the coefficients to be from a field.
let private PolyDivRem<'T> (coefField: 'T Field) (polyA: 'T[]) (polyB: 'T[]) = 
  let deg = PolyDegree coefField
  let vdeg poly = match (deg poly) with | None -> -1 | Some(i) -> i
  let lc = PolyLc coefField
  if not(Array.Exists(polyB, Predicate coefField.IsNotZero)) then None 
  else Some(  
            let bLcInverse = (coefField.Invert (lc polyB)).Value
            let bDegree = (vdeg polyB) 
            let mutable div = [||]
            let mutable rem = polyA  
            while (vdeg rem) >= (vdeg polyB) do                
                let nextMultiply = (Monomial (coefField) ((vdeg rem) - bDegree) (coefField.Multiply (lc rem) bLcInverse))
                rem <- PolySubtract coefField rem (PolyMultiply coefField nextMultiply polyB)
                div <- (PolyAdd coefField div nextMultiply)
            (div, rem)
           )

/// Calculates unit and normal parts for a polynomial with coefficients from a field
/// Unit part is the leading coefficient [| LC |], and the normal part is the monic polynomial poly * [| 1/LC |]
let PolyUnitAndNormalParts (coefficientField: 'T Field) (poly: 'T[]) = 
   if Array.Exists(poly, Predicate coefficientField.IsNotZero) 
   then ([| PolyLc coefficientField poly |], 
         ( PolyMultiply coefficientField poly 
                        [| (coefficientField.Divide coefficientField.One (PolyLc coefficientField poly)).Value |] ))           
   else ([| coefficientField.One |], [||])
   
type PolynomialRing<'TCoefficient> = 
  inherit Ring<'TCoefficient[]>
  abstract member CoefficientRing : Ring<'TCoefficient> 

type PolynomialEuclideanDomain<'TCoefficient> = 
  inherit PolynomialRing<'TCoefficient>   
  inherit EuclideanDomain<'TCoefficient[]>
  abstract member CoefficientRing : Field<'TCoefficient>

type private PolyRingOverRing<'TCoefficient>(coefficientRing: 'TCoefficient Ring) = 
  inherit Construct.Ring<'TCoefficient[]>(
    Construct.CommutativeGroup(Array.empty, CommutativeBinaryOp(PolyAdd coefficientRing), 
                               UnaryOp(PolyNegate coefficientRing), EqualityChecker(PolyComparison coefficientRing)),
    Construct.Monoid(Array.empty, BinaryOp(PolyMultiply coefficientRing), 
                     EqualityChecker(PolyComparison coefficientRing)),
    Some(fun n -> [| coefficientRing.IntegerConstant n |]))
  member p.CoefficientRing = coefficientRing
  interface PolynomialRing<'TCoefficient> with 
    member p.CoefficientRing = p.CoefficientRing
type private PolyDomainOverField<'TCoefficient>(coefficientField: 'TCoefficient Field) = 
  inherit Construct.EuclideanDomain<'TCoefficient[]>(
      Construct.CommutativeGroup(Array.empty, CommutativeBinaryOp(PolyAdd coefficientField), 
                                 UnaryOp(PolyNegate coefficientField), EqualityChecker(PolyComparison coefficientField)),
      Construct.CommutativeMonoid(Array.empty, CommutativeBinaryOp(PolyMultiply coefficientField), 
                       EqualityChecker(PolyComparison coefficientField)),
      Some(fun n -> [| coefficientField.IntegerConstant n |]),
      PolyUnitAndNormalParts coefficientField,
      PolyDivRem coefficientField,
      PolyDegreeBigInt coefficientField)
  interface PolynomialEuclideanDomain<'TCoefficient> with
    member _.CoefficientRing = coefficientField :> 'TCoefficient Ring
    member _.CoefficientRing = coefficientField 

type Construct = class  
  static member PolyEqualityChecker coefficientRing = 
    (Сache (fun coefRing -> EqualityChecker(PolyComparison coefRing))) coefficientRing    

  static member PolyAdditiveGroup (coefficientRing: 'TCoefficient Ring) =
    (Сache (fun coefRing -> Construct.CommutativeGroup(Array.empty, CommutativeBinaryOp(PolyAdd coefRing), 
                                           UnaryOp(PolyNegate coefRing), EqualityChecker(PolyComparison coefRing)))) coefficientRing
    
  static member PolyMultiplicativeMonoid (coefficientRing: 'TCoefficient Ring) =
    (Сache (fun coefRing -> Construct.Monoid(Array.empty, BinaryOp(PolyMultiply coefRing), 
                                           EqualityChecker(PolyComparison coefRing)))) coefficientRing

  static member PolyMultiplicativeMonoid (coefficientRing: 'TCoefficient CommutativeRing) = 
    (Сache (fun coefRing -> Construct.CommutativeMonoid(Array.empty, CommutativeBinaryOp(PolyMultiply coefRing), 
                                           EqualityChecker(PolyComparison coefRing)))) coefficientRing

  static member PolyIntConstantMaker (coefficientRing: 'TCoefficient Ring) = 
    (Сache (fun (coefRing: 'TCoefficient Ring) -> Some(fun n -> [| coefRing.IntegerConstant n |]) )) coefficientRing
         
  static member UnivariatePolynomialRing(coefficientRing: 'TCoefficient Ring) = 
    (Сache (fun (coefRing : 'TCoefficient Ring) -> PolyRingOverRing(coefficientRing) :> PolynomialRing<'TCoefficient>)) coefficientRing

  static member UnivariatePolynomialRing(coefficientField: 'TCoefficient Field) = 
    (Сache (fun (coefField : 'TCoefficient Field) -> PolyDomainOverField(coefficientField) :> PolynomialEuclideanDomain<'TCoefficient>)) coefficientField 
end
  