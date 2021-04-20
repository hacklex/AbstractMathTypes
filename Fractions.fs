module Fractions
  
open AbstractAlgebra

type Fraction<'T> = 
  | F of ('T*'T)
  | N of 'T
  | Zero 
  | One 

/// Extracts the numerator of a fraction. Think (fst f). Or car, if you're rusty enough ;)
let Numerator (ring: 'T Ring) (fraction: 'T Fraction) = 
  match fraction with | F (a, _) -> a | N n -> n | Zero -> ring.Zero | One -> ring.One
/// Extracts the denominator of a fraction. Think (snd f). Or cdr, if you know what I mean ;)
let Denominator (ring: 'T Ring) (fraction: 'T Fraction) = 
  match fraction with | F (_, b) -> b | _ -> ring.One
/// Downgrades the fraction to a tuple (numerator, denominator), taking redundant values from given ring
let FullFraction (ring: 'T Ring) (x: 'T Fraction) = 
    match x with
    | F a -> a
    | N n -> (n, ring.One)
    | Zero -> (ring.Zero, ring.One)
    | One -> (ring.One, ring.One)

/// Normalizes the fraction, extracting the unit part to the numerator
let private QuotientNormalize (domain: 'T IntegralDomain) (fraction : 'T Fraction) = 
  let (num, den) = FullFraction domain fraction
  let finalUnitPart = domain.Multiply (domain.UnitPart num) (domain.UnitInverse (domain.UnitPart den))  
  F((domain.Multiply finalUnitPart (domain.NormalPart num)), (domain.NormalPart den))

/// gets unit and normal parts of the fraction
let private QuotientUnitAndNormalParts (domain: 'T IntegralDomain) (fraction: 'T Fraction) : ('T Fraction * 'T Fraction) = 
  let (normNum, normDen) = (FullFraction domain (QuotientNormalize domain fraction))
  F((domain.UnitPart normNum), domain.One), F((domain.NormalPart normNum), normDen) 
  
/// reduces the fraction if possible, dividing numerator and denominator by their gcd, when possible
let private QuotientCompact (domain: 'T IntegralDomain) (fraction: 'T Fraction) : 'T Fraction =
  let (a,b) = FullFraction domain fraction
  match domain with
  | :? EuclideanDomain<'T> -> let ed = domain :?> 'T EuclideanDomain
                              let gcd = ed.Gcd a b
                              if gcd.IsNone then F(domain.Zero, domain.One)
                              else let cd = gcd.Value
                                   QuotientNormalize domain (F((ed.Div a cd).Value, (ed.Div b cd).Value))
  | _ -> QuotientNormalize domain (F(a, b))  

type Construct = class
  static member Fraction domain num den = (QuotientCompact domain (F(num, den)))
end

/// adds two fractions
let private QuotientAdd (domain: 'T IntegralDomain) (x: 'T Fraction) (y: 'T Fraction) = 
  let (a,b) = FullFraction domain x
  let (c,d) = FullFraction domain y
  let num = domain.Add (domain.Multiply a d) (domain.Multiply b c)
  let den = domain.Multiply b d
  QuotientCompact domain (F(num, den))
/// negates a fraction
let private QuotientNegate (domain: 'T IntegralDomain) (f: 'T Fraction) =
  let (a,b) = FullFraction domain f
  F((domain.Negate a), b)
/// subtracts fraction b from fraction a
let private QuotientSubtract (domain: 'T IntegralDomain) (a: 'T Fraction) (b: 'T Fraction) =     
  QuotientAdd domain a (QuotientNegate domain b)
/// multiplies fraction p by fraction q
let private QuotientMultiply (domain: 'T IntegralDomain) (p: 'T Fraction) (q: 'T Fraction) =   
  let (a,b) = FullFraction domain p
  let (c,d) = FullFraction domain q
  let num = domain.Multiply a c
  let den = domain.Multiply b d
  QuotientCompact domain (F(num, den))   

/// returns true if fraction a equals fraction b, otherwise false
let private QuotientEqualityChecker (domain: 'T IntegralDomain) =   
  EqualityChecker(fun a b -> domain.IsZero (Numerator domain (QuotientSubtract domain a b)))
/// divides fraction p by fraction q
let private QuotientDivide (domain: 'T IntegralDomain) (p: 'T Fraction) (q: 'T Fraction) =     
  let (a,b) = FullFraction domain p
  let (c,d) = FullFraction domain q  
  if (domain.IsZero c) then None
  else if (domain.IsZero a) 
  then Some(F(domain.Zero, domain.One)) else        
    let (numUnit, numAbs) = (domain.UnitAndNormalParts (domain.Multiply a d))
    let (denUnit, denAbs) = (domain.UnitAndNormalParts (domain.Multiply b c))
    let unitPart = domain.Multiply numUnit (domain.UnitInverse denUnit)
    let num = domain.Multiply unitPart numAbs
    let den = denAbs
    Some(QuotientCompact domain (F(num, den)))
/// Field of fractions constructed from an Euclidean Domain 
type QuotientField<'T> (domain: 'T IntegralDomain) =
  inherit Construct.Field<'T Fraction>(
      Construct.CommutativeGroup(F(domain.Zero, domain.One), 
                       CommutativeBinaryOp(QuotientAdd domain),
                       UnaryOp(QuotientNegate domain),
                       QuotientEqualityChecker domain),
      Construct.CommutativeMonoid(F(domain.One, domain.One),
                        CommutativeBinaryOp(QuotientMultiply domain),
                        QuotientEqualityChecker domain),
      Some(fun n -> N (domain.IntegerConstant(n))),
      QuotientUnitAndNormalParts domain,      
      QuotientDivide domain,
      fun frac ->
        let (num, den) = FullFraction domain frac
        if (domain.IsZero num) then "0"
        else if (QuotientEqualityChecker domain).AreEqual frac (F(domain.One, domain.One))
          then "1" else 
            let numString = domain.GetString num 
            let denString = domain.GetString den 
            if (domain.IsOne den) then numString else 
                let finalNum = if ((numString.Contains "+") || (numString.Contains "-")) && ((not (numString.StartsWith "(")) || (not (numString.EndsWith ")")))
                               then "(" + numString + ")" else numString
                let finalDen = if ((denString.Contains "+") || (denString.Contains "-")) && ((not (denString.StartsWith "(")) || (not (denString.EndsWith ")")))
                               then "(" + denString + ")" else denString
                finalNum+"/"+finalDen      
      )
  member _.NumOverOne numerator = F(numerator, domain.One)
  member d.Fraction numerator denominator = QuotientCompact d (F(numerator, denominator))
   