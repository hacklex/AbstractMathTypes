/// Most basic implementation for abstract algebra types.
/// Refer to K.O. Geddes, S.R. Czapor, G. Labahn -- Algorithms for computer algebra
/// when unsure about naming conventions, terms etc.
module MathTypes

open System.Numerics
open System
open System.Text

/// Generic binary operation with no conditions forced upon it
type 'T BinaryOp(op : 'T -> 'T -> 'T) = member _.Op = op
/// Generic unary operation with no conditions forced upon it
type 'T UnaryOp(op: 'T -> 'T) = member _.Op = op
/// Commutative binary operation, i.e. (op a b) is equivalent to (op b a)
type 'T CommutativeBinaryOp(op) = inherit ('T BinaryOp)(op)

/// Equivalence check operation, that is, reflexive symmetric transitive function t -> t -> bool
type 'T EquivalenceCheck = 'T -> 'T -> bool

let inline private SimpleAdd a b = a + b
let inline private SimpleMultiply a b = a * b
let inline private SimpleEquals a b = a = b

let private PadArrayWith array paddingLength paddingElement = 
  if paddingLength = 0 then array 
  else Array.concat [array; (Array.create paddingLength paddingElement)]

/// <summary>
/// Semigroup from elements of type <typeparamref name="'T">'T</typeparamref> 
/// with group operation <paramref name="op"/> and equivalence check <paramref name="compare"/>
/// </summary>
type 'T Semigroup(op: 'T BinaryOp,
                  compare : 'T EquivalenceCheck) = 
  member _.Op x y = op.Op x y
  member _.Compare x y = compare x y
  
/// <summary>
/// Monoid from elements of type <typeparamref name="'T">'T</typeparamref> with
/// neutral element <paramref name="neutralElement"/>, group operation <paramref name="op"/>,
/// and equivalence check <paramref name="compare"/> 
/// </summary>
type 'T Monoid(neutralElement: 'T, 
               op: 'T BinaryOp, 
               compare: 'T EquivalenceCheck) = 
  inherit ('T Semigroup)(op, compare)
  member _.NeutralElement = neutralElement
  
/// <summary>
/// Commutative semigroup from elements of type <typeparamref name="'T">'T</typeparamref> 
/// with commutative group operation <paramref name="op"/>,
/// and equivalence check <paramref name="compare"/>
/// </summary>
type 'T CommutativeSemigroup(op: 'T CommutativeBinaryOp,
                             compare: 'T EquivalenceCheck) = 
  inherit ('T Semigroup)(op, compare)
  
/// <summary>
/// Commutative monoid from elements of type <typeparamref name="'T">'T</typeparamref>
/// with neutral element <paramref name="neutralElement"/>,
/// commutative group operation <paramref name="op"/>,
/// and equivalence check <paramref name="compare"/>
/// </summary>
/// <remarks> This should in fact inherit both Monoid and CommutativeSemigroup.
/// Have yet to find a way of doing that in full generality. </remarks>
type 'T CommutativeMonoid(neutralElement: 'T, 
                          op: 'T CommutativeBinaryOp,
                          compare : 'T EquivalenceCheck) = 
  inherit ('T Monoid)(neutralElement, op, compare)

/// <summary>
/// Group from elements of type <typeparamref name="'T">'T</typeparamref> with neutral
/// element <paramref name="neutralElement"/>, group operation <paramref name="op"/>, 
/// inversion <paramref name="invert"/>, and equivalence check <paramref name="compare"/>
/// </summary>
type 'T Group(neutralElement: 'T,
              op: 'T BinaryOp, 
              invert: 'T UnaryOp, 
              compare : 'T EquivalenceCheck) =
  inherit ('T Monoid)(neutralElement, op, compare)  
  member _.Invert x = invert.Op x

/// <summary>
/// Commutative, or Abelian, group from elements of type <typeparamref name="'T">'T</typeparamref> 
/// with neutral element <paramref name="neutralElement"/>, commutative group 
/// operation <paramref name="op"/>, inversion <paramref name="invert"/>, 
/// and equivalence check <paramref name="compare"/>
/// </summary>
type 'T CommutativeGroup(neutralElement: 'T, 
                         op: 'T CommutativeBinaryOp,
                         invert: 'T UnaryOp,
                         compare : 'T EquivalenceCheck) =
  inherit ('T Group)(neutralElement, op, invert, compare)
  
/// <summary>
/// Nonunital ring (or Rng or pseudo-ring) from elements 
/// of type <typeparamref name="'T">'T</typeparamref> 
/// constructed from an additive commutative group <paramref name="additiveGroup"/> 
/// and a multiplicative semigroup <paramref name="multiplicativeSemigroup"/>.
/// </summary>
type 'T Rng(additiveGroup: 'T CommutativeGroup,
            multiplicativeSemigroup: 'T Semigroup) =  
  member _.Zero = additiveGroup.NeutralElement
  member _.IsZero = Predicate(fun x -> (additiveGroup.Compare x additiveGroup.NeutralElement))
  member _.IsNotZero = Predicate(fun x -> not (additiveGroup.Compare x additiveGroup.NeutralElement))
  member _.Compare x y = additiveGroup.Compare x y
  member _.Add x y = additiveGroup.Op x y
  member _.Subtract x y = additiveGroup.Op x (additiveGroup.Invert y)
  member _.Negate x = additiveGroup.Invert x
  member _.Multiply x y = multiplicativeSemigroup.Op x y
  
/// <summary>
/// Ring from elements of type <typeparamref name="'T">'T</typeparamref> 
/// constructed from an additive commutative group <paramref name="additiveGroup"/> 
/// and a multiplicative monoid <paramref name="multiplicativeMonoid"/>.
/// </summary>
type 'T Ring(additiveGroup: 'T CommutativeGroup,
             multiplicativeMonoid: 'T Monoid) = 
  inherit ('T Rng)(additiveGroup, multiplicativeMonoid)
  member _.One = multiplicativeMonoid.NeutralElement

/// <summary>
/// Commutative ring from elements of type <typeparamref name="'T">'T</typeparamref> 
/// constructed from an additive commutative group <paramref name="additiveGroup"/> 
/// and a multiplicative commutative monoid <paramref name="multiplicativeMonoid"/>.
/// </summary>
type 'T CommutativeRing(additiveGroup: 'T CommutativeGroup,
                        multiplicativeCommutativeMonoid: 'T CommutativeMonoid) = 
  inherit ('T Ring)(additiveGroup, multiplicativeCommutativeMonoid)  

/// <summary>
/// Domain from elements of type <typeparamref name="'T">'T</typeparamref> 
/// constructed from an additive commutative group <paramref name="additiveGroup"/> 
/// and a multiplicative monoid <paramref name="multiplicativeMonoid"/>, that
/// additionally satisfies the zero-product property, namely, if (ab==0) then either (a==0) or (b==0)
/// </summary>
type 'T Domain(additiveGroup: 'T CommutativeGroup,
               multiplicativeMonoid: 'T Monoid) = 
  inherit ('T Ring)(additiveGroup, multiplicativeMonoid)

/// <summary>
/// Ingegral (or Commutative) Domain from elements of type <typeparamref name="'T">'T</typeparamref> 
/// constructed from an additive commutative group <paramref name="additiveGroup"/> , 
/// a multiplicative commutative monoid <paramref name="multiplicativeMonoid"/>, that
/// additionally satisfies the zero-product property, namely, if (ab==0) then either (a==0) or (b==0),
/// and an associated element distinction function <paramref name="unitAndNormalParts"/>
/// </summary>
/// <remarks>
/// This should inherit both Domain and CommutativeRing.
/// I'm yet to find a good way of doing that.
/// </remarks>
type 'T IntegralDomain(additiveGroup: 'T CommutativeGroup, 
                       multiplicativeCommutativeMonoid: 'T CommutativeMonoid, 
                       unitAndNormalParts : 'T -> ('T * 'T),
                       unitInverse: 'T -> 'T) =
  inherit ('T Domain)(additiveGroup, multiplicativeCommutativeMonoid)
  member d.AsCommutativeRing = CommutativeRing(additiveGroup, multiplicativeCommutativeMonoid)
  member _.UnitAndNormalParts x = unitAndNormalParts x
  member _.UnitPart x = fst (unitAndNormalParts x)
  member _.NormalPart x = snd (unitAndNormalParts x)
  member _.UnitInverse x = unitInverse x
  
/// <summary>
/// Unique Factorization Domain from elements of type <typeparamref name="'T">'T</typeparamref> 
/// constructed from an additive commutative group <paramref name="additiveGroup"/>, 
/// a multiplicative commutative monoid <paramref name="multiplicativeMonoid"/>, that
/// additionally satisfies the zero-product property, namely, if (ab==0) then either (a==0) or (b==0),
/// associated element distinction function <paramref name="unitAndNormalParts"/>, and a
/// DivRem implementation <paramref name="divRem"/>
/// </summary>
type 'T UniqueFactorizationDomain(additiveGroup: 'T CommutativeGroup, 
                                  multiplicativeCommutativeMonoid: 'T CommutativeMonoid, 
                                  unitAndNormalParts : 'T -> ('T * 'T), 
                                  divRem : 'T -> 'T -> ('T * 'T) option) = 
  inherit ('T IntegralDomain)(additiveGroup, 
                              multiplicativeCommutativeMonoid, 
                              unitAndNormalParts, 
                              fun unit -> fst ((divRem multiplicativeCommutativeMonoid.NeutralElement unit).Value))
  member _.DivRem x y = divRem x y
  member _.Div x y = match divRem x y with | Some (d, _) -> Some(d) | None -> None
  member _.Rem x y = match divRem x y with | Some (_, r) -> Some(r) | None -> None

/// <summary>
/// Unique Factorization Domain from elements of type <typeparamref name="'T">'T</typeparamref> 
/// constructed from an additive commutative group <paramref name="additiveGroup"/>, 
/// a multiplicative commutative monoid <paramref name="multiplicativeMonoid"/>, that
/// additionally satisfies the zero-product property, namely, if (ab==0) then either (a==0) or (b==0),
/// associated element distinction function <paramref name="unitAndNormalParts"/>, a
/// DivRem implementation <paramref name="divRem"/>, and a valuation function <paramref name="valuation"/> 
/// </summary>
type 'T EuclideanDomain(additiveGroup: 'T CommutativeGroup, 
                        multiplicativeCommutativeMonoid: 'T CommutativeMonoid, 
                        unitAndNormalParts : 'T -> ('T * 'T), 
                        divRem : 'T -> ('T -> (('T * 'T) option)), 
                        valuation: 'T -> (BigInteger option)) = 
  inherit ('T UniqueFactorizationDomain)(additiveGroup, multiplicativeCommutativeMonoid, 
                                         unitAndNormalParts, divRem)
  // for example, polynomial degree, number absolute value
  member _.Valuation x = valuation x
  member this.Gcd(a, b) =     
    if ((this.Compare this.Zero a) || (this.Compare this.Zero b)) then None 
    else   
      let mutable c = this.NormalPart a
      let mutable d = this.NormalPart b 
      while not (this.Compare this.Zero d) do
        let r = (this.Rem c d).Value // this is valid because Rem only returns None when d is zero
        c <- d
        d <- r
      Some (this.NormalPart c)
  // extended euclidean algorithm, (a,b) -> (s,t) such that as+bt = 1
  member this.Eea(a, b) = 
    let mutable c = this.NormalPart a
    let mutable d = this.NormalPart b 
    let mutable c1 = this.One
    let mutable d1 = this.Zero
    let mutable c2 = this.Zero
    let mutable d2 = this.One
    while this.IsNotZero.Invoke(d) do
      let q = (this.Div c d).Value  
      let r = this.Subtract c (this.Multiply q d)
      let r1 = this.Subtract c1 (this.Multiply q d1)
      let r2 = this.Subtract c2 (this.Multiply q d2)
      c <- d
      c1 <- d1
      c2 <- d2
      d <- r
      d1 <- r1
      d2 <- r2
    let g = this.NormalPart c
    let s = this.Multiply c1 (this.Multiply (this.UnitInverse (this.UnitPart a)) (this.UnitInverse (this.UnitPart c)))
    let t = this.Multiply c2 (this.Multiply (this.UnitInverse (this.UnitPart b)) (this.UnitInverse (this.UnitPart c)))
    (s,t)
    
/// Field. Pinnacle of ring evolution. Strongest of structures.
/// Ok just kidding. Differential fields coming, soon.
type 'T Field(additiveGroup : 'T CommutativeGroup,
              multiplicativeCommutativeMonoid : 'T CommutativeMonoid,
              unitAndNormalParts: 'T -> ('T * 'T),              
              valuation: 'T -> (bigint option),
              divide : 'T -> 'T -> ('T option)) = 
  inherit ('T EuclideanDomain)(additiveGroup,  
                               multiplicativeCommutativeMonoid,
                               unitAndNormalParts,
                               (fun a b -> if (additiveGroup.Compare b additiveGroup.NeutralElement) then None 
                                           else Some((divide a b).Value, additiveGroup.NeutralElement)),
                               valuation)
  member _.Divide a b = divide a b
  member _.Invert a = (divide multiplicativeCommutativeMonoid.NeutralElement a)
  member _.AdditiveGroup = additiveGroup
  member _.MultiplicativeCommutativeMonoid = multiplicativeCommutativeMonoid   

/// the Fraction type allows for less bulky literal construction
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
                              let gcd = ed.Gcd(a, b)
                              if gcd.IsNone then F(domain.Zero, domain.One)
                              else let cd = gcd.Value
                                   QuotientNormalize domain (F((ed.Div a cd).Value, (ed.Div b cd).Value))
  | _ -> QuotientNormalize domain (F(a, b))  

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
let private QuotientCompare (domain: 'T IntegralDomain) (a: 'T Fraction) (b: 'T Fraction) =    
  domain.IsZero.Invoke (Numerator domain (QuotientSubtract domain a b))
/// divides fraction p by fraction q
let private QuotientDivide (domain: 'T IntegralDomain) (p: 'T Fraction) (q: 'T Fraction) =     
  let (a,b) = FullFraction domain p
  let (c,d) = FullFraction domain q  
  if (domain.Compare c domain.Zero) then None
  else if (domain.Compare a domain.Zero) 
  then Some(F(domain.Zero, domain.One)) else        
    let (numUnit, numAbs) = (domain.UnitAndNormalParts (domain.Multiply a d))
    let (denUnit, denAbs) = (domain.UnitAndNormalParts (domain.Multiply b c))
    let unitPart = domain.Multiply numUnit (domain.UnitInverse denUnit)
    let num = domain.Multiply unitPart numAbs
    let den = denAbs
    Some(QuotientCompact domain (F(num, den)))
/// Field of fractions constructed from an Euclidean Domain 
type QuotientField<'T> (domain: 'T IntegralDomain) =
  inherit  Field<'T Fraction>(
      CommutativeGroup(F(domain.Zero, domain.One), 
                       CommutativeBinaryOp(QuotientAdd domain),
                       UnaryOp(QuotientNegate domain),
                       QuotientCompare domain),
      CommutativeMonoid(F(domain.One, domain.One),
                        CommutativeBinaryOp(QuotientMultiply domain),
                        QuotientCompare domain),
      QuotientUnitAndNormalParts domain,
      (fun p -> if (domain.IsZero.Invoke((Numerator domain p))) then None else Some(0I)),
      QuotientDivide domain
  )

/// Shortcut to make A*x^n
let Monomial<'T> (ring: 'T Ring) degree (coefficient: 'T) = 
  [| yield! (Array.create degree ring.Zero); coefficient |]

/// This would probably be reused a lot of times...
let private PolyComparison<'T> (ring : 'T Ring) (polyA :'T[]) (polyB : 'T[]) = 
  (polyA.Length = polyB.Length) && Seq.forall2 ring.Compare polyA polyB

/// Truncates the polynomial, removing trailing zeros
let private CompactPoly<'T> (ring: 'T Ring) (poly: 'T[]) = 
  let lastNonZero = array.FindLastIndex(poly, ring.IsNotZero)
  Array.sub poly.[..lastNonZero] 0 (lastNonZero + 1)

/// returns the integer degree of the input polynomial
let private PolyDegree<'T> (ring: 'T Ring) (poly: 'T[]) = 
  if poly.Length < 1 then None else Some(array.FindLastIndex(poly, ring.IsNotZero))
  
/// returns the leading coefficient of the input polynomial
let private PolyLc<'T> (ring: 'T Ring) (poly: 'T[]) = 
  let lastNonZero = array.FindLastIndex(poly, ring.IsNotZero)
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
  if not(Array.Exists(polyB, coefField.IsNotZero)) then None 
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
let private PolyUnitAndNormalParts<'T> (coefficientField: 'T Field) (poly: 'T[]) = 
   if Array.Exists(poly, coefficientField.IsNotZero) 
   then ([| PolyLc coefficientField poly |], 
         ( PolyMultiply coefficientField poly 
                        [| (coefficientField.Divide coefficientField.One (PolyLc coefficientField poly)).Value |] ))           
   else ([| coefficientField.One |], [||])

/// <summary>
/// Additive group of Univariate Polynomials with coefficients in <paramref name="coefficientRing"/>.
/// The coefficient ring structure dictates the ring operations on the polynomials.
/// Note that K[x] is only a ring in general, and only becomes a field if x is algebraic over K.
/// </summary>
type 'TCoefficient UnivariatePolynomialAdditiveGroup(coefficientRing: 'TCoefficient Ring) = 
  inherit CommutativeGroup<'TCoefficient[]>(
      //zero polynomial
      Array.empty, 
      //polynomial addition
      new ('TCoefficient[] CommutativeBinaryOp)(fun polyA polyB -> 
        let zero = coefficientRing.Zero
        let l1 = if polyA.Length > polyB.Length then polyA else polyB
        let l2small = if polyA.Length > polyB.Length then polyB else polyA
        let lengthDiff = l1.Length - l2small.Length
        let l2 = PadArrayWith l2small lengthDiff zero                                                        
        let resultArray = Array.map2 coefficientRing.Add l1 l2
        CompactPoly coefficientRing resultArray
      ), 
      //polynomial negation
      new UnaryOp<'TCoefficient[]>(fun poly -> Array.map coefficientRing.Negate poly), 
      PolyComparison coefficientRing)

/// <summary>
/// Multiplicative monoid of Univariate Polynomials with coefficients in <paramref name="coefficientRing"/>.
/// The coefficient ring structure dictates the ring operations on the polynomials.
/// Note that K[x] is merely a ring in general, and only becomes a field if x is algebraic over K.
/// </summary>
type 'TCoefficient UnivariatePolynomialMultiplicativeMonoid(coefficientRing: 'TCoefficient Ring) = 
  inherit Monoid<'TCoefficient[]>(
    // multiplicative identity polynomial, i.e. just one constant coefficient "1"
    [| coefficientRing.One |], 
    // Polynomial multiplication. By the time I was writing this one, I was so exhausted 
    // that I actually double-checked in a CAS that deg (p*q) = deg(p)+deg(q) -_-
    (new ('TCoefficient[] BinaryOp)(PolyMultiply coefficientRing)), 
    // glory to the curry!
    (PolyComparison coefficientRing))

/// <summary>
/// Commutative multiplicative monoid of Univariate Polynomials with coefficients in <paramref name="coefficientRing"/>.
/// The coefficient ring structure dictates the ring operations on the polynomials.
/// This is so sad that we have to duplicate the above type due to lack of multiple inheritance in F#.
/// Note that K[x] is merely a ring in general, and only becomes a field if x is algebraic over K.
/// </summary>
type 'TCoefficient UnivariatePolynomialMultiplicativeCommutativeMonoid(coefficientRing: 'TCoefficient CommutativeRing) = 
  inherit CommutativeMonoid<'TCoefficient[]>(
    // multiplicative identity polynomial, i.e. just one constant coefficient "1"
    [| coefficientRing.One |], 
    // Polynomial multiplication. By the time I was writing this one, I was so exhausted 
    // that I actually double-checked in a CAS that deg (p*q) = deg(p)+deg(q) -_-
    (new ('TCoefficient[] CommutativeBinaryOp)(PolyMultiply coefficientRing)), 
    // glory to the curry!
    PolyComparison coefficientRing)
    
/// Polynomial ring with arbitrary coefficient ring
type 'TCoefficient UnivariatePolynomialRing(coefficientRing : 'TCoefficient Ring) =
  inherit Ring<'TCoefficient[]>(
    (new ('TCoefficient UnivariatePolynomialAdditiveGroup)(coefficientRing)), 
    (new ('TCoefficient UnivariatePolynomialMultiplicativeMonoid)(coefficientRing)))

/// Polynomial ring with coefficients from a field, which allows poly division
type 'TCoefficient UnivariatePolyOverFieldDomain(coefficientField : 'TCoefficient Field) =
  inherit EuclideanDomain<'TCoefficient[]>(  
      (new ('TCoefficient UnivariatePolynomialAdditiveGroup)(coefficientField)), 
      // we have to do this due to the lack of multi-inheritance in F#
      (new ('TCoefficient UnivariatePolynomialMultiplicativeCommutativeMonoid)(coefficientField.AsCommutativeRing)),
      (fun (poly : 'TCoefficient[]) -> // Unit part is sign(lc(p(x))), Normal part is p(x)/unitPart(p(x))
         if Array.Exists(poly, coefficientField.IsNotZero) 
         then ([| PolyLc coefficientField poly |], 
               ( PolyMultiply coefficientField poly 
                              [| (coefficientField.Divide coefficientField.One (PolyLc coefficientField poly)).Value |] ))           
         else ([| coefficientField.One |], [||])),
      (fun (polyA : 'TCoefficient[]) (polyB : 'TCoefficient[]) -> (PolyDivRem coefficientField polyA polyB)),
      (fun (poly : 'TCoefficient[]) -> (PolyDegree coefficientField poly) |> Option.map BigInteger)      
  )  
/// Multiplicative monoid that represents an algebraic extension of a field, 
/// given the minimal polynomial of the element to be adjoined 
type 'TCoefficient AlgebraicElementPolyMultiplicativeMonoid(coefficientField: 'TCoefficient Field, minimalPoly: 'TCoefficient[])=
  inherit CommutativeMonoid<'TCoefficient[]>([| coefficientField.One |], 
    CommutativeBinaryOp(fun polyA polyB ->
      let simpleDomain = UnivariatePolyOverFieldDomain(coefficientField)
      (simpleDomain.Rem (simpleDomain.Multiply polyA polyB) minimalPoly).Value
    ), PolyComparison coefficientField) 

/// Algebraic extension of a field K by element t, constructed by its minimal polynomial p(x)
/// This is the most general construction for algebraic numbers, which includes any radicals
/// as well as elements that cannot be expressed by radicals.
/// Basically, it's a quotient ring of K[x]/(p(x)), that is a field if p(x) is the minimal, 
/// irreducible polynomial of an element that is algebraic over K
type 'TCoefficient AlgebraicExtensionPolyField(coefficientField : 'TCoefficient Field, minimalPoly : 'TCoefficient[]) = 
  inherit Field<'TCoefficient[]>(UnivariatePolynomialAdditiveGroup(coefficientField),
                                 AlgebraicElementPolyMultiplicativeMonoid(coefficientField, minimalPoly),
                                 PolyUnitAndNormalParts coefficientField,
                                 (fun poly -> if poly.Length<1 then None else Some(0I)),
                                 (fun polyA polyB ->                                     
                                    let regularPolyDomain = UnivariatePolyOverFieldDomain(coefficientField)
                                    if regularPolyDomain.IsZero.Invoke(polyB) then None else
                                    let (pB, _) = regularPolyDomain.Eea(polyB, minimalPoly)
                                    let algMonoid = AlgebraicElementPolyMultiplicativeMonoid(coefficientField, minimalPoly)
                                    Some(algMonoid.Op polyA pB)
                                 )
  
  )

/// A functional field with given derivation
type 'T DifferentialField(functionField: 'T Field, derive : 'T -> 'T) = 
  inherit ('T Field)(functionField.AdditiveGroup, functionField.MultiplicativeCommutativeMonoid,
                     functionField.UnitAndNormalParts, functionField.Valuation, functionField.Divide)
  member _.Derive = derive
  
/// Regular integers as an Euclidean Domain
type IntegerRing() = 
  inherit EuclideanDomain<bigint>((new CommutativeGroup<bigint>(0I, 
                                     new CommutativeBinaryOp<bigint>(SimpleAdd), 
                                     new UnaryOp<bigint>(bigint.Negate), 
                                     SimpleEquals)),                                  
                                  (new CommutativeMonoid<bigint>(1I, 
                                     new CommutativeBinaryOp<bigint>(SimpleMultiply), 
                                     SimpleEquals)),
                                  // unit part and normal part is the generalization of sign and absolute value 
                                  // for general domains. Certain rings have more units than just -1 and 1.
                                  (fun p -> if p < 0I then (-1I, -p) else (1I, p)),
                                  // DivRem is standard, except we consider it to be None when the divisor is 0      
                                  (fun p q -> if q = 0I then None else Some(p/q, p % q)),
                                  // valuation for integers is just the absolute value function
                                  (fun p -> if p = 0I then None else Some (bigint.Abs p)))

/// Returns a human-readable string representation for given polynomial
let GetPolyString<'T> (ring: 'T Ring) (coeffToString: 'T -> string) (poly: 'T[]) =
  if poly.Length=0 then "0" else
  let sb = StringBuilder()
  let coefWrite (index:int) coef = 
    if index = 0 then (coeffToString coef) else      
        let xPart = if index=1 then "x" else "x^"+index.ToString()
        let coefPart = (if (ring.Compare ring.One coef) then "" else (coeffToString coef))
        coefPart+xPart
  let cf i = poly.[poly.Length-i-1]
  for i in 0..poly.Length-1 do
    let deg = poly.Length-1-i
    let cfNotZero = ring.IsNotZero.Invoke((cf i))
    if i>0 && cfNotZero then 
      sb.Append(" + ") |> ignore
    if cfNotZero then 
      sb.Append((coefWrite deg (cf i)) ) |> ignore
  sb.ToString()
 
/// Returns a human-readable string representation for given fraction
let GetFracString<'T> (ring: 'T Ring) (itemToString: 'T -> string) (frac: 'T Fraction) = 
  let (a,b) = FullFraction ring frac
  if ring.IsZero.Invoke(a) then "0" else
  if ring.Compare ring.One b then "(" + (itemToString a) + ")" else
  "(" + (itemToString a) + ")/(" + (itemToString b) + ")"

/// Usual polynomials with integer coefficients
type IntegerUnivariatePolyRing() = 
  // this imports all the logic just as planned
  inherit (BigInteger UnivariatePolynomialRing)(IntegerRing())  
  // we add a little beauty to print stuff to the console with style
  member _.GetString(poly: BigInteger[]) = 
    let sign (x: BigInteger) = if x < 0I then " - " else " + "
    let firstSign (x: BigInteger) = if x < 0I  then "-" else ""
    let abs (x: BigInteger) = if x < 0I then -x else x    
    let sb = new StringBuilder()
    let ubound = poly.Length - 1
    // zero poly is just "0"
    if poly.Length = 0 then sb.Append("0") |> ignore 
    else 
      for i in 0..ubound do 
        // we write our poly starting from the highest degree
        let power = ubound - i
        if poly.[power] <> 0I then 
          let signString = (if i = 0 then firstSign else sign) poly.[power]
          // if the highest degree is 0 (constant poly), we write the constant
          if ubound = 0 then sb.Append(poly.[0].ToString()) |> ignore
          // we don't write "1x", just "x"
          else sb.Append(if (power <> 0 && poly.[power] = 1I) 
                         then signString 
                         else signString + (abs poly.[power]).ToString() 
                        ) |> ignore 
          // we write "" instead of "x^0", and "x" instead of "x^1"
          // other than that, we write x^{power}.
          if power > 0 
          then sb.Append("x" + if (ubound - i = 1) 
                               then "" else "^" + (ubound - i).ToString()
                        ) |> ignore
    sb.ToString()