module MathTypes

open System.Numerics
open System
open System.Text

/// Generic binary operation with no conditions forced upon it
type 'T BinaryOp(op : 'T -> 'T -> 'T) = member _.Op = op

/// Generic unary operation with no conditions forced upon it
type 'T UnaryOp(op: 'T -> 'T) = member _.Op = op

/// Equivalence check operation, that is, reflexive symmetric transitive function t -> t -> bool
type 'T EquivalenceOp = 'T -> 'T -> bool

/// Commutative binary operation, i.e. (op a b) is equivalent to (op b a)
type 'T CommutativeBinaryOp(op) = inherit ('T BinaryOp)(op)

let private SimpleEquals a b = a = b
let private SimpleAdd a b = a + b
let private SimpleMultiply a b = a * b

/// <summary>
/// Semigroup from elements of type <typeparamref name="'T">'T</typeparamref> 
/// with group operation <paramref name="op"/> and equivalence check <paramref name="compare"/>
/// </summary>
type 'T Semigroup(op: 'T BinaryOp, compare : 'T EquivalenceOp) = 
  member _.Op x y = op.Op x y
  member _.Compare x y = compare x y
  
/// <summary>
/// Monoid from elements of type <typeparamref name="'T">'T</typeparamref> with neutral element <paramref name="neutralElement"/>,
/// group operation <paramref name="op"/>, and equivalence check <paramref name="compare"/>
/// </summary>
type 'T Monoid(neutralElement: 'T, op: 'T BinaryOp, compare : 'T EquivalenceOp) = 
  inherit ('T Semigroup)(op, compare)
  member _.NeutralElement = neutralElement
  member _.Concat xs = Seq.fold op.Op neutralElement xs
  
/// <summary>
/// Commutative semigroup from elements of type <typeparamref name="'T">'T</typeparamref> 
/// with commutative group operation <paramref name="op"/>, and equivalence check <paramref name="compare"/>
/// </summary>
type 'T CommutativeSemigroup(op: 'T CommutativeBinaryOp, compare: 'T EquivalenceOp) = 
  inherit ('T Semigroup)(op, compare)
  
/// <summary>
/// Commutative monoid from elements of type <typeparamref name="'T">'T</typeparamref> with neutral element <paramref name="neutralElement"/>,
/// commutative group operation <paramref name="op"/>, and equivalence check <paramref name="compare"/>
/// </summary>
/// <remarks> This should in fact inherit both Monoid and CommutativeSemigroup. Have yet to find a way of doing that in full generality. </remarks>
type 'T CommutativeMonoid(neutralElement: 'T, op: 'T CommutativeBinaryOp, compare : 'T EquivalenceOp) = 
  inherit ('T Monoid)(neutralElement, op, compare)

/// <summary>
/// Group from elements of type <typeparamref name="'T">'T</typeparamref> with neutral element <paramref name="neutralElement"/>,
/// group operation <paramref name="op"/>, inversion <paramref name="invert"/>, and equivalence check <paramref name="compare"/>
/// </summary>
type 'T Group(neutralElement: 'T, op: 'T BinaryOp, invert: 'T UnaryOp, compare : 'T EquivalenceOp) =
  inherit ('T Monoid)(neutralElement, op, compare)  
  member _.Invert x = invert.Op x

/// <summary>
/// Commutative, or Abelian, group from elements of type <typeparamref name="'T">'T</typeparamref> 
/// with neutral element <paramref name="neutralElement"/>,
/// commutative group operation <paramref name="op"/>, 
/// inversion <paramref name="invert"/>, 
/// and equivalence check <paramref name="compare"/>
/// </summary>
type 'T CommutativeGroup(neutralElement: 'T, op: 'T CommutativeBinaryOp, invert: 'T UnaryOp, compare : 'T EquivalenceOp) =
  inherit ('T Group)(neutralElement, op, invert, compare)
  
/// <summary>
/// Nonunital ring (or Rng or pseudo-ring) from elements of type <typeparamref name="'T">'T</typeparamref> 
/// constructed from an additive commutative group <paramref name="additiveGroup"/> 
/// and a multiplicative semigroup <paramref name="multiplicativeSemigroup"/>.
/// </summary>
type 'T Rng(additiveGroup: 'T CommutativeGroup, multiplicativeSemigroup: 'T Semigroup) =
  /// <summary>Additive identity, that is, e such that ∀x x+e=e+x=x </summary>
  member _.Zero = additiveGroup.NeutralElement 
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
type 'T Ring(additiveGroup: 'T CommutativeGroup, multiplicativeMonoid: 'T Monoid) = 
  inherit ('T Rng)(additiveGroup, multiplicativeMonoid)
  member _.One = multiplicativeMonoid.NeutralElement

/// <summary>
/// Commutative ring from elements of type <typeparamref name="'T">'T</typeparamref> 
/// constructed from an additive commutative group <paramref name="additiveGroup"/> 
/// and a multiplicative commutative monoid <paramref name="multiplicativeMonoid"/>.
/// </summary>
type 'T CommutativeRing(additiveGroup: 'T CommutativeGroup, multiplicativeCommutativeMonoid: 'T CommutativeMonoid) = 
  inherit ('T Ring)(additiveGroup, multiplicativeCommutativeMonoid)  

/// <summary>
/// Domain from elements of type <typeparamref name="'T">'T</typeparamref> 
/// constructed from an additive commutative group <paramref name="additiveGroup"/> 
/// and a multiplicative monoid <paramref name="multiplicativeMonoid"/>, that
/// additionally satisfies the zero-product property, namely, if (ab==0) then either (a==0) or (b==0)
/// </summary>
type 'T Domain(additiveGroup: 'T CommutativeGroup, multiplicativeMonoid: 'T Monoid) = 
  inherit ('T Ring)(additiveGroup, multiplicativeMonoid)

/// <summary>
/// Ingegral (or Commutative) Domain from elements of type <typeparamref name="'T">'T</typeparamref> 
/// constructed from an additive commutative group <paramref name="additiveGroup"/> 
/// and a multiplicative commutative monoid <paramref name="multiplicativeMonoid"/>, that
/// additionally satisfies the zero-product property, namely, if (ab==0) then either (a==0) or (b==0)
/// </summary>
type 'T IntegralDomain(additiveGroup: 'T CommutativeGroup, multiplicativeCommutativeMonoid: 'T CommutativeMonoid, unitAndNormalParts : 'T -> ('T * 'T)) =
  inherit ('T Domain)(additiveGroup, multiplicativeCommutativeMonoid)
  member _.UnitAndNormalParts x = unitAndNormalParts x
  member _.UnitPart x = fst (unitAndNormalParts x)
  member _.NormalPart x = snd (unitAndNormalParts x)

type 'T UniqueFactorizationDomain(additiveGroup: 'T CommutativeGroup, multiplicativeCommutativeMonoid: 'T CommutativeMonoid, 
                                  unitAndNormalParts : 'T -> ('T * 'T), divRem : 'T -> 'T -> ('T * 'T) option) = 
  inherit ('T IntegralDomain)(additiveGroup, multiplicativeCommutativeMonoid, unitAndNormalParts)
  member _.DivRem x y = divRem x y
  member _.Div x y = match divRem x y with 
                     | Some (d, _) -> Some(d)
                     | None -> None
  member _.Rem x y = match divRem x y with 
                     | Some (_, r) -> Some(r)
                     | None -> None

type 'T EuclideanDomain(additiveGroup: 'T CommutativeGroup, multiplicativeCommutativeMonoid: 'T CommutativeMonoid, 
                        unitAndNormalParts : 'T -> ('T * 'T), divRem : 'T -> 'T -> ('T * 'T) option,
                        valuation: 'T -> BigInteger option) = 
  inherit ('T UniqueFactorizationDomain)(additiveGroup, multiplicativeCommutativeMonoid, unitAndNormalParts, divRem)
  member _.Valuation x = valuation x
  member this.Gcd(a, b) : 'T option =     
    if ((this.Compare this.Zero a) || (this.Compare this.Zero b)) then None 
    else   
      let mutable c = this.NormalPart a
      let mutable d = this.NormalPart b 
      while not (this.Compare this.Zero d) do
        let r = this.Rem c d
        c <- d
        d <- match r with 
             | Some (rem) -> rem
             | None -> this.Zero //we should NEVER get here as d is already checked to be non-zero
      Some (this.NormalPart c)

type 'TCoefficient UnivariatePolynomialAdditiveGroup(coefficientRing: 'TCoefficient Ring) = 
  inherit CommutativeGroup<'TCoefficient[]>([||], new ('TCoefficient[] CommutativeBinaryOp)(fun polyA -> fun polyB -> 
                                                    let l1 = if polyA.Length > polyB.Length then polyA else polyB
                                                    let l2small = if polyA.Length > polyB.Length then polyB else polyA
                                                    let lengthDiff = l1.Length - l2small.Length
                                                    let l2 = if lengthDiff = 0 
                                                             then l2small 
                                                             else Array.concat [l2small; (Array.create lengthDiff coefficientRing.Zero)]         
                                                    let resultArray = Array.map2 (fun (p:'TCoefficient) -> fun q -> coefficientRing.Add p q) l1 l2
                                                    let lastNonZero = array.FindLastIndex(resultArray, 
                                                                                          fun coeff -> not (coefficientRing.Compare coeff coefficientRing.Zero))
                                                    (Array.sub resultArray.[..lastNonZero] 0 (lastNonZero+1))     
    ), 
    new UnaryOp<'TCoefficient[]>(fun poly -> Array.map coefficientRing.Negate poly), fun polyA -> fun polyB -> 
      (polyA.Length = polyB.Length) && ((polyA.Length = 0) || array.TrueForAll( Seq.toArray(Seq.zip polyA polyB), fun (co1, co2)-> coefficientRing.Compare co1 co2) ))


type 'TCoefficient UnivariatePolynomialMultiplicativeMonoid(coefficientRing: 'TCoefficient Ring) = 
  inherit Monoid<'TCoefficient[]>([||], new ('TCoefficient[] BinaryOp)(fun polyA -> fun polyB -> 
                                                    let resultingDegree = polyA.Length + polyB.Length
                                                    if resultingDegree = 0 then [||] else
                                                        let result = Array.create resultingDegree coefficientRing.Zero 
                                                        for i in 0..polyA.Length-1 do 
                                                          for j in 0..polyB.Length-1 do
                                                            Array.set result (i+j) (coefficientRing.Add result.[i+j] ( coefficientRing.Multiply  polyA.[i] polyB.[j] ))
                                                        result
    ), 
    fun polyA -> fun polyB -> (polyA.Length = polyB.Length) && ((polyA.Length = 0) || array.TrueForAll( Seq.toArray(Seq.zip polyA polyB), fun (co1, co2)-> coefficientRing.Compare co1 co2) ))


type 'TCoefficient UnivariatePolynomialRing(coefficientRing : 'TCoefficient Ring) =
  inherit Ring<'TCoefficient[]>(new ('TCoefficient UnivariatePolynomialAdditiveGroup)(coefficientRing), new ('TCoefficient UnivariatePolynomialMultiplicativeMonoid)(coefficientRing))


type IntegerRing() = 
  inherit EuclideanDomain<bigint>(
    (new CommutativeGroup<bigint>(0I, new CommutativeBinaryOp<bigint>(SimpleAdd), new UnaryOp<bigint>(bigint.Negate), SimpleEquals)),
    (new CommutativeMonoid<bigint>(0I, new CommutativeBinaryOp<bigint>(SimpleMultiply), SimpleEquals)),
    (fun p -> ((if p < 0I then -1I else 1I), (if p<0I then -p else p))),
    (fun p -> fun q -> if q=0I then None else Some(p/q, p % q)),
    (fun p -> Some(if p < 0I then -p else p))
  )

type IntegerUnivariatePolyRing() = 
  inherit (BigInteger UnivariatePolynomialRing)(IntegerRing())  
  member _.GetString(poly: BigInteger[]) = 
    let sign (x: BigInteger) = if x < 0I then " - " else " + "
    let firstSign (x: BigInteger) = if x < 0I  then "-" else ""
    let abs (x: BigInteger) = if x < 0I then -x else x    
    let sb = new StringBuilder()
    let ubound = poly.Length - 1
    if poly.Length = 0 then sb.Append("0") |> ignore 
    else
      for i in 0..ubound do 
        if ubound = 0 then sb.Append(poly.[0].ToString()) |> ignore
        else sb.Append( 
                       if (ubound <> i && poly.[ubound - i] = 1I) 
                       then "" 
                       else (
                         ( 
                           (if i = 0 then firstSign else sign) poly.[ubound-i]
                         )
                       ) + (abs poly.[ubound - i]).ToString() ) |> ignore 
        if i < ubound 
        then sb.Append("x" + if (ubound - i = 1) 
                             then "" 
                             else "^"+(ubound - i).ToString()
                      ) |> ignore
    sb.ToString()