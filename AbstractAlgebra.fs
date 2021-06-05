/// Most basic implementation for abstract algebra types.
/// Refer to K.O. Geddes, S.R. Czapor, G. Labahn -- Algorithms for computer algebra
/// when unsure about naming conventions, terms etc.

module public rec AbstractAlgebra

type 'T BinaryOp(op : 'T -> 'T -> 'T) = member _.Op = op
type 'T UnaryOp(op: 'T -> 'T) = member _.Op = op
type 'T CommutativeBinaryOp(op: 'T -> 'T -> 'T) = inherit ('T BinaryOp)(op) 
type 'T EqualityChecker(checker: 'T -> 'T -> bool) = member _.AreEqual = checker

let inline SimpleAddition a b = a + b
let inline SimpleMultiplication a b = a * b
let inline SimpleEqualityCheck a b = a = b

let private PadArrayWith array paddingLength paddingElement = 
    if paddingLength = 0 then array 
    else Array.concat [array; (Array.create paddingLength paddingElement)]

type 'T Semigroup = 
    abstract member Op : 'T -> 'T -> 'T
    abstract member AreEqual : 'T -> 'T -> bool
  
type 'T Monoid = 
    inherit Semigroup<'T>
    abstract member NeutralElement : 'T

type 'T CommutativeSemigroup = 
    inherit ('T Semigroup)

type 'T CommutativeMonoid = 
    inherit ('T CommutativeSemigroup)
    inherit ('T Monoid)

type 'T Group = 
    inherit ('T Monoid)
    abstract member Invert : 'T -> 'T

type 'T CommutativeGroup = 
    inherit ('T CommutativeMonoid)
    inherit ('T Group)
   
type 'T Rng = 
    abstract member AdditionStructure : 'T CommutativeGroup
    abstract member MultiplicationStructure : 'T Semigroup
    abstract member Zero : 'T
    abstract member Add : 'T -> 'T -> 'T
    abstract member Negate : 'T -> 'T
    abstract member Subtract : 'T -> 'T -> 'T
    abstract member Multiply : 'T -> 'T -> 'T
    abstract member IsZero : 'T -> bool
    abstract member IsNotZero : 'T -> bool
    abstract member AreEqual : 'T -> 'T -> bool
    abstract member GetString : 'T -> string
    abstract member Coerce : obj -> 'T option
type 'T Ring = 
    inherit ('T Rng)
    abstract member MultiplicationStructure : 'T Monoid
    abstract member One : 'T
    abstract member IntegerConstant : int -> 'T
    abstract member IsOne : 'T -> bool

type 'T CommutativeRing = 
    inherit ('T Ring)
    abstract member MultiplicationStructure : 'T CommutativeMonoid

type 'T Domain = 
    inherit ('T Ring)

type 'T IntegralDomain = 
    inherit ('T Domain)
    inherit ('T CommutativeRing)  
    abstract member UnitAndNormalParts : 'T -> 'T * 'T
    abstract member UnitPart : 'T -> 'T
    abstract member NormalPart : 'T -> 'T
    abstract member UnitInverse : 'T -> 'T
   
type 'T UniqueFactorizationDomain = 
    inherit ('T IntegralDomain)  
    abstract member DivRem : 'T -> 'T -> (('T * 'T) option)
    abstract member Div : 'T -> 'T -> ('T option)
    abstract member Rem : 'T -> 'T -> ('T option) 


type 'T EuclideanDomain = 
    inherit ('T UniqueFactorizationDomain)  
    abstract member Valuation : 'T -> (bigint option)
    abstract member Gcd : 'T -> 'T -> ('T option) 
    abstract member Eea : 'T -> 'T -> ('T * 'T) 

type 'T Field = 
    inherit ('T EuclideanDomain)
    abstract member Divide : 'T -> 'T -> ('T option)
    abstract member Invert : 'T -> ('T option)

let private MakeIntegerConstant (ring : 'T Ring) (maker : (int -> 'T) option) (n) = 
    if maker.IsSome then (maker.Value n) else
    let mutable multiply = if n < 0 then (ring.Negate ring.One) else ring.One
    let mutable count = if n < 0 then -n else n
    let mutable sum = ring.Zero
    while count > 0 do      
        if (count % 2 = 1) then sum <- ring.Add sum multiply
        count <- count / 2
        multiply <- ring.Add multiply multiply
    sum 
 
module public Construct = 
  type 'T Semigroup(op: BinaryOp<'T>, eqChecker: EqualityChecker<'T>) =  
    member _.Op a b = op.Op a b
    member _.AreEqual a b = eqChecker.AreEqual a b
    interface AbstractAlgebra.Semigroup<'T> with 
      member p.Op a b = p.Op a b
      member p.AreEqual a b = p.AreEqual a b
  type 'T Monoid (neutralElement, op : BinaryOp<'T>, eqChecker) = 
    inherit Semigroup<'T>(op, eqChecker)
    member _.NeutralElement = neutralElement
    interface AbstractAlgebra.Monoid<'T> with member m.NeutralElement = m.NeutralElement
  type 'T CommutativeSemigroup(op : CommutativeBinaryOp<'T>, eqChecker) = 
    inherit Semigroup<'T>(op, eqChecker)
    interface AbstractAlgebra.CommutativeSemigroup<'T> with member s.Op a b = s.Op a b
  type 'T CommutativeMonoid(neutralElement, op, eqChecker) = 
    inherit CommutativeSemigroup<'T>(op, eqChecker)
    member _.NeutralElement = neutralElement
    interface AbstractAlgebra.CommutativeMonoid<'T> with member m.NeutralElement = m.NeutralElement    
  type 'T Group(neutralElement, op : BinaryOp<'T>, invert: UnaryOp<'T>, eqChecker) = 
    inherit Monoid<'T>(neutralElement, op, eqChecker)
    member _.Invert a = invert.Op a
    interface AbstractAlgebra.Group<'T> with member p.Invert a = p.Invert a
  type 'T CommutativeGroup(neutralElement, op : CommutativeBinaryOp<'T>, invert, eqChecker) =
    inherit Group<'T>(neutralElement, op, invert, eqChecker)
    interface AbstractAlgebra.CommutativeGroup<'T>  

  let private Coerce<'T> (obj:obj) = match obj with | :? 'T as x -> Some x | _ -> None
   
  type 'T Rng(additionGroup: 'T AbstractAlgebra.CommutativeGroup, multiplicationSemigroup: 'T AbstractAlgebra.Semigroup, getString: ('T -> string)) = 
    let mutable _coercion : (obj -> 'T option) = fun (obj:obj) -> match obj with
                                                                     | :? 'T as x -> Some x
                                                                     | _ -> None                                                                                                                                         
    member _.SetCoercion (newCoercion: obj -> 'T option) = _coercion <- newCoercion
    member _.Coerce p = _coercion p 
    member _.AdditionStructure = additionGroup
    member _.MultiplicationStructure = multiplicationSemigroup
    member _.Zero = additionGroup.NeutralElement
    member _.Add a b = additionGroup.Op a b
    member _.Negate a = additionGroup.Invert a
    member _.Subtract a b = additionGroup.Op a (additionGroup.Invert b)
    member _.Multiply a b = multiplicationSemigroup.Op a b
    member _.IsZero a = additionGroup.AreEqual additionGroup.NeutralElement a
    member _.IsNotZero a = not (additionGroup.AreEqual additionGroup.NeutralElement a)
    member _.AreEqual a b = additionGroup.AreEqual a b
    member _.GetString a = getString a
    interface AbstractAlgebra.Rng<'T> with 
      member p.Coerce obj = p.Coerce obj
      member _.AdditionStructure = additionGroup
      member _.MultiplicationStructure = multiplicationSemigroup
      member _.Zero = additionGroup.NeutralElement
      member _.Add a b = additionGroup.Op a b
      member _.Negate a = additionGroup.Invert a
      member _.Subtract a b = additionGroup.Op a (additionGroup.Invert b)
      member _.Multiply a b = multiplicationSemigroup.Op a b
      member _.IsZero a = additionGroup.AreEqual additionGroup.NeutralElement a
      member _.IsNotZero a = not (additionGroup.AreEqual additionGroup.NeutralElement a)
      member _.AreEqual a b = additionGroup.AreEqual a b  
      member p.GetString a = getString a
  type 'T Ring(additionGroup, multiplicationMonoid: 'T AbstractAlgebra.Monoid, integerConstantMaker, getString) = 
    inherit ('T Rng)(additionGroup, multiplicationMonoid, getString)    
    member _.One = multiplicationMonoid.NeutralElement
    member _.MultiplicationStructure = multiplicationMonoid
    member _.IsOne x = multiplicationMonoid.AreEqual x multiplicationMonoid.NeutralElement
    member p.IntegerConstant n = (AbstractAlgebra.MakeIntegerConstant p integerConstantMaker n)
    interface AbstractAlgebra.Ring<'T> with 
      member q.IsOne x = q.IsOne x
      member q.One = q.One
      member q.MultiplicationStructure = q.MultiplicationStructure
      member q.IntegerConstant n = q.IntegerConstant n
  type 'T CommutativeRing(additionGroup, multiplicationMonoid: 'T AbstractAlgebra.CommutativeMonoid, integerConstantMaker, getString) = 
    inherit ('T Ring)(additionGroup, multiplicationMonoid, integerConstantMaker, getString)    
    member _.MultiplicationStructure = multiplicationMonoid    
    interface AbstractAlgebra.CommutativeRing<'T> with member q.MultiplicationStructure = q.MultiplicationStructure
  type 'T Domain(additionGroup, multiplicationMonoid, integerConstantMaker, getString) = 
    inherit ('T Ring)(additionGroup, multiplicationMonoid, integerConstantMaker, getString)
    interface AbstractAlgebra.Domain<'T>   
  type 'T IntegralDomain(additionGroup, multiplicationMonoid, integerConstantMaker, unitAndNormalParts, unitInverse, getString) = 
    inherit ('T CommutativeRing)(additionGroup, multiplicationMonoid, integerConstantMaker, getString)
    member _.UnitAndNormalParts a = unitAndNormalParts a
    member _.UnitPart a = fst (unitAndNormalParts a)
    member _.NormalPart a = snd (unitAndNormalParts a) 
    member _.UnitInverse u = unitInverse u
    interface AbstractAlgebra.IntegralDomain<'T> with 
      member d.UnitAndNormalParts a = d.UnitAndNormalParts a
      member d.UnitPart a = d.UnitPart a
      member d.NormalPart a = d.NormalPart a
      member d.UnitInverse u = d.UnitInverse u
  type 'T UniqueFactorizationDomain(additionGroup: 'T AbstractAlgebra.CommutativeGroup, multiplicationMonoid: 'T AbstractAlgebra.CommutativeMonoid, 
                                    integerConstantMaker, unitAndNormalParts, divRem: 'T -> 'T -> ('T * 'T) option, getString) = 
    inherit ('T IntegralDomain)(additionGroup, multiplicationMonoid, integerConstantMaker, unitAndNormalParts, 
      (fun u -> fst (divRem multiplicationMonoid.NeutralElement u).Value), getString)
    member _.DivRem a b = divRem a b
    member _.Div a b = (divRem a b) |> Option.map fst
    member _.Rem a b = (divRem a b) |> Option.map snd
    interface AbstractAlgebra.UniqueFactorizationDomain<'T> with 
      member q.DivRem a b = q.DivRem a b
      member q.Div a b = q.Div a b
      member q.Rem a b = q.Rem a b
  type 'T EuclideanDomain(additionGroup, multiplicationMonoid, integerConstantMaker, unitAndNormalParts, divRem, valuation, getString) = 
    inherit ('T UniqueFactorizationDomain)(additionGroup, multiplicationMonoid, integerConstantMaker, unitAndNormalParts, divRem, getString)
    member _.Valuation p = valuation p
    member this.Gcd a b =     
      if ((this.AreEqual this.Zero a) || (this.AreEqual this.Zero b)) then None 
      else   
        let mutable c = this.NormalPart a
        let mutable d = this.NormalPart b 
        while not (this.AreEqual this.Zero d) do
          let r = (this.Rem c d).Value // this is valid because Rem only returns None when d is zero
          c <- d
          d <- r
        Some (this.NormalPart c)
    // extended euclidean algorithm, (a,b) -> (s,t) such that as+bt = 1
    member this.Eea a b = 
      let mutable c = this.NormalPart a
      let mutable d = this.NormalPart b 
      let mutable c1 = this.One
      let mutable d1 = this.Zero
      let mutable c2 = this.Zero
      let mutable d2 = this.One
      while this.IsNotZero d do
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
    interface AbstractAlgebra.EuclideanDomain<'T> with 
      member e.Valuation p = e.Valuation p
      member e.Eea a b = e.Eea a b
      member e.Gcd a b = e.Gcd a b
  type 'T Field(additiveGroup, multiplicativeMonoid, integerConstantMaker, unitAndNormalParts, divide: 'T -> 'T -> 'T option, getString) = 
    inherit ('T EuclideanDomain)(additiveGroup,  
                                 multiplicativeMonoid,
                                 integerConstantMaker,          
                                 unitAndNormalParts,
                                 (fun a b -> if (additiveGroup.AreEqual b additiveGroup.NeutralElement) then None 
                                             else Some((divide a b).Value, additiveGroup.NeutralElement)),
                                 (fun a -> if (additiveGroup.AreEqual additiveGroup.NeutralElement a) then None else Some(0I)), getString)
    member f.Divide a b = divide a b
    member f.Invert a = (divide multiplicativeMonoid.NeutralElement a) 
    interface AbstractAlgebra.Field<'T> with 
      member f.Divide a b = f.Divide a b
      member f.Invert a = f.Invert a