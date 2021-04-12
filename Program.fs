// Basic abstract algebra types

open System
open System.Numerics

type ITypeWithAddition<'T> =  
    abstract add : 'T -> 'T 
type ITypeWithMultiplication<'T> = 
    abstract multiply : 'T -> 'T 
type ITypeWithMultiplicativeIdentity<'T> =
    abstract one : 'T    
type ITypeWithMultiplicativeInverse<'T> =
    inherit ITypeWithMultiplicativeIdentity<'T>
    abstract inverse : 'T option


type IAdditiveGroup<'T when 'T :> ITypeWithAddition<'T>> = 
    inherit ITypeWithAddition<'T>
    abstract negate : 'T
    abstract zero : 'T
    abstract isZero : bool

open System.Runtime.CompilerServices

[<Extension>]
type Extensions() = 
    [<Extension>]
    static member subtract(q : IAdditiveGroup<'T>, p : IAdditiveGroup<'T>) = p.add(q.negate)    

type IMultiplicativeSemigroup<'T> =
    inherit ITypeWithMultiplication<'T>

type IMultiplicativeGroup<'T> =
    inherit IMultiplicativeSemigroup<'T>
    inherit ITypeWithMultiplicativeInverse<'T>


type IRing<'TValue when 'TValue :> IAdditiveGroup<'TValue> and 'TValue :> IMultiplicativeSemigroup<'TValue>> =     
    inherit IMultiplicativeSemigroup<'TValue> 
    inherit IAdditiveGroup<'TValue> 

type IIntegralDomain<'TValue when 'TValue :> IRing<'TValue>> = 
    inherit IRing<'TValue>
    inherit ITypeWithMultiplicativeIdentity<'TValue>
    abstract member unitPart : 'TValue
    abstract member normalPart : 'TValue


type IUniqueFactorizationDomain<'TValue when 'TValue :> IIntegralDomain<'TValue>> = 
    inherit IIntegralDomain<'TValue>
    abstract member divRem : 'TValue -> ('TValue * 'TValue) option

type IEuclideanDomain<'TValue when 'TValue :> IUniqueFactorizationDomain<'TValue>> = 
    inherit IUniqueFactorizationDomain<'TValue>
    abstract member valuation : 'TValue -> int option 

[<Extension>]
type EuclideanDomainExtensions() = 
    [<Extension>]
    static member gcdWith(p: IEuclideanDomain<'TValue>, q : 'TValue) = 
        let gcd (a : IEuclideanDomain<'TValue>, b: 'TValue) = 
            let mutable c = a.normalPart
            let mutable d = b.normalPart
            while not d.isZero do
                let r = c.divRem d
                c <- d
                d <- match r with
                    | Some (div : 'TValue, rem : 'TValue) -> rem
                    | None -> c.zero
            c.normalPart
        gcd(p, q)

        



type IField<'TValue when 'TValue :> IAdditiveGroup<'TValue> and 'TValue :> IMultiplicativeGroup<'TValue>> =
    inherit IRing<'TValue> 
    inherit IMultiplicativeGroup<'TValue> 

type IntegerValueType (bigInt : BigInteger) =
    member _.value = bigInt
    interface IUniqueFactorizationDomain<IntegerValueType> with 
        member _.zero = new IntegerValueType(BigInteger.Zero)     
        member _.one = new IntegerValueType(BigInteger.One)     
        member p.add(q) = new IntegerValueType(p.value + q.value)
        member p.negate = new IntegerValueType(-p.value)
        member p.isZero = p.value.IsZero     
        member p.multiply(q) = new IntegerValueType(p.value * q.value)
        member p.divRem(q) = if (q:>IAdditiveGroup<IntegerValueType>).isZero then None else Some (new IntegerValueType(p.value / q.value), new IntegerValueType(p.value % q.value))
        member p.normalPart = new IntegerValueType(p.value * new BigInteger(p.value.Sign))
        member p.unitPart = new IntegerValueType(new BigInteger(p.value.Sign))



type PolyRing<'TCoefficient when 'TCoefficient :> IRing<'TCoefficient>>(coeffs : 'TCoefficient[])  =
    member _.coeffs = coeffs  
    member _.zero = new PolyRing<'TCoefficient>([||])
    interface IAdditiveGroup<PolyRing<'TCoefficient>> with 
        member _.zero = new PolyRing<'TCoefficient>([||])
        member p.isZero = p.coeffs.Length = 0
        member p.negate = new PolyRing<'TCoefficient>( Array.map (fun q -> (q :> IAdditiveGroup<'TCoefficient>).negate) p.coeffs)
        member p.add(q) = 
            let l1 = if p.coeffs.Length > q.coeffs.Length then p.coeffs else q.coeffs
            let l2small = if p.coeffs.Length > q.coeffs.Length then q.coeffs else p.coeffs
            let lendiff = l1.Length - l2small.Length
            let l2 = if lendiff = 0 then l2small else Array.concat [ l2small ; [| for i in 1..lendiff -> (l1 .[0]).zero |] ]            
            let resultArray = Array.map2 (fun (p:'TCoefficient) -> fun q -> p.add(q)) l1 l2
            let lastNonZero = Array.FindLastIndex(resultArray, fun coeff -> not coeff.isZero)
            new PolyRing<'TCoefficient>(Array.sub resultArray.[..lastNonZero] 0 (lastNonZero+1))

type IntPolyRing(coeffs : IntegerValueType[]) =
    inherit PolyRing<IntegerValueType>(coeffs)

[<EntryPoint>]
let main argv =

    let poly1 = new IntPolyRing([| new IntegerValueType(new BigInteger(1)) ; new IntegerValueType(new BigInteger(4)) |] )
    let poly2 = new IntPolyRing([| new IntegerValueType(new BigInteger(2)) ; new IntegerValueType(new BigInteger(3)) ; new IntegerValueType(new BigInteger(7)) |] )
        
    let p3 = (poly1 :> IAdditiveGroup<PolyRing<IntegerValueType>>).add(poly2);

    printfn "At least this didn't freeze!" 
    0 // return an integer exit code
