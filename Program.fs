﻿// Basic abstract algebra types
open MathTypes

[<EntryPoint>]
let main argv =     
    printfn "Q(x) Quotient Field test:"
    // We start with JUST INTEGERS, nothing else. IntegerRing is the euclidean domain of integers,
    // that just defers its implementation to bigint.

    // We construct the field of fractions for it, and get rationals with automatic simplify
    // of fractions, i.e. we will get 1/3+1/6 = 1/2, not 3/6!
    let rationalField = QuotientField(IntegerRing())
    // We construct the domain of univariate polynomials over Q, and get poly DivRem for free,
    // including normalization unit part is lc(f), normal part is monic poly
    let rationalPolyDomain = UnivariatePolyOverFieldDomain(rationalField)
    // finally we construct the field of fractions Q(x), for all rational functions of x,
    // and get function simplify, gcd and such, for free, just for it being a quotient field.
    let rationalFunctionsOfX = QuotientField(rationalPolyDomain)
    
    let polyDerive (f: (bigint * bigint)[]) =
      Array.mapi (fun n (a,b) -> (rationalField.Multiply (a, b) (bigint(n+1), 1I))) (Array.sub f 1 (f.Length-1))
    

    let ratFunDifField = DifferentialField(rationalFunctionsOfX, (fun (p,q) ->         
         let polyOne = [|(1I, 1I)|]
         let a = (p, polyOne)
         let b = (q, polyOne)
         let dA = ((polyDerive p), polyOne)
         let dB = ((polyDerive q), polyOne)         
         let nume = (rationalFunctionsOfX.Subtract (rationalFunctionsOfX.Multiply dA b)
                                                   (rationalFunctionsOfX.Multiply a dB))
         let deno = rationalFunctionsOfX.Multiply (q, polyOne) (q, polyOne)
         (rationalFunctionsOfX.Divide nume deno).Value
        )
    )

    // surely we'd love pretty-printing our polys
    let getQPolyString = GetPolyString rationalField (fun (a,b) -> if a=0I then "0"
                                                                   else if b=1I then if a<0I 
                                                                                     then "("+a.ToString()+")"
                                                                                     else a.ToString()
                                                                   else if a<0I 
                                                                   then "("+a.ToString()+"/"+b.ToString()+")"
                                                                   else a.ToString()+"/"+b.ToString() )
     
    // the literals are ugly so far, but even with those we can feel the power of abstract algebra
    // here, we construct f(x)=(x+1)/(x²+1) and g(x)=(x-2)/(x²+4)
    let f = ( [|(1I, 1I) ; (1I, 1I)|], [|(1I, 1I) ; (0I, 1I) ; (1I, 1I)|] )
    let g = ( [|(-2I, 1I) ; (1I, 1I)|], [|(4I, 1I) ; (0I, 1I) ; (1I, 1I)|] )
    // let's test adding those two
    let (sumNum, sumDen) = rationalFunctionsOfX.Add f g

    printfn "(%s)/(%s)  +  (%s)/(%s)   =   (%s)/(%s)" (getQPolyString (fst f)) (getQPolyString (snd f))
                                                      (getQPolyString (fst g)) (getQPolyString (snd g))
                                                      (getQPolyString sumNum) (getQPolyString sumDen)
    
    printfn ""
    printfn "Derivation test"
    printfn ""

    let pd = (polyDerive sumNum)

    printfn "D[%s] = %s" (getQPolyString sumNum) (getQPolyString pd)
    
    printfn ""

    let (dn, dd) = ratFunDifField.Derive (sumNum, sumDen)

    printfn "D[(%s)/(%s)] = %s (%s)/(%s)" (getQPolyString sumNum) (getQPolyString sumDen)
                                       (System.Environment.NewLine)
                                       (getQPolyString dn) (getQPolyString dd)

    0 // return an integer exit code
