// Basic abstract algebra types
open MathTypes 

[<EntryPoint>]
let main argv =     
    printfn "Q(x) Quotient Field test:"
    // We start with JUST INTEGERS, nothing else. IntegerRing is the euclidean domain of integers,
    // that just defers its implementation to bigint.
    let integerRing = IntegerRing()
    // We construct the field of fractions for it, and get rationals with automatic simplify
    // of fractions, i.e. we will get 1/3+1/6 = 1/2, not 3/6!
    
    let rationalField = QuotientField(integerRing)
    
    let oneThird = F(1I, 3I)
    let oneFifth = F(1I, 5I)
    let sum = rationalField.Add oneThird oneFifth
   
    // We construct the domain of univariate polynomials over Q, and get poly DivRem for free,
    // including normalization unit part is lc(f), normal part is monic poly
    let rationalPolyDomain = UnivariatePolyOverFieldDomain(rationalField)
    // finally we construct the field of fractions Q(x), for all rational functions of x,
    // and get function simplify, gcd and such, for free, just for it being a quotient field.
    let rationalFunctionsOfX = QuotientField(rationalPolyDomain)      

    let fullFrac = FullFraction rationalPolyDomain

    // derivation on Q[x]
    let polyDerive (f: (bigint Fraction)[]) =
      if f.Length < 2 then [|N 0I|] else
      Array.mapi (fun n f -> (rationalField.Multiply f (F(bigint(n+1), 1I)))) (Array.sub f 1 (f.Length-1))
    // Q(x) is a differential field, given derivation on Q[x]:
    let ratFunDifField = DifferentialField(rationalFunctionsOfX, (fun f ->      
         let (p,q) = FullFraction rationalPolyDomain f         
         let a = N p
         let b = N q
         let dA = N(polyDerive p)
         let dB = N(polyDerive q)         
         let nume = (rationalFunctionsOfX.Subtract (rationalFunctionsOfX.Multiply dA b)
                                                   (rationalFunctionsOfX.Multiply a dB))
         let deno = rationalFunctionsOfX.Multiply (N q) (N q)
         (rationalFunctionsOfX.Divide nume deno).Value
       ))

    // surely we'd love pretty-printing our polys
    let getQPolyString = GetPolyString rationalField (fun frac -> 
                                                                   let (a,b) = (FullFraction integerRing frac)
                                                                   if a=0I then "0"
                                                                   else if b=1I then if a<0I 
                                                                                     then "("+a.ToString()+")"
                                                                                     else a.ToString()
                                                                   else if a<0I 
                                                                   then "("+a.ToString()+"/"+b.ToString()+")"
                                                                   else a.ToString()+"/"+b.ToString() )
     
    let getQRatFunString = GetFracString rationalPolyDomain getQPolyString
    
    // the literals are ugly so far, but even with those we can feel the power of abstract algebra
    // here, we construct f(x)=(x+1)/(x²+1) and g(x)=(x-2)/(x²+4)
    let f = F( [|N 1I; N 2I; N 1I|], [|N 5I; N 1I|] )
    let g = F( [|N 2I; N 1I|], [|N 1I|] )
    // let's test adding those two
    let (sumNum, sumDen) = rationalFunctionsOfX.Add f g |> fullFrac
    
    printfn "%s  +  %s   =   %s" (getQRatFunString f)
                                                      (getQRatFunString g)
                                                      (getQRatFunString (F(sumNum, sumDen)))
    // now let's do some calculus-pre-I
    printfn ""
    printfn "Derivation test"
    printfn ""

    let pd = (polyDerive sumNum)

    printfn "D[%s] = %s" (getQPolyString sumNum) (getQPolyString pd)
    
    printfn ""

    let (dn, dd) = ratFunDifField.Derive (F(sumNum, sumDen)) |> fullFrac

    printfn "D[(%s)/(%s)] = %s (%s)/(%s)" (getQPolyString sumNum) (getQPolyString sumDen)
                                       (System.Environment.NewLine)
                                       (getQPolyString dn) (getQPolyString dd)
                                       
    printfn ""
    printfn "Now testing algebraic extensions of Q:"
    printfn ""
    let minimalPoly = [|N 2I; N 0I; N 1I |]
    printfn "let K = Q[x]/(%s)" (getQPolyString minimalPoly)
    printfn ""
    let algExOfQ = AlgebraicExtensionField(rationalField, minimalPoly)
    let divTest = algExOfQ.Divide algExOfQ.One [| N 1I; N 1I |]

    printfn "Then 1/(%s) = %s" (getQPolyString [| N 1I; N 1I |]) (getQPolyString divTest.Value)
    
    printfn ""
    printfn "Now testing algebraic extensions of Q(x):"    
    printfn ""

    let minimalPoly = [| N [| N -1I; N -1I|]; rationalFunctionsOfX.Zero; rationalFunctionsOfX.One |]
    let algExOfQx = AlgebraicExtensionDifferentialField(ratFunDifField, minimalPoly)

    printfn "let K = Q(x)[t]/(t^2-x-1)"
    
    let dr = (algExOfQx.Divide algExOfQx.One [| N [| N 1I |]; N [| N 1I |] |]).Value

    let t = [| rationalFunctionsOfX.Zero; rationalFunctionsOfX.One |]
    let tPlusOne = algExOfQx.Add t algExOfQx.One
    let tPlusOneSquared = algExOfQx.Multiply tPlusOne tPlusOne
    let twoTimesTPOS = algExOfQx.Add tPlusOneSquared tPlusOneSquared
    let den = algExOfQx.Multiply twoTimesTPOS t 
    let checkDer = (algExOfQx.Divide (algExOfQx.Negate algExOfQx.One) den).Value

    let derivative = algExOfQx.Derive dr
    let dt = algExOfQx.Derive t
    
    let getAlgFunString (af: (Fraction<Fraction<bigint>[]>[])) = "(" + getQRatFunString af.[0]  + ")+(" + getQRatFunString af.[1]  + ")*sqrt(1+x)" 

    printfn  "D[%s] = %s" (getAlgFunString t) (getAlgFunString dt)

    printfn "D[%s] = %s + (%s)*sqrt(x + 1)" (getAlgFunString dr) (getQRatFunString derivative.[0]) (getQRatFunString derivative.[1])
      
    

    0 // This is fine ©