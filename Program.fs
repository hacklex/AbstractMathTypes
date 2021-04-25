// Basic abstract algebra types
open AbstractAlgebra 
open Fractions 
open Polynomials
open FieldExtensions
open DifferentialAlgebra

[<EntryPoint>]
let main argv =     
 
    // seriously, my little CAS can't be this cute.
    printfn "Initializing CuteCAS, all systems green\n"
    printfn "Constructing Q..."

    let intSign n = if n<0 then -1 else 1
    let integers = Construct.EuclideanDomain(Construct.CommutativeGroup(0, CommutativeBinaryOp(fun a b -> a+b), UnaryOp(fun a -> -a), EqualityChecker(fun a b -> a=b)),
                                             Construct.CommutativeMonoid(1, CommutativeBinaryOp(fun a b -> a*b), EqualityChecker(fun a b -> a=b)),
                                             Some(fun n -> n),
                                             (fun n -> (intSign n), n*(intSign n)),
                                             (fun a b -> if b=0 then None else Some(a/b, a%b)),
                                             (fun a -> if a=0 then None else Some(bigint(if a<0 then -a else a))),
                                             (fun n -> n.ToString()))    
    let rationals = QuotientField(integers)
    let rationalArithmeticResult = (rationals.Divide (rationals.Add (N 15) (N 25)) (N 16)).Value

    printfn "Done. Testing (15+25)/16 = %s" (rationals.GetString rationalArithmeticResult)
    printfn "\nNow proceeding to construct Q[x]...\n"

    let qPolys = Construct.UnivariatePolynomialRing(rationals, "x")

    let poly1 = [| N 1; N 0; N 1 |]
    let poly2 = [| N 2; N 1|]
    let polyMultiplyResult = qPolys.Multiply poly1 poly2

    printfn "Done. Testing (%s)*(%s) = %s" (qPolys.GetString poly1) (qPolys.GetString poly2) (qPolys.GetString polyMultiplyResult)
    printfn "\nNow constructing the differential field for Q(x) from Q with zero derivation...\n"

    // When we develop Transcendental Extensions, we will not need even this.
    // We instead will just define Q with zero derivation, and Q(x) as a transcendental
    // extension of Q with x'=1.
    let polyDerive (ring: 'TCoefficient Ring) (poly: 'TCoefficient[]) = 
      if poly.Length < 2 then [||] else
          let subCoef = Array.sub poly 1 (poly.Length-1)
          for i=0 to (subCoef.Length-1) do 
            let oldCoef = subCoef.[i]
            Array.set subCoef i (ring.Multiply oldCoef (ring.IntegerConstant (i+1)))      
          subCoef

    let trivialDiffFieldOnQ = DifferentialField(rationals, fun _ -> rationals.Zero)
    let ratFunAsExt = DifferentialTranscendentExtensionField(trivialDiffFieldOnQ, "x", [| rationals.One |])

    let derTest = ratFunAsExt.Derive (N polyMultiplyResult)
    let polyMulResult = N polyMultiplyResult

    let rationalFunctionsOfX = ratFunAsExt.Field
    let oneOverDerTest = (rationalFunctionsOfX.Invert derTest).Value
    let sum = rationalFunctionsOfX.Add oneOverDerTest (N polyMultiplyResult)
    let gs = rationalFunctionsOfX.GetString // we need a short name for that

    printfn "\nDone. Testing %s + %s = %s\n" (gs oneOverDerTest) (gs (N polyMultiplyResult)) (gs sum)
    printfn "Now constructing algebraic extension Q(x)[t]/(t^2-x-1)..."

    let minusXMinusOne = N [| N -1; N -1 |] // -x-1
    let algExt = AlgebraicExtensionField(rationalFunctionsOfX, [| minusXMinusOne; rationalFunctionsOfX.Zero; rationalFunctionsOfX.One |], "Sqrt(x+1)")    
    let expr = (algExt.Divide algExt.One [| rationalFunctionsOfX.One; rationalFunctionsOfX.One |]).Value

    printfn "\nDone. Testing 1/(1+Sqrt(x+1)) = %s\n" (algExt.GetString expr)
    printfn "Now constructing the differential field from Q(x)[t]..."

    let difQ = trivialDiffFieldOnQ   
    let difExt = DifferentialAlgebraicExtensionField(ratFunAsExt, algExt.MinimalPoly, "sqrt(x+1)")    
    let dExpr = difExt.Derive expr

    printfn "\nDone. Testing D[%s] = %s\n" (algExt.GetString expr) (algExt.GetString dExpr)
    printfn "Now constructing the differential field from Q(x)(exp(x))..."
    

    let exponentialExtField = DifferentialTranscendentExtensionField(ratFunAsExt, "exp(x)", [| rationalFunctionsOfX.Zero; rationalFunctionsOfX.One |])
    let eField = exponentialExtField.Field
    let expExpression = N [| rationalFunctionsOfX.One; rationalFunctionsOfX.Zero; rationalFunctionsOfX.One |]
    let der = exponentialExtField.Derive expExpression

    printfn "\nDone. Testing D[%s] = %s\n" (eField.GetString expExpression) (eField.GetString der)

    0 // This is fine ©