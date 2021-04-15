// Basic abstract algebra types
open MathTypes

[<EntryPoint>]
let main argv =    
    let rationalNumberField = MathTypes.RationalNumberField()
    let domainOfPolynomialsOverQ = MathTypes.UnivariatePolyOverFieldDomain(rationalNumberField) //Q[x]

    let r1 = [| 2Q; 1Q; 5Q |]
    let r2 = [| 10Q/7Q; 1Q |]
    let R1DivRemR2 = domainOfPolynomialsOverQ.DivRem r1 r2

    let getQPolyString = GetPolyString rationalNumberField rationalNumberField.GetString 

    let (div, rem) = R1DivRemR2.Value

    printfn "%s = (%s)*(%s) + %s" (getQPolyString r1) (getQPolyString r2) (getQPolyString div) (getQPolyString rem)
    
    0 // return an integer exit code
