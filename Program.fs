// Basic abstract algebra types




[<EntryPoint>]
let main argv =
    let intRing = MathTypes.IntegerUnivariatePolyRing();

    let p1 = MathTypes.IntegerUnivariatePolyRing().Multiply [| 10I; 3I |] [| 2I; 1I; 5I |]
    
    printfn "%s" (intRing.GetString([| 10I; 3I |]))
    printfn "  times  "
    printfn "%s" (intRing.GetString([| 2I; 1I; 5I |]))
    printfn "  equals %s%s" Environment.NewLine (intRing.GetString(p1))
    0 // return an integer exit code
