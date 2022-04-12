open System
let rec FindGCD one two =
    if one=0 || two = 0 then one + two
    else 
        let none =
            if one > two then one % two
            else one 
        let ntwo =
            if one <= two then two % one
            else two
        FindGCD none ntwo
        
let PrimeNumber value func init =
    let rec PrimeNumberR value func init acc =
        if acc <= 0 then init
        else 
            let ninit = if FindGCD value acc = 1 then func value init else init
            let nacc = acc - 1
            PrimeNumberR value func ninit nacc
    PrimeNumberR value func init value
    
let Euler q = 
    PrimeNumber q (fun x y -> x + 1) 0

[<EntryPoint>]
let main argv =
    printfn "input Euler Number"
    let x = Console.ReadLine() |> Int32.Parse
    let resultP = PrimeNumber x (fun x y -> x + y) 0
    let resultEu = Euler x
    resultP |> printfn "%d"
    resultEu |> printfn"%d"
    0 
