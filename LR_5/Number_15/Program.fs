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
    

[<EntryPoint>]
let main argv =
    printfn "input the number"
    let x = Console.ReadLine() |> Int32.Parse
    let result = PrimeNumber x (fun x y -> x + y) 0
    result |> printfn "%d"
    0 
    