open System
let Div value func init =
    let rec DivR value func init acc =
        if acc = 0 then init
        else
            let prom= if value % acc=0 then func init acc else init
            let newd=acc-1
            DivR value func prom newd
    DivR value func init value

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

// с функциями предикатами 

let DivPred value predic func init = 
    let Pfunc init divisor = 
        if predic divisor then func init divisor else init 
    Div value Pfunc init 

let PrimePred value predic func init = 
    let Pfunc init prom =
        if predic prom then func init prom else init 
    PrimeNumber value Pfunc init
    

    
[<EntryPoint>]
let main argv =
    let x = Console.ReadLine() |> Int32.Parse
    let result1 = DivPred x (fun x -> x < 20) (fun x y -> x + y) 0
    let result2 = Div x (fun x y -> x + y) 0
    result1 |> printfn "predic:%d"
    result2 |> printfn "not predic:%d"
    
    let result3 = PrimePred x (fun x -> x < 20) (fun x y -> x + y) 0
    let result4 = PrimeNumber x (fun x y -> x + y) 0
    result3 |> printfn "predic:%d"
    result4 |> printfn "not predic:%d"
    0
