open System
let SumNumber value = 
    let rec Prom value sum =
        if value=0 then sum
        else 
            let newsum = sum + value % 10
            let newval = value / 10
            Prom newval newsum
    Prom value 0

let SumDiv value =
    let rec Prom value acc init = 
        if acc = 0 then init
        else
            let newinit =
                if value % acc = 0 && SumNumber value > SumNumber acc then (fun x y -> x * y) acc init
                else init
            let newacc= acc-1
            Prom value newacc newinit
    Prom value value 1
                           
[<EntryPoint>]
let main argv =
    let x = Console.ReadLine() |> Int32.Parse
    let result = SumDiv x
    result |> printfn"%d"
    0
