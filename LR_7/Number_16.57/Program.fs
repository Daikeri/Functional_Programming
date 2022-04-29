open System

let more_than list = 
    list |> List.filter (fun x ->
    x > (list |> List.take(list |> List.findIndex (fun y -> y = x)) |> List.sum) && x <> list.Head ) |> printfn "%A" 

[<EntryPoint>]
let main argv =
    let l = [1;2;3;10;5;5;70] 
    let k = [100;100;100;400;2000;1000;-1000;4000;-99;-1;34;55000]
    more_than l
    more_than k
    0
