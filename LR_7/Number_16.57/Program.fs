open System

let more_than_2 list = 
    list |> List.indexed |> List.filter (fun x ->
    (snd x) > ((0,(list |> List.take (fst x)) ) ||> List.fold (fun acc k -> acc + k)) && snd x <> list.Head ) |> printfn "%A" 

[<EntryPoint>]
let main argv =
    let l = [1;2;3;10;5;5;70] 
    let k = [100;100;100;400;1000;1000;-1000;4000;-99;-1;34;55000]
    more_than_2 l
    more_than_2 k
    0
