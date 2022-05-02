open System
let find_div (number:int) = 
    let rec prom (number:int) acc list =
        if acc = 0 then list 
        else 
            let new_list = if number % acc = 0 then list@[acc] else list
            let new_acc = acc-1
            prom number new_acc new_list
    prom number number [] 

let predic (origin:int list) (listDiv:int list) = 
    let average = List.sum origin / List.length origin
    let prom = origin |> List.filter(fun g -> g < average )
    listDiv |> List.filter (fun el ->
    (origin |> List.indexed |> List.exists(fun k -> (snd k) % el = 0 && (fst k) % 2 = 0)) &&
    (prom |> List.forall(fun k -> k % el <> 0)) )  |> List.sum

let general (list:int list) =
    let rec prom list (copy:int list) acc =
        match copy with 
        |[] -> acc
        |head::tail ->
            let new_acc = acc @ [head |> find_div|> predic list ]
            prom list tail new_acc
    prom list list []

[<EntryPoint>]
let main argv =
    let l = [1;2;3;10;5;5;70] 
    let k = [18;15;24;11;36;7;7;72]
    let g = [18;15;24;11;36;7]
    let h = [18;15]
    general g |> List.sort |> printfn "%A"
    general h |> List.sort |> printfn "%A"
    general k |> List.sort |> printfn "%A"
    general l |> List.sort |> printfn "%A"
    0