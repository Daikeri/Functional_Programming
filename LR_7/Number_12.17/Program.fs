open System

let Max list el = 
    List.max list = el

let Min list el = 
    List.min list = el

let exc_min_max (list: int list) =
    let rec prom list acc_list mi_in ma_in min max index =
        match list with
        |[] -> acc_list
        |head::tail -> 
            let el = if index = mi_in then min elif index = ma_in then max else head
            let new_acc_list= acc_list@[el]
            let new_index = index+1
            prom tail new_acc_list mi_in ma_in min max new_index
    let mi_in = list |> List.findIndex (fun x -> Max list x)
    let ma_in = list |> List.findIndex (fun y -> Min list y)
    prom list [] mi_in ma_in (List.min list) (List.max list) 0
    
[<EntryPoint>]
let main argv =
    let l = [1;20;3;4;-7;10;4]
    exc_min_max l |> printfn "%A"
    0
