open System
(*1.17 Дан целочисленный массив. Необходимо поменять местами
минимальный и максимальный элементы массива.*)
let findmax list =
    let rec prom list max index ma_index =
        match list with
        |[] -> (max,ma_index)
        |head::tail ->
            let nmax = if head > max then head else max
            let nma_index = if nmax <> max then index else ma_index
            let nindex=index+1
            prom tail nmax nindex nma_index 
    prom list list.Head 0 0

let findmin list =
    let rec prom list min index mi_index =
        match list with
        |[] -> (min, mi_index)
        |head::tail ->
            let nmin = if head < min then head else min
            let nmi_index = if nmin <> min then index else mi_index
            let nindex=index+1
            prom tail nmin nindex nmi_index 
    prom list list.Head 0 0

let replacement list =  // max -> min
    let rec prom list index max min ma_index mi_index acc_list =
        match list with
        |[] -> acc_list
        |head::tail ->
           let el = 
            if index = mi_index then max 
            else if index = ma_index then min
            else head
           let new_list = acc_list @ [el]
           let nindex = index + 1
           prom tail nindex  max min ma_index mi_index new_list
    let min_result = findmin list
    let max_result = findmax list
    prom list 0 (fst max_result) (fst min_result) (snd max_result) (snd min_result) []

let rec printl = function
| [] -> ()
| headch::tails -> 
    printf "%O " headch 
    printl tails

[<EntryPoint>]
let main argv =
    let l = [-9; 1; 2; 3; 33;13; 83; -7]
    l |> printl
    printfn " "
    replacement l |> printl 
    0 