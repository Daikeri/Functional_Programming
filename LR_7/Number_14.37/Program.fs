open System
(*
1.37 Дан целочисленный массив. Вывести индексы элементов, которые
меньше своего левого соседа, и количество таких чисел.
*) 

let leftbig (list:int list) =
    let rec prom list acc_list =
        match list with
        |[] -> acc_list
        |head::tail ->
            let new_acc_list = if tail <> [] then (if snd tail.Head < snd head then acc_list @  [fst (tail.Head)] else acc_list) else acc_list
            prom tail new_acc_list
    prom (List.indexed list) []

[<EntryPoint>]
let main argv =
    let l = [10; 9; 8; 7; 12; 54; 1; 6] // 4: 9 8 7 1
    leftbig l |> printfn "%A"
    0
