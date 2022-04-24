open System
(*1.11 Дан целочисленный массив, в котором лишь один элемент
отличается от остальных. Необходимо найти значение этого элемента.*)
let quantity number list =
    let rec prom number list quan =
        match list with 
        |[] -> quan
        |head::tail -> 
            let new_quan = if head = number then quan+1 else quan
            prom number tail new_quan
    prom number list 0

let find_func list =
    let rec prom list el primal_list=
        match list with 
        |[] -> el
        |head::tail ->
            let result = quantity head primal_list 
            let new_el = if result = 1 then head else el 
            let new_tail = if result = 1 then [] else tail
            prom new_tail new_el primal_list
    prom list list.Head list

[<EntryPoint>]
let main argv =
    let l = [5;6;6;6;6;6;6;6]
    let k = [6;6;6;6;5;6;6;6]
    let g = [6;6;6;6;6;6;6;5]
    find_func l |> printfn "%O"
    find_func k |> printfn "%O"
    find_func g |> printfn "%O"
    0
