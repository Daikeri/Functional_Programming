open System
(*1.34 Дан целочисленный массив и отрезок a..b. Необходимо найти
элементы, значение которых принадлежит этому отрезку.*)
let segment list (a,b) =
    let rec prom list (a,b) acc =
        match list with
        |[] -> acc
        |head::tail ->
            let nacc = if head >= a && head <= b then head::acc else acc
            prom tail (a,b) nacc
    prom list (a,b) []

let rec printl =
    function
    |[] -> ()
    |head::tail ->
        printfn "%O" head
        printl tail

[<EntryPoint>]
let main argv =
    let l = [1;2;3;12;17;80;-8;20;10;5]
    let ab = (10,20)
    segment l ab |> printl 
    0 
