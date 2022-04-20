open System
let leftbigger list =
    let rec prom list index lacc =
        match list with
        |[] -> lacc
        |head::tail ->
            let nlacc = if tail <> [] then (if tail.Head < head then (index::lacc) else lacc) else lacc
            let newind= index+1
            prom tail newind nlacc
    prom list 1 []
           
let rec printl =
    function
    |[] -> ()
    |head::tail ->
        printfn "%O" head
        printl tail

[<EntryPoint>]
let main argv =
    let l = [10; 9; 8; 7; 12; 54; 1] // 4
    let res = leftbigger l
    printfn "%d" res.Length
    printfn "-"
    printl res
    0