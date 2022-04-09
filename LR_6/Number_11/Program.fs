open System

let rec ReadLi n = 
    if n=0 then []
    else
        let headch = Console.ReadLine() |> Convert.ToInt32
        let tails = ReadLi (n-1)
        headch::tails

let rec PrintLi = function
    | [] -> ()
    | headch::tails -> 
        printfn "%O" headch
        PrintLi tails

let Three L func =
    let rec ToThreeInter L func curL =
        match L with
        | [] -> curL
        | el0::t ->
            let el1 = if t <> [] then t.Head else 1
            let el2 = if t <> [] then (if t.Tail <> [] then t.Tail.Head else 1) else 1
            let newel = func el0 el1 el2
            let newL = curL @ [newel]
            let shiftL = if t <> [] then (if t.Tail <> [] then t.Tail.Tail else []) else []
            ToThreeInter shiftL func newL
    ToThreeInter L func []

[<EntryPoint>]
let main argv =
    printfn "Количество элементов и список:"
    let L = ReadLi (Console.ReadLine() |> Convert.ToInt32)
    let new_L = Three L (fun x y z -> x + y + z)
    printfn "Результат: "
    PrintLi new_L
    0