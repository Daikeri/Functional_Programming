open System

let quan_rus s =
    s |> String.filter (fun k -> k >= 'А' && k <= 'я') |> String.length |> printf "%d"

[<EntryPoint>]
let main argv =
    let inp = Console.ReadLine()
    quan_rus inp
    0