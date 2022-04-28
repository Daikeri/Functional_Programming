open System

let shift_right list =
    list |> List.permute (fun x ->
    if x > ((List.length list-1)-2) then (x+1)%(List.length list-1)
    else x+2) |> printfn "%A"

[<EntryPoint>]
let main argv =

    let l = [1..4]
    let k = [1;7;3;45;-1;234;56;-99]
    shift_right l
    shift_right k
    0
