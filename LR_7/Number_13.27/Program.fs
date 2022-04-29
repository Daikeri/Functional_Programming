open System

let shift_left list =
    list |> List.permute (fun x ->
    if x=0 then x+(List.length list-1)
    else x-1) |> printfn "%A"

[<EntryPoint>]
let main argv =
    
    let l = [1..4]
    let k = [1;7;3;45;-1;234;56;-99]
    shift_left l
    shift_left k
    0 
