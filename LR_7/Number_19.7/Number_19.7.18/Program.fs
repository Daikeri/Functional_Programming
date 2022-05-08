open System
open System.Text.RegularExpressions

let find_date (strr:string) =
    let regex = @"^(0[1-9]|[12][0-9]|3[01])[.](0[1-9]|1[012])[.](19|20)\d\d$"
    strr.Split(' ') |> Array.filter(fun k -> Regex.IsMatch(k,regex)) |> printfn "%A"

[<EntryPoint>]
let main argv =
    let str = @"In 07.12.2003 07\12\2003 45.54.2012 78|56|1222 23.01.2024"
    find_date str
    0
