open System
open FParsec

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" (result |> List.reduce(fun a b -> a * b ))
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg
 
let str s = pstring s
[<EntryPoint>]
let main argv =
    let ws = spaces
    let str_ws s = pstring s .>> ws
    let float_ws = pfloat .>> ws
    let numberList = str_ws "(" >>. sepBy float_ws (str_ws ",") .>> str_ws ")"
    let numberListFile = ws >>. numberList .>> eof
    test numberListFile "(1,
                             2,  
                                3,  
                                    4,
                                        5)"
    0 
 