open System

let Div value func init =
    let rec DivR value func init acc =
        if acc = 0 then init
        else
            let prom= if value%acc=0 then func acc init else init
            let newd=acc-1
            DivR value func prom newd
    DivR value func init value
    
    
[<EntryPoint>]
  let main argv =
    printfn "intput the number"
    let value = Console.ReadLine() |> Int32.Parse
    let result = Div value (fun z k -> z+k) 0
    result |> printfn "%d"
    0

     