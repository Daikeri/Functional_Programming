open System
let even list =
   let rec prom list quan =
       match list with
       |[] -> quan
       |head::tail -> 
           let newq = if head % 2 = 0 then quan+1 else quan
           prom tail newq
   prom list 0
let f31 list =
    match list with
    |[] -> 0
    |_ -> even list

[<EntryPoint>]
    let main argv =
        let l = [1..10]
        let result = f31 l
        printfn "%O" result // 5
        0
