open System
 
let SimpleDiv value =  // проверка на простое число
    let rec Prom value acc quan =
        if quan > 2 then false
        else 
            if acc > 0 then 
                let newquan = if value % acc = 0 then quan+1 else quan
                let newacc = acc-1 
                Prom value newacc newquan 
            else true 
    Prom value value 0
 
let SumDiv value = 
    let rec Prom value acc sum =
        if acc = 0 then sum
        else 
            let newsum = if value % acc = 0 && SimpleDiv acc then (fun x y -> x + y) sum acc else sum
            let newacc = acc-1
            Prom value newacc newsum
    Prom value value 0
            
[<EntryPoint>]
let main argv =
    let x = Console.ReadLine() |> Int32.Parse
    let result = SumDiv x
    result |> printfn "%d"
    0 