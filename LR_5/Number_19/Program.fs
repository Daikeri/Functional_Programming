open System

// 18.1
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
 // 18.2
let UnevenQuantity value =
    let rec Prom value quan = 
        if value = 0 then quan
        else
            let promik = value % 10
            let newquan =
                if promik % 2 = 1 && (fun x -> x > 3) promik then quan+1
                else quan
            let newval = value / 10
            Prom newval newquan
    Prom value 0
 // 18.3
let SumNumber value = 
    let rec Prom value sum =
        if value=0 then sum
        else 
            let newsum = sum + value % 10
            let newval = value / 10
            Prom newval newsum
    Prom value 0

let MultDiv value =
    let rec Prom value acc init = 
        if acc = 0 then init
        else
            let newinit =
                if value % acc = 0 && SumNumber value > SumNumber acc then (fun x y -> x * y) acc init
                else init
            let newacc= acc-1
            Prom value newacc newinit
    Prom value value 1

[<EntryPoint>]
let main argv =
   Console.WriteLine("data for 18.1")
   Console.ReadLine() |> Convert.ToInt32 |> SumDiv |> printfn "%d"

   Console.WriteLine("data for 18.2")
   Console.ReadLine() |> Convert.ToInt32 |> UnevenQuantity |> printfn "%d"

   Console.WriteLine("data for 18.3")
   Console.ReadLine() |> Convert.ToInt32 |> MultDiv |> printfn "%d"
   0
