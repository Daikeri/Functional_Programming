open System

let rec MultUp y =
    if y=0 then 1
    else (y%10) * MultUp(y/10)

let rec MinChUp y =
    if y < 10 then y
    else min (y % 10) (MinChUp (y / 10))

let rec MaxChUp y =
    if y < 10 then y
    else max (y % 10) (MinChUp (y / 10))

let MultDown y =
    let rec MultDownR y acc =
        if y=0 then acc
        else MultDownR (y/10) (acc*(y%10))
    MultDownR y 1

let MinChDown y =
    let rec MinChDownR y min =
        if y = 0 then min
        else
            let newMin = if y % 10 < min then y % 10 else min
            let newy = y / 10
            MinChDownR newy newMin
    MinChDownR y (y % 10)

let MaxChDown y =
    let rec MaxChDownR y max =
        if y = 0 then max
        else
            let newMax = if y % 10 > max then y % 10 else max
            let newy = y / 10
            MaxChDownR newy newMax
    MaxChDownR y (y % 10)


[<EntryPoint>]
let main argv =
    let x=Console.ReadLine()|> Int32.Parse
    
    MultUp x |> printfn "вверх произведение: %d"
    MinChUp x |> printfn "вверх мин цифра: %d" 
    MaxChUp x |> printfn "вверх макс цифра : %d"
    MultDown x |> printfn "вниз произведение: %d"
    MinChDown x |> printfn "вниз мин цифра %d"
    MaxChDown x |> printfn "вниз макс цифра %d"
    0 
