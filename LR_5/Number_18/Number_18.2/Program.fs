open System
(*Метод 1. Найти сумму простых делителей числа. 
Метод 2. Найти количество нечетных цифр числа, больших 3.
Метод 3. Найти прозведение таких делителей числа, сумма цифр
которых меньше, чем сумма цифр исходного числа.*)
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


[<EntryPoint>]
let main argv =
    let x = Console.ReadLine() |> Int32.Parse
    let result = UnevenQuantity x
    result |> printfn "%d"
    0
