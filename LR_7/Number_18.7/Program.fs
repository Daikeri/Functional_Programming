open System

let input =
    Console.WriteLine("1.Ручной ввод\n2.Готовый набор")
    let inp = Convert.ToInt32(Console.ReadLine())
    match inp with 
    |1 -> 
        Console.WriteLine("Размерность 1-го массива:")
        let n1 = Convert.ToInt32(Console.ReadLine())
        let arrOne = [|for i in 0..n1 -> Convert.ToInt32(Console.ReadLine())|]

        Console.WriteLine("Размерность 2-го массива:")
        let n2 = Convert.ToInt32(Console.ReadLine())
        let arrTwo = [|for i in 0..n2 -> Convert.ToInt32(Console.ReadLine())|]

        let arrThree =
            arrOne |> Array.distinct |> Array.filter (fun x -> arrTwo |> Array.contains x)

        arrThree |> Array.toList |> printfn "%A"

    | 2 -> 
        let arrOne = [|1;1;1;2;2;3;8;9;17;20;20;31;451|]
        let arrTwo = [|7;7;8;9;22;31;32;34;34;67|]

        let arrThree =
            arrOne |> Array.distinct |> Array.filter (fun x -> arrTwo |> Array.contains x)

        arrThree |> Array.toList |> printfn "%A"
       
    |_ -> printfn "none"

[<EntryPoint>]
let main argv =
    input
    0 
