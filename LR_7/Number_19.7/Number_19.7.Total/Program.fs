open System
open System.Text.RegularExpressions

let quan_rus s =
    s |> String.filter (fun k -> k >= 'А' && k <= 'я') |> String.length |> printf "%d"

let palindrom (strr:string) =
    strr.Split(' ') |> Array.filter (fun x -> x = (String.Concat<char>(x.ToCharArray() |> Array.rev))) |> printfn "%A"

let find_date (strr:string) =
    let regex = @"^(0[1-9]|[12][0-9]|3[01])[.](0[1-9]|1[012])[.](19|20)\d\d$"
    strr.Split(' ') |> Array.filter(fun k -> Regex.IsMatch(k,regex)) |> printfn "%A"

let general () =
    Console.WriteLine("Введите цифру от 1 до 3:")
    let input = Console.ReadLine()
    match input with
    |"1" -> 
        Console.WriteLine("Задача 19.1\nВведите строку, состоящую из символов русского алфавита:")
        Console.ReadLine() |> quan_rus
    |"2" ->
        Console.WriteLine("Задача 19.9\nНеобходимо проверить образуют ли строчные символы латиницы палиндром.")
        "gig kayak horse level madam input qwerty fish " |> palindrom
    |"3" -> 
        Console.WriteLine("Задача 19.18\nНайти в тексте даты формата «день.месяц.год»")
        @"In 07.12.2003 07\12\2003 45.54.2012 78|56|1222 23.01.2024" |> find_date
    |_ -> printfn "чето не то"

        
[<EntryPoint>]
let main argv =
   general()
   0
