open System
(*
Дана строка. Необходимо проверить образуют ли строчные
символы латиницы палиндром.
*)
let palindrom (strr:string) =
    strr.Split(' ') |> Array.filter (fun x -> x = (String.Concat<char>(x.ToCharArray() |> Array.rev))) |> printfn "%A"
    
[<EntryPoint>]
let main argv =
    let str = "gig kayak horse level madam input qwerty fish "
    palindrom str
    0
