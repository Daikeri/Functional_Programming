open System
open System.Text.RegularExpressions

let bebra (curr_str:string) =
    let vowel_regex = @"([АЕЁИОУЫЭЮЯаеёиоуыэюя])"
    let consonant_regex = @"([БВГДЖЗЙКЛМНПРСТФХЦЧШЩбвгджзйклмнпрстфхцчшщ])"
    let general =
        curr_str.Split(' ') |> Array.map(fun el -> 
            let start = 
                if Regex.IsMatch(string el.[0],vowel_regex) then "vowel" 
                elif (Regex.IsMatch(string el.[0],consonant_regex)) then "consonant"
                else "xexe"
            let rec prom (list:char list) (first:string) (save:string) =
                match list,first with
                |[],_ -> first
        
                |head::tail,"vowel" ->
                    let n_save = 
                        if save = "vowel" && Regex.IsMatch(string head,consonant_regex) then "consonant"
                        elif (save = "consonant" && Regex.IsMatch(string head,vowel_regex)) then "vowel"
                        else "none"
                    let n_tail = if n_save = "none" then [] else tail
                    let n_first = if n_save = "none" then "none" else first
                    prom n_tail n_first n_save
        
                |head::tail,"consonant" ->
                    let n_save = 
                        if save = "consonant" && Regex.IsMatch(string head,vowel_regex) then "vowel"
                        elif (save = "vowel" && Regex.IsMatch(string head,consonant_regex)) then "consonant"
                        else "none"
                    let n_tail = if n_save = "none" then [] else tail
                    let n_first = if n_save = "none" then "none" else first
                    prom n_tail n_first n_save
            if el.Length % 2 = 0 then prom ((el.ToCharArray()|>Array.toList).Tail) start start else "none" )
    ((0,general)||>Array.fold(fun acc v -> if v = "vowel" then acc + 1 else acc),(0,general)||>Array.fold(fun acc c -> if c = "consonant" then acc + 1 else acc))
    ||> (fun x y -> Math.Abs(x-y)) 

let general list =
    list |> List.map(fun x ->(x, bebra x)) |> List.sortBy(fun i -> snd i) |> printfn"%A"

[<EntryPoint>]
let main argv =
    let gen_list = ["аб ук як эт са";"выхожу лицо";"аб ук як эт са аб ук як этэт са"]
    gen_list |> general
    0
