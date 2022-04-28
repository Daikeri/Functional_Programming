(*
1.47. Для введенного списка положительных чисел построить список всех
положительных делителей элементов списка без повторений
*)

let find_div (number:int) = 
    let rec prom (number:int) acc list =
        if acc = 0 then list 
        else 
            let new_list = if number % acc = 0 then list@[acc] else list
            let new_acc = acc-1
            prom number new_acc new_list
    prom number number [] 

let cool list =
   ([],list) ||> List.fold(fun acc x -> acc@find_div x) |> List.distinct

[<EntryPoint>]
let main argv =
    let l = [1;2;3;4;5;18]
    cool l |> printfn "%A"
    0 
