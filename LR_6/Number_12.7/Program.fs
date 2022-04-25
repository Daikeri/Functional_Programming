open System 
let leng list = 
    let rec loop list length =
        match list with
        |head::tail ->
            let nextLength = length + 1
            loop tail nextLength
        |[] -> length
    loop list 0
    
let shift list =
    let rec prom list acc_list listlen = 
        match list with 
        |[] -> acc_list
        |head::tail ->
            let new_acc_list = if listlen > 0 then (acc_list @ [head]) else (if listlen <> -1 then list@acc_list else acc_list)
            let new_listlen= listlen - 1
            prom tail new_acc_list new_listlen
    prom list [] (leng list - 2)
let rec print_l =
    function
    | [] -> ()
    | headch::tails -> 
        printf "%O " headch 
        print_l tails 

[<EntryPoint>]
let main argv =
    let l = [1;2;3;4;5;6]
    shift l |> print_l
    0
