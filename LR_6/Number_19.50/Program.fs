open System
(*
1.50. Для двух введенных списков L1 и L2 построить новый список,
состоящий из элементов, встречающихся только в одном из этих списков и
не повторяющихся в них.
*)
let quan list number = // подсчет количества вхождений
    let rec prom list number qu =
        match list with
        |[] -> qu
        |head::tail ->
            let new_qu = if head = number then qu+1 else qu
            prom tail number new_qu
    prom list number 0

let pred value list =
    let rec prom value list t_f =
        match list with 
        |[] -> t_f
        |head::tail ->
            let n_t_f = if value = head then false else t_f
            prom value tail n_t_f
    prom value list true
    
let arrange list = // формирует список кортежей вида (a,b), где a - число, b - кол-во вхождений числа в исходном списке
    let rec prom list acc_list ignor_list =
        match list with
        |[] -> acc_list
        |head::tail ->
            let result = pred head ignor_list 
            let xi = quan list head
            let new_acc_list = if result <> false then acc_list@[(head,xi)] else acc_list
            let new_ignor_list = ignor_list@[head]
            prom tail new_acc_list new_ignor_list
    prom list [] []

let predic list number =
    let rec prom list number t_f =
        match list with 
        |[] -> t_f
        |head::tail ->
            let new_t_f = if (fst head = fst number) then true else t_f
            prom tail number new_t_f
    prom list number false

let general_alp list1 list2 = // выявляем элементы, встр. в обоих списках 
    let rec prom list1 list2 alp =
        match list2 with 
        |[] -> alp
        |h2::t2 -> 
            let new_alp = if (predic list1 h2) then alp@[fst h2] else alp
            prom list1 t2 new_alp
    prom list1 list2 []

let flopchik number alp =
    let rec prom number alp t_f =
        match alp with 
        |[] -> t_f
        |head::tail ->
            let new_t_f = if number = head then false else t_f
            prom number tail new_t_f
    prom number alp true    
  
let result list1 list2 alp =
    let rec prom list1 list2 alp acc_list =
        match list1,list2 with
        |[],[] -> acc_list
        |h1::t1,[] ->
            let one = flopchik (fst h1) alp
            let new_acc_list = if one && (snd h1 = 1) then acc_list@[fst h1] else acc_list
            prom t1 [] alp new_acc_list
        |[],h2::t2 -> 
            let two = flopchik (fst h2) alp
            let new_acc_list = if two && (snd h2 = 1) then acc_list@[fst h2] else acc_list
            prom [] t2 alp new_acc_list
        |h1::t1,h2::t2 ->
            let one = flopchik (fst h1) alp
            let two = flopchik (fst h2) alp
            let new_acc_list = if one && (snd h1 = 1) then acc_list@[fst h1] else (if two && (snd h2 = 1) then acc_list @[fst h2] else acc_list)
            prom t1 t2 alp new_acc_list
    prom list1 list2 alp 
    
let rec print_l =
    function
    | [] -> ()
    | headch::tails -> 
        printf "%O " headch
        print_l tails      

let arrange_two list_one list_two = 
    match list_one,list_one with
    |[],[] -> printfn "none"
    |_,_ -> 
       let one_arr = arrange list_one 
       let two_arr = arrange list_two
       let prom = general_alp one_arr two_arr
       (result one_arr two_arr prom) [] |> print_l      
        
[<EntryPoint>]
let main argv =
    let l = [5;5;5;6;7;1;6;5;6;5;5]
    let k = [5;5;5;6;5;5]

    let i = [4;3;4;3;4;3]
    let j = [4;4;4;3;3;3;4;3;7;2;7]
    arrange_two l k
    printfn ""
    arrange_two i j
    0
     