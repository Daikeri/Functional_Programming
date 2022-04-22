(*1.55. Для введенного списка построить новый список, который получен из 
начального упорядочиванием по количеству встречаемости элемента,
То есть из списка [5,6,2,2,3,3,3,5,5,5] необходимо получить список 
[5,5,5,5,3,3,3,2,2,6].*)

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
            let new_acc_list = if result <> false then acc_list@[(head,xi)] else acc_list@[]
            let new_ignor_list = ignor_list@[head]
            prom tail new_acc_list new_ignor_list
    prom list [] []

let pred2 value list =
    let rec prom value list t_f =
        match list with 
        |[] -> t_f
        |head::tail ->
            let n_t_f = if fst value = fst head then false else t_f
            prom value tail n_t_f
    prom value list true

let find_max_quan list ig = // поиск числа с максимальным количеством вхождений в уже созданном списке кортежей 
    let rec prom list max ig =
        match list with 
        |[] -> max
        |head::tail -> 
            let n_max = if (snd head) > (snd max) && (pred2 head ig) then head else max 
            prom tail n_max ig 
    prom list list.Head ig

let create number = 
    let rec prom value quan acc_list =
        if quan > 0 then 
            let new_acc_list = value::acc_list
            let new_quan = quan - 1
            prom value new_quan new_acc_list
        else acc_list
    prom (fst number) (snd number) []

let concatenation_all list = 
    let rec prom list acc_list ignore_list=
        match list with 
        |[] -> acc_list
        |head::tail -> 
            let r = find_max_quan list ignore_list
            let new_acc_list = if (pred2 r ignore_list) then acc_list @ (r |> create) else acc_list
            let new_ignore_list = ignore_list @ [r]
            let new_tail = if (pred2 head new_ignore_list) then list else tail 
            prom new_tail new_acc_list new_ignore_list
    prom list [] []

let rec print_l =
    function
    | [] -> ()
    | headch::tails -> 
        printf "%O " headch 
        print_l tails 

[<EntryPoint>]
let main argv =
    let l = [5;6;2;2;3;3;3;5;5;5]
    let k = [5;6;2;2;3;8;3;5;4;5;5;6;2;2;3;3;3;5;5;8]
    arrange l |> concatenation_all |> print_l
    arrange k |> concatenation_all |> print_l
    0
