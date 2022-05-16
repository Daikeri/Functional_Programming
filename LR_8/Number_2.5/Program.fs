open System 
 type MyType<'a> = // определяет два контекста
 | Bedolaga of 'a
 | None

 let imitationFmap func ty = // функтор
    match ty with
    |Bedolaga k -> Bedolaga (func k) // разворачиваем контекст, применяем функцию, снова заворачиваем в контекст
    |None -> None
 
 let immitFmap_list func list = 
    List.map(fun x -> func x) list

 let immitationApply func ty = // apply для апликативного функтора
    match func,ty with
    |Bedolaga f, Bedolaga t -> Bedolaga(f t)
    |_ -> None

 let immitApply_list (func_list: ('a -> 'b) list) (el_list:'a list) =
    [ for f in func_list do
        for el in el_list do
            yield f el
    ]
 
 let immitApply_list2 (el_list:'a list) (func_list: ('a -> 'b) list) =
     [ for f in func_list do
         for el in el_list do
             yield f el
     ]

// а монада крч дома спит 

let give x = x 
let func_one x = x+10
let func_two x = x*2

let gen_for_Fmap () =
    let orig_list = [1..3]
    Console.WriteLine("1-й закон:")
    orig_list |> printfn "Исходный список: %A"
    orig_list |> immitFmap_list give |> printfn "Подъем нейтральной функции: %A"
    let orig_list = [1..3]
    Console.WriteLine("2-й закон:")
    let prom_list = immitFmap_list func_one orig_list
    prom_list |> immitFmap_list func_two |> printfn "Последовательное поднятие функций: %A"
    orig_list |> immitFmap_list (func_one>>func_two) |> printfn "Поднятие композиции: %A"

let gen_for_Apply () =
    let orig_list = [1..3]
    let x = 9
    Console.WriteLine("1-й закон:")
    orig_list |> printfn "Исходный список: %A"
    orig_list |> immitApply_list [give] |> printfn "Подъем нейтральной функции: %A"
    Console.WriteLine("2-й закон:")
    [func_one x] |> printfn "Подъем f(x): %A"
    immitApply_list [func_one] [x] |> printfn "Подъем f и x: %A"
    Console.WriteLine("3-й закон:")
    immitApply_list [func_one;func_two] orig_list |> printfn "Функции и список: %A"
    immitApply_list2 orig_list [func_one;func_two] |> printfn "Список и функции: %A"
    
[<EntryPoint>]
let main argv =
    gen_for_Fmap()
    Console.WriteLine()
    gen_for_Apply() 
    0
