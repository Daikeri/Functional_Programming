open System


let YourResponse language =
    match language with
    |"F#"|"Prolog"->printfn "%s" "жэс чел ну ты дурында конечно"
    |"Scala" -> printfn "%s" "нифига а ты норм мэнчик"
    |"Swift" -> printfn "%s" "ага попался яблоковод (Диня, Рустамчик)"
    |"Pascal" -> printfn "%s" "пожилая беброчка получается" 
    |"Python" -> printfn "%s" "ну все ясно ты дундук"
    |_ -> printfn "%s" "ну нет так нет че бубнить то"

[<EntryPoint>]
let main argv =
    printfn "Какую языку предпочитаете?"
    let x = Console.ReadLine()
    YourResponse x
    0 