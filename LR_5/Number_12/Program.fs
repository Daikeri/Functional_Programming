open System

[<EntryPoint>]
let main argv =
    let YourResponse language =
        match language with
        |"F#"|"Prolog"->printfn "%s" "жэс чел ну ты дурында конечно"
        |"Scala" -> printfn "%s" "нифига а ты норм мэнчик"
        |"Swift" -> printfn "%s" "ага попался яблоковод (Диня, Рустамчик)"
        |"Pascal" -> printfn "%s" "пожилая беброчка получается" 
        |"Python" -> printfn "%s" "ну все ясно ты дундук"
        |_ -> printfn "%s" "ну нет так нет че бубнить то"
    printfn "Какую языку предпочитаете?"

    // композиция функций 

    (Console.ReadLine>>YourResponse)()

    // каррирование 

    let func inn ans = ans(inn())
    func Console.ReadLine YourResponse
    0 
