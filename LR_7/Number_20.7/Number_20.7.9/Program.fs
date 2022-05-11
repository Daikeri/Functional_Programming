open System
let find_max (str:string) =
    Convert.ToInt32(str.ToCharArray() |> Array.max)
    
let find_diff (strr:string) =
    let centre =
        if strr.Length % 2 = 0 && strr.Length > 2 then ((strr.Length/2)-1,strr.Length/2)
        elif strr.Length % 2 = 1 && strr.Length > 2 then ((strr.Length-1)/2,0)
        else (0,0)
    match centre with
    |(0,0) -> 0

    |(other,0) ->
        let one = Convert.ToInt32(strr.[other-1])
        let two = Convert.ToInt32(strr.[other+1])
        (fun x y -> x - y) one two
        
    |(left,right) ->
        let one = Convert.ToInt32(strr.[left-1])
        let two = Convert.ToInt32(strr.[right+1])
        (fun z k -> z - k) one two

let general list =
    list |> List.map(fun x -> (find_max x - find_diff x)*(find_max x - find_diff x)) |> List.sort |> printfn "%A"
    
    


[<EntryPoint>]
let main argv =
    let gen = ["__{c*__";"1";"11";"sPqa(*"] // *- 42 { - 123 = 81 -> max = { - 123 -> (123-81)^2 = 1764
    general gen                      // ( - 40 P - 80  = 40 -> max = s - 115 -> (40-115)^2 = 5625
    0
