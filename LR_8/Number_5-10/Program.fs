open System
open System.Text.RegularExpressions
open System.Diagnostics

type IPrint = interface
    abstract member Print: unit -> unit 
    end

let regSeries (str:string) =
    let r = @"^([0-9]{2}[0-9]{2})?$"
    Regex.IsMatch(str,r)

let regNumber (str:string) =
    let r = @"^([0-9]{6})?$"
    Regex.IsMatch(str,r)

let regBirthDate (str:string) =
    let r = @"^(0[1-9]|[12][0-9]|3[01])[.](0[1-9]|1[012])[.](19|20)\d\d$"
    Regex.IsMatch(str,r)

let regFullName (str:string) =
    let r = @"^([А-ЯA-Z]|[А-ЯA-Z][\x27а-яa-z]{1,}|[А-ЯA-Z][\x27а-яa-z]{1,}\-([А-ЯA-Z][\x27а-яa-z]{1,}|(оглы)|(кызы)))\040[А-ЯA-Z][\x27а-яa-z]{1,}(\040[А-ЯA-Z][\x27а-яa-z]{1,})?$"
    Regex.IsMatch(str,r)

let regGender(str:string) =
    let r = "(^Муж$)|(^муж$)|(^Жен$)|(^жен$)|(^Мужской$)|(^мужской$)|(^Женский$)|(^женский$)|(^М$)|(^м$)|(^Ж$)|(^ж$)"
    Regex.IsMatch(str,r)
    
type PassportRF(_series,_number,_fullName,_gender,_birthdate) =
    member val Series = 
        if regSeries _series then _series
        else
            let rec prom str t_f =
                match str,t_f with
                |other,true -> other
                |other,false ->
                    Console.Clear()
                    Console.WriteLine("Неверный формат серии паспорта! Введите корректный вариант:")
                    let s = Console.ReadLine()
                    let new_t_f = if regSeries s then true else false
                    prom s new_t_f
            prom "" false 
            
    member val Number = 
        if regNumber _number then _number
        else
            let rec prom str t_f =
                match str,t_f with
                |other,true -> other
                |other,false ->
                    Console.Clear()
                    Console.WriteLine("Неверный формат номера паспорта! Введите корректный вариант:")
                    let s = Console.ReadLine()
                    let new_t_f = if regNumber s then true else false
                    prom s new_t_f
            prom "" false  

    member val FullName = 
        if regFullName _fullName then _fullName
        else
            let rec prom str t_f =
                match str,t_f with
                |other,true -> other
                |other,false ->
                    Console.Clear()
                    Console.WriteLine("Неверный формат ФИО! Введите корректный вариант:")
                    let s = Console.ReadLine()
                    let new_t_f = if regFullName s then true else false
                    prom s new_t_f
            prom "" false  
       
    member val Gender =
        if regGender _gender then _gender
        else
            let rec prom str t_f =
                match str,t_f with
                |other,true -> other
                |other,false ->
                    Console.Clear()
                    Console.WriteLine("Неверный формат пола! Введите корректный вариант:")
                    let s = Console.ReadLine()
                    let new_t_f = if regGender s then true else false
                    prom s new_t_f
            prom "" false  
       
    member val BirthDate =
        if regBirthDate _birthdate then _birthdate
        else 
            let rec prom str t_f =
                match str,t_f with
                |other,true -> other
                |other,false ->
                    Console.Clear()
                    Console.WriteLine("Неверный формат даты рождения! Введите корректный вариант:")
                    let s = Console.ReadLine()
                    let new_t_f = if regBirthDate s then true else false
                    prom s new_t_f
            prom "" false  

    override this.ToString() = 
        sprintf "Series: %s | Number: %s | FullName: %s | Gender: %s | BirthDate: %s" this.Series this.Number this.FullName this.Gender this.BirthDate

    interface IPrint with
        member this.Print() = this.ToString() |> printfn "%s"

    member this.Print() = (this :> IPrint).Print()

    interface IComparable with
        member this.CompareTo(obj:obj):int =
            match obj with
            | :? PassportRF as next -> if (this.Series = next.Series) then this.Number.CompareTo next.Number else this.Series.CompareTo next.Series
            | _ -> invalidArg "obj" "сравнение невозможно"

    override this.GetHashCode() =
        Tuple.Create(this.Series,this.Number).GetHashCode()

    override this.Equals(obj2) =
           match obj2 with
           | :? PassportRF as lic2 -> (this.Series = lic2.Series) && (this.Number = lic2.Number)
           | _ -> false
    
[<AbstractClass>]
type GenDoc() =
   abstract member searchDoc : PassportRF -> bool

type ArrayDoc(list:PassportRF list) =
    inherit GenDoc()
    member val Arr = Array.ofList list
    override this.searchDoc(now) =
        Array.exists(fun k -> k.Equals now) this.Arr

type ListDoc(list:PassportRF list) =
    inherit GenDoc()
    member val L = list
    override this.searchDoc(now) =
        List.exists(fun k -> k.Equals now) this.L

type SetDoc(list:PassportRF list) =
    inherit GenDoc()
    member val S = Set.ofList list
    override this.searchDoc(now) =
        Set.exists(fun k -> k.Equals now) this.S

type BinListDoc(list: PassportRF list)=
    inherit GenDoc()

    let rec binSearch (sortedL: PassportRF list) (curr: PassportRF) =
        match List.length sortedL with
        | 0 -> false
        | other ->
            let mid = other/2
            match compare curr sortedL.[mid] with
            | 0 -> true
            | 1->binSearch sortedL.[mid+1..] curr
            | _->binSearch sortedL.[..mid-1] curr

    member this.BinList = List.sortBy (fun (x:PassportRF) -> (x.Series, x.Number)) list 

    override this.searchDoc(lic) =
        binSearch this.BinList lic

let Time (t:Stopwatch) func el =
    t.Restart()
    t.Start()
    let prom = func el
    t.Stop()
    t.ElapsedMilliseconds

[<EntryPoint>]
let main argv =

(*_series,_number,_fullName,_gender,_birthdate*)
    let t1=PassportRF("0312","123456","Дмитрий Нагиев","муж","25.12.1984")
    let t2=PassportRF("1312","234567","Дмитрий Нагиев","муж","25.12.1984")
    let t3=PassportRF("2312","345678","Дмитрий Нагиев","муж","25.12.1984")
    let t4=PassportRF("0099","567881","Дмитрий Нагиев","муж","25.12.1984")
    let t5=PassportRF("4499","167781","Дмитрий Нагиев","муж","25.12.1984")
    let t6=PassportRF("4009","332244","Дмитрий Нагиев","муж","25.12.1984")

    let test_l = [t5;t6;t1;t4;t2;t3]

    let lmao = Stopwatch()

    let arr = ArrayDoc(test_l)
    let l = ListDoc(test_l)
    let set = SetDoc(test_l)
    let bin = BinListDoc(test_l)

    printfn "Array:%d" (Time lmao arr.searchDoc t5)
    printfn "List:%d" (Time lmao l.searchDoc t5)
    printfn "Set:%d" (Time lmao set.searchDoc t5)
    printfn "Bin:%d" (Time lmao bin.searchDoc t5)
    0 
