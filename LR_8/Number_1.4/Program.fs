open System

type IPrint = interface
    abstract member Print: unit -> unit 
    end

[<AbstractClass>]
type GenFigure() =
    abstract member Area: unit -> float

type rectangle(_width, _height) =
    inherit GenFigure()

    member val Width = _width with get,set // автосвойства
    member val Height = _height with get,set

    new() = rectangle(2.0,4.0) // вторичный конструктор

    override this.Area() =
        this.Width * this.Height

    override this.ToString() =
        (this.Width,this.Height,this.Area())
        |||> sprintf "Rectangle\nWidth: %f Height: %f Area: %f"

    interface IPrint with
        member this.Print() = this.ToString() |> printfn "%s"
    // Методы интерфейса можно вызывать только через интерфейс, а не через любой объект типа, реализующего интерфейс.
    // :> переадресация к типу интерфейса
    member this.Print() = (this :> IPrint).Print()

type square(_side) = 
    inherit rectangle(_side,_side)

    member val Side = _side with get,set

    new() = square(2.0)

    override this.ToString() = 
        (this.Side,this.Area()) ||> sprintf "Square\nSide: %f Area: %f"
    // т.к. реализация интерфейса наследуется, то повторная реализация не требуется

type ring(_radius) =
    inherit GenFigure()

    member val Radius = _radius with get,set

    override this.Area() =
        Math.PI * this.Radius ** 2.0

    override this.ToString() =
        (this.Radius,this.Area()) ||> sprintf "Ring\nRadius: %f Area: %f"

    interface IPrint with
        member this.Print() = this.ToString() |> printfn "%s"
    
    member this.Print() = (this :> IPrint).Print()

type Figure =
    |Rect of width:float * height: float
    |Squ of side:float
    |Rin of radius:float

let find_area =
    function
    |Rect(w,h) -> w * h
    |Squ (s) -> s * s
    |Rin (r) -> Math.PI * r * r

[<EntryPoint>]
let main argv =
    List.iter(fun (x:IPrint) -> x.Print()) [rectangle(7.0,4.0);square(9.0);ring(10.0)]
    Console.WriteLine()
    List.iter (fun (y:Figure) -> printfn "%A | %f" y  (find_area y)) [Rect(7.0, 4.0); Squ(9.0); Rin(10.0)]
    0
