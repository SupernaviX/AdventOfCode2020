open System.IO

let numbers = 
    System.IO.File.ReadLines("./input")
    |> Seq.map int
    |> List.ofSeq
    |> List.sort

let candidates = 
    numbers |> List.collect(fun x -> 
                   numbers
                   |> List.filter(fun y -> y > x)
                   |> List.map(fun y -> (x, y))
                   |> List.collect(fun (x, y) -> 
                          numbers
                          |> List.filter(fun z -> z > y)
                          |> List.map(fun z -> (x, y, z))))

let answer = 
    candidates
    |> List.filter(fun (x, y, z) -> x + y + z = 2020)
    |> List.exactlyOne
    |||> (fun x y z -> x * y * z)

printfn "%i" answer