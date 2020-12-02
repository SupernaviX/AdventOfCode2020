open System.IO;

let findAnswer ((seen, solutions) : Set<_> * Set<_ * _>) num =
    if seen.Contains(2020 - num) then
        (seen.Add(num), solutions.Add((num, 2020 - num)))
    else (seen.Add(num), solutions)

let answer = System.IO.File.ReadLines("./input")
    |> Seq.map int
    |> Seq.fold findAnswer (Set.empty, Set.empty)
    |> snd
    |> Seq.exactlyOne
    ||> (fun first second -> first * second)

printfn "%i" answer