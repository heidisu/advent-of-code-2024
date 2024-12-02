
open System.IO

let readFile () = 
    File.ReadLines "input.txt"
    |> Seq.toList
    |> List.map (fun l -> l.Split(" ") |> Array.map int |> Array.toList)

let parsed = readFile()

let is_dec_or_inc (l: int list) =
    let windowed = List.windowed 2 l
    let first = 
        windowed 
        |> List.head 
    let diff = first[1] - first[0]
    if diff = 0 then false
    else 
        let direction = diff / abs(diff)
        let found = 
            windowed
            |> List.tryFind (fun p -> 
                let diff = p[1] - p[0]
                abs(diff) > 3 || abs(diff) < 1 ||  diff = 0 || diff/abs(diff) <> direction
            )
        found = None

let task1 = 
    parsed
    |> List.filter is_dec_or_inc
    |> List.length
let task2 =
    parsed
    |> List.filter (is_dec_or_inc >> not)
    |> List.filter (fun l -> 
        let can_be_ok = 
            [0 .. List.length l - 1]
            |> List.tryFind (fun i -> List.removeAt i l |> is_dec_or_inc)
        Option.isSome can_be_ok
    )
    |> List.length
    |> (+) task1

printfn $"Task 1: {task1}"
printfn $"Task 2: {task2}"
