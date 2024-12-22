
open System.IO
open System

let readFile () = 
    let text = File.ReadAllText "input.txt"
    let splits = text.Split(" ")
    splits |> Array.map (fun s -> int64 s) |> Array.toList

let step stone = 
    if stone = 0L then [1L]
    else
        let stonestr = stone |> string
        let length = Seq.length stonestr
        if length % 2 = 0 then
            let first = stonestr |> Seq.take (length/2) |> String.Concat |> int64
            let second = stonestr |> Seq.skip (length/2) |> String.Concat |> int64
            [first; second]
        else [stone * 2024L]

let task1 = 
    [1 .. 25] 
    |> List.fold (fun l _ -> List.collect step l) (readFile())
    |> List.length

let task2 = 
    let startMap = 
        readFile()
        |> List.map (fun s -> (s, 1L))
        |> Map.ofList
    [1 .. 75] 
    |> List.fold (fun mp i  -> 
        mp
        |> Map.keys
        |> Seq.fold (fun m k -> 
            let prevVal = Map.find k mp
            step k
            |> List.fold (fun m st -> 
                if Map.containsKey st m then Map.add st (prevVal + Map.find st m) m
                else Map.add st prevVal m
            ) m
        ) Map.empty
        ) startMap
    |> Map.values
    |> Seq.sum

printfn $"Task 1: {task1}"
printfn $"Task 2: {task2}"
