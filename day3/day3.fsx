
open System.IO
open System.Text.RegularExpressions

let pattern = "mul\([0-9]+,[0-9]+\)"
let do_pattern = "do\(\)"
let dont_pattern = "don't\(\)"

let text = File.ReadAllText "input.txt"

let getMatches str patt = 
    let matches = Regex.Matches(str, patt)
    printfn "mathces: %A" matches.Count
    [0 .. matches.Count - 1] |> List.map (fun i -> matches.Item(i))

let parseMul (str: string) = 
    let parts = str.Replace("mul", "").Replace("(", "").Replace(")", "").Split(",")
    int64 parts[0] * int64 parts[1]

let task1 = 
    getMatches text pattern
    |> List.map (fun i -> i.Value)
    |> List.map parseMul
    |> List.sum
let task2 =
    let matches = getMatches  text pattern
    let do_matches = getMatches text do_pattern
    let dont_matches = getMatches text dont_pattern
    matches @ do_matches @ dont_matches
    |> List.sortBy (fun i -> i.Index)
    |> List.map (fun i -> i.Value)
    |> List.fold (fun (b, acc) i -> 
        if i = "do()" then (true, acc)
        elif i = "don't()" then (false, acc)
        elif b = false then (false, acc)
        else (true, acc + parseMul i)
    ) (true, 0L)
    |> snd
    
printfn $"Task 1: {task1}"
printfn $"Task 2: {task2}"
