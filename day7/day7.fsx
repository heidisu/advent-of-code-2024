open System.IO

let readFile () = 
    File.ReadLines "input.txt"
    |> Seq.toList
    |> List.map (fun l -> 
        let parts = l.Split(":")
        let testValue = int64 parts[0]
        let numberParts = parts[1].Trim().Split(" ") |> Array.toList |> List.map int64
        (testValue, numberParts))

let concat (x: int64) (y: int64) = 
    (string x) + (string y) |> int64

let rec testEquation operations test (acc: int64 list) numbers = 
    match numbers with
    | [] -> 
        if List.contains test acc then true else false
    | x :: xs -> 
        let newAcc = 
            acc
            |> List.collect (fun n -> operations |> List.map (fun op -> op n x)) 
            |> List.filter (fun n -> n <= test)
        testEquation operations test newAcc xs

let calculateTestSum operators = 
    readFile()
    |> List.map (fun (t, n) ->  (t, testEquation operators t [List.head n] (List.tail n)))
    |> List.filter snd
    |> List.map fst
    |> List.sum
let task1 = calculateTestSum [(+); (*)]
let task2 = calculateTestSum [(+); (*); concat]

printfn $"Task 1: {task1}"
printfn $"Task 2: {task2}"
