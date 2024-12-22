
open System.IO

let readFile () = 
    File.ReadLines "input.txt"
    |> Seq.toList
    |> List.map (fun s -> s |> Seq.toList |> List.map (fun d -> int d - int '0'))

let grid = readFile()
let width = grid |> List.head |> List.length
let height = grid |> List.length

let inGrid (x, y) = x >= 0 && x < width && y >=0 && y < height

let next (x, y, v) = 
    [
        (x - 1, y)
        (x + 1, y)
        (x, y - 1)
        (x, y + 1)
    ]
    |> List.filter inGrid
    |> List.map (fun (x, y) -> (x, y, grid[y][x]))
    |> List.filter(fun (x, y, nv) -> nv = v + 1)

let rec step completed (acc: (int * int * int) list list)  = 
    let stepped = 
        acc 
        |> List.collect (
                fun l -> 
                    let nextSteps = next (List.head l)
                    nextSteps |> List.map (fun n -> n :: l)
            )
    let newAcc = stepped |> List.filter (fun l -> l |> List.head |> fun (x, y, v) -> v < 9)
    let newCompleted = stepped |> List.filter (fun l -> l |> List.head |> fun (x, y, v) -> v = 9)
    match newAcc with
    | [] -> completed @ newCompleted
    | xs -> step (completed @ newCompleted) xs

let task1 =
    [0 .. height - 1]
    |> List.collect (fun y -> [0 .. width - 1] |> List.map (fun x -> (x, y, grid[y][x])))
    |> List.filter (fun (x, y, v) -> v = 0)
    |> List.map (fun p -> [p])
    |> step []
    |> List.groupBy (fun l -> List.last l)
    |> List.map (fun (p, l) -> (p, l |> List.map (fun x -> List.head x) |> List.distinct |> List.length))
    |> List.map snd
    |> List.sum
let task2 = 
    [0 .. height - 1]
    |> List.collect (fun y -> [0 .. width - 1] |> List.map (fun x -> (x, y, grid[y][x])))
    |> List.filter (fun (x, y, v) -> v = 0)
    |> List.map (fun p -> [p])
    |> step []
    |> List.groupBy (fun l -> List.last l)
    |> List.map (fun (p, l) -> (p, l |> List.length))
    |> List.map snd
    |> List.sum

printfn $"Task 1: %A{task1}"
printfn $"Task 2: {task2}"
