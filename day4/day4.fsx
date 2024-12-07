
open System.IO
open System

let grid = 
    File.ReadLines "input.txt"
    |> Seq.toList
    |> List.map (fun l -> Seq.toList l)

let width = grid |> List.head |> List.length
let height = grid |> List.length

let dirs = [
    (1, 0)
    (-1, 0)
    (0, 1)
    (0, -1)
    (1, 1)
    (1, -1)
    (-1, 1)
    (-1, -1)
]

let in_grid grid width height coords = 
    coords
    |> List.tryFind (fun (x, y) -> x < 0 || x >= width || y < 0 || y >= height)
    |> Option.isNone


let search (grid: char list list) x y = 
    if grid[y][x] = 'X' then
        dirs
        |> List.map (fun (a, b) -> [1 .. 3] |> List.map (fun i -> (x + i*a, y + i*b)))
        |> List.filter (in_grid grid width height)
        |> List.map (fun l -> 
                l 
                |> List.map (fun (x, y) -> grid[y][x]) 
                |> (fun l -> String.Concat (l)))
        |> List.filter(fun s -> s = "MAS")
        |> List.length
    else 0

let search2 (grid: char list list) x y = 
    if grid[y][x] = 'A' then
        let diag1 = String.Concat([grid[y-1][x-1]; 'A'; grid[y+1][x+1]])
        let diag2 = String.Concat([grid[y+1][x-1]; 'A'; grid[y-1][x+1]])
        if (diag1 = "MAS" || diag1 = "SAM") && (diag2 = "MAS" || diag2 = "SAM") then 1 else 0
    else 0

let task1 = 
    [0 .. height - 1]
    |> List.collect (fun y -> [0 .. width - 1] |> List.map (fun x -> search grid x y))
    |> List.sum
let task2 =
    [1 .. height - 2]
    |> List.collect (fun y -> [1 .. width - 2] |> List.map (fun x -> search2 grid x y))
    |> List.sum

printfn $"Task 1: {task1}"
printfn $"Task 2: {task2}"
