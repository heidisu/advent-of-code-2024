
open System.IO

let grid = 
    File.ReadLines "input.txt"
    |> Seq.toList
    |> List.map (fun l -> Seq.toList l)

let width = grid |> List.head |> List.length
let height  = grid |> List.length

type Dir = Up | Down | Left | Right

let next dir (x, y) = 
    match dir with
    | Up -> (x, y - 1)
    | Down -> (x, y + 1)
    | Left -> (x - 1, y)
    | Right -> (x + 1, y)

let rotate = function
    | Up -> Right
    | Right -> Down
    | Down -> Left
    | Left -> Up

let rec move acc dir (x, y) = 
    let (nx, ny)  = next dir (x, y)
    if nx < 0 || nx >= width || ny < 0 || ny >= height then (x, y) :: acc
    elif grid[ny][nx] = '#' then move acc (rotate dir) (x, y)
    else move ((x, y) :: acc) dir (nx, ny) 

let rec search_loop (grid: char list list) count acc dir (x, y) = 
    if Set.contains ((x, y), dir) acc then true 
    else
        let (nx, ny)  = next dir (x, y)
        if nx < 0 || nx >= width || ny < 0 || ny >= height then false
        elif grid[ny][nx] = '#' then search_loop grid (count + 1) acc (rotate dir) (x, y)
        else search_loop grid (count + 1) (Set.add ((x, y), dir) acc) dir (nx, ny) 


let startpoint = 
    [0 .. height - 1] 
    |> List.map (fun y -> (List.tryFindIndex (fun x -> x = '^') grid[y], y))
    |> List.find (fun (x, y) -> Option.isSome x)
    |> fun (x, y) -> (Option.get x, y)

let task1 = move  [] Up startpoint |> Set.ofList |> Set.count

let task2 =
    move  [] Up startpoint 
    |> Set.ofList 
    |> Set.remove startpoint 
    |> Set.toList
    |> List.filter (fun (x , y) -> 
        let newGrid = 
            [0 .. height - 1]
            |> List.map (fun b -> 
                if y = b then [0 .. width - 1] |> List.map (fun a -> if a = x then '#' else grid[b][a])
                else grid[b])
        search_loop newGrid 0 Set.empty Up startpoint
    )
    |> List.length

printfn $"Task 1: {task1}"
printfn $"Task 2: {task2}"
