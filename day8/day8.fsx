open System.IO

let readFile () = 
    File.ReadLines "input.txt"
    |> Seq.toList
    |> List.map (fun l -> l |> Seq.toList)

let grid = readFile()
let width = grid |> List.head |> List.length
let height = grid |> List.length

let inGrid (x, y) = x >= 0 && x < width && y >= 0 && y < height

let calcLines (points: (int * int) list) = 
    if List.length points = 1 then Set.empty
    else 
        points
        |> List.collect (fun p -> points |> List.map (fun q -> (p, q)))
        |> List.filter (fun (p, q) -> p <> q)
        |> List.distinct
        |> List.collect (fun ((p1, p2), (q1, q2)) -> 
                let xdist = p1 - q1
                let ydist = p2 - q2
                [(p1 - xdist, p2 - ydist); (p1 + xdist, p2 + ydist); (q1 + xdist, q2 + ydist); (q1 - xdist, q2 - ydist)] 
                |> List.filter (fun x -> x <> (p1, p2) && x <> (q1, q2)))
        |> Set.ofList

let rec move acc (xdist, ydist) (x, y) = 
    if inGrid (x, y) then move ((x, y) :: acc) (xdist, ydist) (x + xdist, y + ydist)
    else acc

let calcLines2 (points: (int * int) list) = 
    if List.length points = 1 then Set.empty
    else 
        points
        |> List.collect (fun p -> points |> List.map (fun q -> (p, q)))
        |> List.filter (fun (p, q) -> p <> q)
        |> List.distinct
        |> List.collect (fun ((p1, p2), (q1, q2)) -> 
                let xdist = p1 - q1
                let ydist = p2 - q2
                move [] (xdist, ydist) (p1, p2) @ move [] (-xdist, -ydist) (p1, p2))
        |> Set.ofList



let task1 = 
    [0 .. height - 1]
    |> List.collect (fun y -> [0 .. width - 1] |> List.map (fun x -> (x, y, grid[y][x])))
    |> List.filter (fun (_, _, c) -> c <> '.')
    |> List.groupBy (fun (_, _, c) -> c)
    |> List.map (fun (c, l) -> (c, l |> List.map (fun (x, y, c) -> (x, y))))
    |> List.map (fun (c, l) -> (c, calcLines l))
    |> List.fold (fun a (x, l) -> Set.union a l) Set.empty
    |> Set.filter inGrid
    |> Set.count 
let task2 =
    [0 .. height - 1]
    |> List.collect (fun y -> [0 .. width - 1] |> List.map (fun x -> (x, y, grid[y][x])))
    |> List.filter (fun (_, _, c) -> c <> '.')
    |> List.groupBy (fun (_, _, c) -> c)
    |> List.map (fun (c, l) -> (c, l |> List.map (fun (x, y, c) -> (x, y))))
    |> List.map (fun (c, l) -> (c, calcLines2 l))
    |> List.fold (fun a (x, l) -> Set.union a l) Set.empty
    |> Set.count

printfn $"Task 1: {task1}"
printfn $"Task 2: {task2}"
