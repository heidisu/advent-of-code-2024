
open System.IO

let readFile () = 
    File.ReadLines "input.txt"
    |> Seq.toList
    |> List.map (fun l -> l |> Seq.toList)

let grid = readFile ()
let width = grid[0] |> List.length
let height = grid |> List.length
let points = [0 .. height - 1] |> List.collect (fun y -> [0 .. width - 1] |> List.map (fun x -> (x, y, grid[y][x])))
let groups = 
    points 
    |> List.groupBy (fun (_, _, c) -> c)
    |> List.map (fun (c, points) -> (c, points |> List.map (fun (x, y, _) -> (x, y))))


let isNeighbours (x, y) (z, w) = (x = z && abs(y - w) = 1) || (y = w && abs(x - z) = 1) 

let rec findSubGroups groups points = 
    match points with
    | [] -> groups
    | x :: xs ->
        let neighbourSets = groups |> Set.filter (fun s -> Set.exists (fun p -> isNeighbours p x) s)
        let xSet = Set.empty |> Set.add x
        match Set.count neighbourSets with
        | 0 -> findSubGroups (Set.add xSet groups) xs
        | _ -> 
            let mergedSet = neighbourSets |> Set.fold (fun acc s -> Set.union s acc) xSet
            let newGroups = 
                groups 
                |> Set.filter (fun s -> not <| Set.contains s neighbourSets)
                |> Set.add mergedSet
            findSubGroups newGroups xs

let findPerimeters group = 
    let neighbourEdges = 
        group
        |> Set.toList 
        |> List.map (fun s -> group |> Set.filter (fun t -> isNeighbours s t) |> Set.count)
        |> List.sum
    (Set.count group) * 4 - neighbourEdges

type Dir = Left | Right | Up | Down

let rec sortBorder visited sorted dir (a, b) border = 
    if Set.isEmpty border then sorted 
    else
        let nv = Set.add (a, b) visited
        printfn "%A %A %A" dir (a, b) border
        let borderList = border |> Set.toList
        match dir with
        | Left -> borderList 
                    |> List.tryFind (fun (x, y) -> x = a - 1 && y = b)
                    |> function
                    | Some v -> sortBorder nv sorted dir v (Set.remove v border)
                    | None -> sortBorder nv (dir :: sorted) Up (a, b) border 
        | Up -> borderList
                    |> List.tryFind (fun (x, y) -> x = a  && y = b - 1)
                    |> function
                    | Some v -> sortBorder nv sorted dir v (Set.remove v border)
                    | None -> sortBorder nv (dir :: sorted) Right (a, b) border 
        | Right -> borderList
                    |> List.tryFind (fun (x, y) -> x = a + 1 && y = b)
                    |> function
                    | Some v -> sortBorder nv sorted dir v (Set.remove v border)
                    | None -> sortBorder nv (dir :: sorted) Down (a, b) border 
        | Down -> borderList
                    |> List.tryFind (fun (x, y) -> x = a  && y = b + 1)
                    |> function
                    | Some v -> sortBorder nv sorted dir v (Set.remove v border)
                    | None -> sortBorder nv (dir :: sorted) Left (a, b) border 

type Line = Vertical | Horisontal

let calcSegmentsSameLine ((key, _), segments) = 
    if List.length segments = 1 then 1 
    else
        let points = 
            match key with
            | Vertical -> 
                segments
                |> List.map (fun ((x, y), (a, b), v) -> (y, b))
                |> List.sort
            | Horisontal -> 
                segments
                |> List.map (fun ((x, y), (a, b), v) -> (x, a))
        let group1 = 
            points 
            |> List.filter (fun (x, y) -> x < y)
            |> List.sort
        let group2 = 
            points 
            |> List.filter (fun (x, y) -> x > y)
            |> List.sortDescending
        let res1 = 
            if List.isEmpty group1 then 0
            else
                group1
                |> List.tail
                |> List.fold (fun (acc, (a, b)) (x, y) -> if b = x then (acc, (x, y)) else (acc + 1, (x, y))) (1, List.head group1)
                |> fst
        let res2 = 
            if List.isEmpty group2 then 0 
            else 
                group2
                |> List.tail
                |> List.fold (fun (acc, (a, b)) (x, y) -> if b = x then (acc, (x, y)) else (acc + 1, (x, y))) (1, List.head group2)
                |> fst
        res1 + res2
let findSides c group = 
    let toSegments = 
        group
        |> Set.toList
        |> List.collect (fun (x, y) -> [
            ((x, y), (x + 1, y))
            ((x + 1, y), (x + 1, y + 1))
            ((x + 1, y + 1), (x, y + 1))
            ((x, y + 1), (x, y))
            ])
        |> List.map (
            fun ((x, y), (z, w)) -> 
                let key = if (x, y) <= (z, w) then ((x, y), (z, w)) else ((z, w), (x, y))
                let value = ((x, y), (z, w))
                (key, value))
        |> List.groupBy fst
        |> List.map (fun (s, l) -> (s, l, List.length l))
        |> List.filter(fun (s, l, c) -> c = 1)
        |> List.collect (fun (s, l, c) -> l |> List.map snd)
    let segGroups =
        toSegments
        |> List.map (fun ((x, y), (z, w)) -> 
                let key = 
                    if x = z then (Vertical, x) else (Horisontal, y) 
                ((x, y), (z, w), key)
        )
        |> List.groupBy (fun (x, y, k) -> k)
        |> List.map calcSegmentsSameLine
        |> List.sum
    segGroups

let task1 = 
    groups
    |> List.map (fun (_, l) -> 
        let firstSet = Set.empty |> Set.add (List.head l)
        l 
        |> List.tail 
        |> findSubGroups (Set.empty |> Set.add firstSet)
        |> Set.toList 
        |> List.map (fun s -> int64 (Set.count s) * int64 (findPerimeters s)))
    |> List.map List.sum
    |> List.sum
        
let task2 =
    groups
    |> List.map (fun (c, l) -> 
        let firstSet = Set.empty |> Set.add (List.head l)
        l 
        |> List.tail 
        |> findSubGroups (Set.empty |> Set.add firstSet)
        |> Set.toList 
        |> List.map (fun s -> int64 (Set.count s) * int64 (findSides c s)))
        |> List.map (List.sum)
    |> List.sum 

printfn $"Task 1: {task1}"
printfn $"Task 2: {task2}"