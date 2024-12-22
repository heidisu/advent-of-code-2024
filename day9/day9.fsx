
open System.IO

let readFile () = 
    let numbers = 
        File.ReadAllText "input.txt"
        |> (fun s -> s.Trim() + "0")
        |> Seq.map (fun c -> (int c) - (int '0'))
    let size = numbers |> Seq.sum |> int
    let data = Array.create size (-1)
    let pairs = Seq.chunkBySize 2 numbers
    pairs
    |> Seq.fold (fun (idx, id) p -> 
        let file = p[0]
        let space = p[1]
        [idx .. idx + file - 1]|> List.iter(fun i -> data[i] <- id)
        (idx + file + space, id + 1)) (0, 0)
    |> ignore
    data

let rec move (data: int array) (idxa: int) (idxb: int) = 
    if idxa = idxb then data
    else
        let front = data[idxa]
        let back = data[idxb]
        if front = -1 && back >= 0 then 
            data[idxa] <- back
            data[idxb] <- -1
            move data (idxa + 1) (idxb - 1)
        elif front = -1 && back = -1 then move data idxa (idxb - 1)
        elif front >= 0 && back >= 0 then move data (idxa + 1) idxb
        else move data (idxa + 1) (idxb - 1)

let rec move2 (data: int array) (files: (int * int * int) list) (blanks: (int * int) list) = 
    match files with
    | [] -> data
    | (i, s, l) :: xs -> 
        let found = blanks |> List.tryFind (fun (it, lt) -> it < s && lt >= l)
        match found with
        | None -> move2 data xs blanks
        | Some (it, lt) ->
            [0 .. l - 1] 
            |> List.iter (fun j -> 
                data[it + j] <- i
                data[s + j] <- -1)
            let newBlanks = 
                blanks |> List.filter (fun x -> x <> (it, lt))
            move2 data xs ((it + l, lt - l) :: newBlanks |> List.sortBy fst)


let data = readFile()
let task1 = 
    let data = readFile()
    let res = move data 0 (Array.length data - 1)
    res 
    |> Array.fold (fun (idx, acc) i -> if i > 0 then (idx + 1, acc + int64 (idx * i)) else (idx + 1, acc)) (0, 0L)
    |> snd
let task2 =
    let data = readFile()
    let blanks = 
        [0 .. Array.length data - 1] 
        |> List.filter (fun i -> data[i] = -1)
    let foo = 
        blanks
        |> List.tail 
        |> List.fold (fun (a, p) i -> if i - List.head p > 1 then (p:: a, [i]) else (a, i :: p) ) ([], [List.head blanks])
        |> fst
        |> List.rev
        |> List.map (fun l -> (List.last l, List.length l))
    let files = 
        data 
        |> Array.mapi (fun i v -> (i, v))
        |> Array.toList
        |> List.groupBy (fun (i, v) -> v)
        |> List.map (fun (v, l) -> (v, l |> List.map fst |> List.min, List.length l))
        |> List.filter (fun (v, i, l) -> v >= 0)
        |> List.sortByDescending (fun (v, i, l) -> v)
    let res = move2 data files foo
    res 
    |> Array.fold (fun (idx, acc) i -> if i > 0 then (idx + 1, acc + int64 (idx * i)) else (idx + 1, acc)) (0, 0L)
    |> snd

printfn $"Task 1: {task1}" // 6310675819476
printfn $"Task 2: {task2}" // 6339868922822 too high
