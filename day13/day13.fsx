
open System.IO

let readFile () = 
    File.ReadLines "input.txt"
    |> Seq.toList
    |> List.fold (fun (a, b) l -> 
        if l = "" then (b :: a, [])
        else (a, b @ [l])
    ) ([], [])
    |> (fun (a, b) -> b :: a)
    |> List.map (fun l -> 
        let first = l[0].Replace("Button A: X+", "").Replace(" Y+", "").Split(",")
        let second = l[1].Replace("Button B: X+", "").Replace(" Y+", "").Split(",")
        let prize = l[2].Replace("Prize: X=", "").Replace(" Y=", "").Split(",")
        ((int64 first[0], int64 first[1]), (int64 second[0], int64 second[1]), (int64 prize[0], int64 prize[1]))
        )

let solve ((a, b): int64 * int64) ((c, d): int64 * int64) ((z, w): int64 * int64) = 
    let y = (double a * double w - double b * double z) /(double a * double d - (double b * double c))
    let x = (double z - double c * y)/double a 
    if x = floor x && y = floor y then Some (int64 x, int64 y)
    else None

let task1 = 
    readFile()
    |> List.map (fun (x, y, z) -> solve x y z)
    |> List.choose id
    |> List.map (fun (x, y) -> x * 3L + y * 1L)
    |> List.sum
let task2 = 
    readFile ()
    |> List.map (fun (x, y, (a, b)) -> (x, y, (a + 10000000000000L, b + 10000000000000L)))
    |> List.map (fun (x, y, z) -> solve x y z)
    |> List.choose id
    |> List.map (fun (x, y) -> x * 3L + y * 1L)
    |> List.sum

printfn $"Task 1: {task1}"
printfn $"Task 2: {task2}"
