
open System.IO

let readFile () = 
    let lines = 
        File.ReadLines "input.txt"
        |> Seq.toList
        |> List.map (fun l -> 
                        let parts = l.Split(" ")
                        (int parts[0], int parts[Array.length parts - 1])
                        )
    (lines |> List.map fst, lines |> List.map snd)               

let (l1, l2) = readFile ()
let task1 =
    List.zip (List.sort l1) (List.sort l2)
    |> List.map (fun (a, b) -> abs (a - b))
    |> List.sum
let task2 =
    List.fold (fun acc v -> 
            let res = l2 |> List.filter (fun i -> i = v) |> List.length
            acc + (res * v)    
            ) 0 l1

printfn $"Task 1: {task1}" // 1530215
printfn $"Task 2: {task2}" // 26800609
