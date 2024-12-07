
open System.IO

let readFile () = 
    File.ReadLines "input.txt"
    |> Seq.toList
    |> List.fold (fun (pairs, prints, parsing_pairs) l -> 
            if l = "" then (pairs, prints, false) 
            elif parsing_pairs then 
                let parts = l.Split("|")
                ((int parts[0], int parts[1]) :: pairs, prints, parsing_pairs)
            else 
                let print = l.Split(",") |> Array.map int |> Array.toList
                (pairs, print :: prints, parsing_pairs)
        ) ([],[], true)
    |> (fun (x, y, z) -> (x, y))

let (pairs, prints) = readFile()

let get_afters d = 
    pairs
    |> List.filter (fun (x, y) -> x = d)
    |> List.map snd

let get_befores d = 
    pairs
    |> List.filter (fun (xm, y) -> y = d)
    |> List.map fst

let is_valid_print l =
    [0 .. List.length l - 1] 
    |> List.tryFind (fun i -> 
        let (before, y) = List.splitAt i l
        let item = List.head y
        let after = List.tail y
        let actual_before = get_befores item
        let acutal_after = get_afters item
        let someBeforeIsAfter = before |> List.tryFind (fun i -> List.contains i acutal_after)
        let someAfterIsBefore = after |> List.tryFind(fun i -> List.contains i actual_before)
        someAfterIsBefore.IsSome || someBeforeIsAfter.IsSome   
        )
    |> _.IsNone

let rec order_list acc l = 
    match l with
    | [] -> acc
    | l ->    
        let start = 
            l 
            |> List.filter (fun i -> 
                Set.intersect (get_befores i |> Set.ofList) (Set.ofList l) = Set.empty
                )
            |> List.head
        order_list (start :: acc)  (List.removeAt (List.findIndex (fun i -> i = start) l) l)

let task1 = 
    prints
    |> List.filter is_valid_print
    |> List.map (fun l -> List.item (List.length l / 2) l)
    |> List.sum
let task2 =
    prints
    |> List.filter (is_valid_print >> not)
    |> List.map (order_list [])
    |> List.map (fun l -> List.item (List.length l / 2) l)
    |> List.sum


printfn $"Task 1: {task1}"
printfn $"Task 2: {task2}"
