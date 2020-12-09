open System
open System.IO

[<EntryPoint>]
let main _ =

    let input = 
        File.ReadAllLines("input")
        |> Array.map Int64.Parse
        |> List.ofArray

    let targetSum = 1212510616L

    let takeUntilTargetSum list =
        let rec takeUntil acc sum list =
            match list with
            | head::tail when head + sum < targetSum 
                -> takeUntil (head::acc) (head + sum) tail
            | head::_ when head + sum = targetSum -> Some acc
            | _ -> None 
        takeUntil [] 0L list
    
    let rec solve list =
        match list, takeUntilTargetSum list with
        | _, Some(result) -> result
        | _::tail,_ -> solve tail
        | [],_ -> failwith "No solution found"

    let solution = solve input
    let min, max = List.min solution, List.max solution

    min + max
    |> printfn "%i"

    0 // return an integer exit code
