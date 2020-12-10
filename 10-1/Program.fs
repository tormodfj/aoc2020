open System.IO

[<EntryPoint>]
let main _ =

    let input = 
        File.ReadAllLines("input")
        |> Array.map int
        |> List.ofArray

    let outlet, device, adapters = 0, 3 + List.max input, input

    let joltDiffs =
        outlet::device::adapters
        |> List.sort
        |> List.pairwise
        |> List.map (fun (x,y) -> y-x)
        |> List.groupBy id
        |> List.map (fun (i,lst) -> (i, lst.Length))
        |> Map.ofList

    joltDiffs.[1] * joltDiffs.[3]
    |> printfn "%i"

    0 // return an integer exit code
