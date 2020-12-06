open System
open System.IO

[<EntryPoint>]
let main argv =

    let input = File.ReadAllText("input")
    let groups = input.Split("\n\n")

    let count (group:string) =
        group.Split("\n")
        |> Array.map Set.ofSeq
        |> Set.intersectMany
        |> Set.count
    
    groups
    |> Array.sumBy count 
    |> printfn "%i"

    0 // return an integer exit code
