open System
open System.IO

[<EntryPoint>]
let main argv =

    let input = File.ReadAllText("input")
    let groups = input.Split("\n\n")

    let count (group:string) =
        set [for c in group.Replace("\n", "") -> c]
        |> Set.count
    
    groups
    |> Array.sumBy count 
    |> printfn "%i"

    0 // return an integer exit code
