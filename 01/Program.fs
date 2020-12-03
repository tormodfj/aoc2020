// Learn more about F# at http://fsharp.org

open System
open System.IO

[<EntryPoint>]
let main argv =
    let input:int List = 
        File.ReadAllLines("input")
        |> Array.map Int32.Parse
        |> List.ofArray

    let isMatch needle haystack =
        haystack |> List.contains (2020 - needle)

    let rec solve list =
        match list with
        | head::tail when isMatch head tail -> head * (2020 - head)
        | [] -> 0
        | head::tail -> solve tail

    input
    |> solve
    |> printf "%i"
        
    0 // return an integer exit code
