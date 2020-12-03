// Learn more about F# at http://fsharp.org

open System
open System.IO

[<EntryPoint>]
let main argv =
    let input:int List = 
        File.ReadAllLines("input")
        |> Array.map Int32.Parse
        |> List.ofArray

    let productIfSum2020 x y z = 
        if x + y + z = 2020 then
            Some(x * y * z)
        else
            None

    let rec solveGivenXY x y list =
        match list with
        | z::tail ->
            match productIfSum2020 x y z with
            | None -> solveGivenXY x y tail
            | Some(x) -> Some(x)
        | [] -> None

    let rec solveGivenX x list =
        match list with
        | y::tail ->
            match solveGivenXY x y tail with
            | None -> solveGivenX x tail
            | Some(x) -> Some(x)
        | [] -> None

    let rec solve list =
        match list with
        | x::tail ->
            match solveGivenX x tail with
            | Some(solution) -> solution
            | None -> solve tail
        | [] -> 0
       
    input
    |> solve
    |> printf "%i"
    0 // return an integer exit code
