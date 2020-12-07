open System
open System.IO
open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

[<EntryPoint>]
let main argv =

    let input = File.ReadAllLines("input")

    let parse line =
        let parseContent content =
            match content with
            | Regex "\d+ (.*) bag" [ color ] -> Some(color)
            | _ -> None
        match line with
        | Regex "contain no other bags" _ -> None
        | Regex "(.*) bags contain (.*)\." [color; content] -> Some(color, content.Split(",") |> Array.choose parseContent)
        | _ -> None

    let bags =
        input
        |> Array.choose parse
        |> Map.ofArray 

    let rec canContainShinyGold _ content =
        let canItselfContain = content |> Array.contains "shiny gold"
        let canContentContain bag =
            match bags.TryGetValue(bag) with
            | (true, contents) -> canContainShinyGold bag contents
            | _ -> false
        canItselfContain || (content |> Array.exists canContentContain)

    bags
    |> Map.filter canContainShinyGold
    |> Map.count
    |> printfn "%i"

    0 // return an integer exit code
