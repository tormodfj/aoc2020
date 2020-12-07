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
            | Regex "(\d+) (.*) bag" [ count; color ] -> Some(Int32.Parse count, color)
            | _ -> None
        match line with
        | Regex "(.*) bags contain no other bags" [ color ] -> Some(color, [||])
        | Regex "(.*) bags contain (.*)\." [color; content] -> Some(color, content.Split(",") |> Array.choose parseContent)
        | _ -> None

    let bags =
        input
        |> Array.choose parse
        |> Map.ofArray 

    let rec countBags bagColor =
        let innerCount (count, color) = count + count * countBags color
        bags.[bagColor] 
        |> Array.sumBy innerCount

    countBags "shiny gold"
    |> printfn "%i"
    
    0 // return an integer exit code
