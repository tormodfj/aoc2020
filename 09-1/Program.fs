open System
open System.IO

[<EntryPoint>]
let main _ =

    let input = 
        File.ReadAllLines("input")
        |> Array.map Int64.Parse
        |> List.ofArray

    let preamble, numbers = input.[..24], input.[25..]
    let push element list = (element::list).[..24]
    let containsSum sum list =
        list |> List.exists (fun x -> 
        list |> List.exists (fun y -> 
            x <> y && x + y = sum))
    
    let rec findError prev numbers =
        match numbers with
        | head::tail when prev |> containsSum head -> 
            findError (prev |> push head) tail
        | head::_ -> head
        | _ -> failwith "No illegal number found"
    
    findError preamble numbers
    |> printfn "%i"

    0 // return an integer exit code
