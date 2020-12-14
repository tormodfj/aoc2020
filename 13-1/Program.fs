open System
open System.IO

[<EntryPoint>]
let main _ =

    let input = File.ReadAllLines("input")

    let earliest = int input.[0]
    let parseId (id:string) =
        match Int32.TryParse id with
        | true,int -> Some int
        | _ -> None
    let firstDeparture id =
        (id, earliest+id - earliest%id)

    let (id, departure) =
        input.[1].Split(',')
        |> List.ofArray
        |> List.choose parseId
        |> List.map firstDeparture 
        |> List.minBy (fun (_,departure) -> departure)

    id * (departure - earliest)
    |> printfn "%i"

    0 // return an integer exit code
