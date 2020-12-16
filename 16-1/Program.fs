open System.IO
open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

type Field = { Name:string; Validate:int -> bool }
type Ticket = Ticket of int list

[<EntryPoint>]
let main _ =

    let input = 
        File.ReadAllLines("input")
        |> List.ofArray

    let parseField (field:string) =
        match field with
        | Regex "(.*): (\d+)-(\d+) or (\d+)-(\d+)" [ name; n1s; n2s; m1s; m2s ] ->
            let n1,n2,m1,m2 = int n1s, int n2s, int m1s, int m2s
            { Name = name; Validate = fun i -> (n1<=i && i<=n2) || (m1<=i && i<=m2) }
        | _ -> failwith "Unrecognized field"
    
    let parseTicket (ticket:string) =
        ticket.Split(',')
        |> List.ofArray
        |> List.map int
        |> Ticket

    let fields =
        input
        |> List.takeWhile (fun line -> line <> "")
        |> List.map parseField

    let nearbyTickets =
        input
        |> List.skipWhile (fun line -> line <> "nearby tickets:")
        |> List.skip 1
        |> List.map parseTicket

    let getInvalidValues ticket =
        let (Ticket values) = ticket
        let isValid (value:int) =
            fields
            |> List.exists (fun f -> f.Validate value)
        values
        |> List.filter (isValid >> not)

    nearbyTickets
    |> List.collect getInvalidValues
    |> List.sum
    |> printfn "%i"

    0 // return an integer exit code
