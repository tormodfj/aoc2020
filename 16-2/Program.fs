open System.IO
open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

type Field = 
    { Index:int
      IndexCandidates:int list
      Name:string
      Validate:int -> bool }
module Field =
    let ofString (field:string) =
        match field with
        | Regex "(.*): (\d+)-(\d+) or (\d+)-(\d+)" [ name; n1s; n2s; m1s; m2s ] ->
            let n1,n2,m1,m2 = int n1s, int n2s, int m1s, int m2s
            { Index = -1
              IndexCandidates = [] 
              Name = name 
              Validate = fun i -> (n1<=i && i<=n2) || (m1<=i && i<=m2) }
        | _ -> failwith "Unrecognized field"

type Ticket = 
    Ticket of int list
module Ticket =
    let ofString (ticket:string) =
        ticket.Split(',')
        |> List.ofArray
        |> List.map int
        |> Ticket
 
[<EntryPoint>]
let main _ =

    let input = 
        File.ReadAllLines("input")
        |> List.ofArray

    let fields =
        input
        |> List.takeWhile (fun line -> line <> "")
        |> List.map Field.ofString
    
    let myTicket =
        input
        |> List.skipWhile (fun line -> line <> "your ticket:")
        |> List.skip 1
        |> List.head
        |> Ticket.ofString

    let nearbyTickets =
        input
        |> List.skipWhile (fun line -> line <> "nearby tickets:")
        |> List.skip 1
        |> List.map Ticket.ofString

    let isValid ticket =
        let (Ticket values) = ticket
        let isValid (value:int) =
            fields
            |> List.exists (fun f -> f.Validate value)
        values 
        |> List.forall isValid

    let valuesByIndex =
        nearbyTickets
        |> List.filter isValid
        |> List.map (fun (Ticket v) -> v)
        |> List.transpose
        |> List.mapi (fun i values -> (i, values))

    let determineIndexCandidates field =
        let indexCandidates =
            valuesByIndex
            |> List.filter (fun (_,values) -> values |> List.forall field.Validate)
            |> List.map (fun (i,_) -> i)
        { field with IndexCandidates = indexCandidates }

    let determineIndexes fields =
        let candidateCount field = List.length field.IndexCandidates
        let (indexedFields,_) =
            fields 
            |> List.sortBy candidateCount
            |> List.mapFold (fun indexesTaken field ->
                let index = 
                    field.IndexCandidates 
                    |> List.except indexesTaken 
                    |> List.exactlyOne
                ({ field with Index = index }, index::indexesTaken)) []
        indexedFields
    
    let departureFieldIndices =
        fields
        |> List.map determineIndexCandidates
        |> determineIndexes
        |> List.filter (fun field -> field.Name.StartsWith("departure"))
        |> List.map (fun field -> field.Index)

    let (Ticket myValues) = myTicket

    departureFieldIndices
    |> List.map ((fun i -> myValues |> List.item i) >> int64) 
    |> List.reduce (*)
    |> printfn "%i"

    0 // return an integer exit code
