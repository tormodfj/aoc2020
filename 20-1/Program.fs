open System
open System.IO

type Tile = { ID:int64; Edges:Set<string> }

module Tile =

    let flipEdges tile =
        let reverse edge =
            let reversed = edge |> Seq.rev |> Array.ofSeq
            String(reversed)
        { tile with Edges=tile.Edges |> Set.map reverse }

    let ofString (tile:string) =
        let lines = tile.Split("\n")
        let tile = lines.[1..10]
        let transposed =
            tile
            |> Array.map Array.ofSeq
            |> Array.transpose
            |> Array.map (fun cs -> String(cs))

        let id = lines.[0].Substring(5,4) |> int64
        let edges = set [ tile.[0]; tile.[9]; transposed.[0]; transposed.[9] ]

        { ID = id; Edges = edges }

[<EntryPoint>]
let main _ =

    let input = File.ReadAllText "input"

    let tiles =
        input.Split("\n\n")
        |> List.ofArray
        |> List.map Tile.ofString

    let countMatchingTiles (tile:Tile) =
        let allEdges tile =
            tile.Edges |> Set.union (Tile.flipEdges tile).Edges
        let ownEdges = allEdges tile
        let otherEdges = 
            tiles 
            |> List.except [tile]
            |> List.map allEdges

        otherEdges
        |> List.filter (fun edges ->
            edges
            |> Set.intersect ownEdges
            |> Set.isEmpty
            |> not)
        |> List.length

    // Only the corner tiles have just 2 matching tiles
    tiles
    |> List.filter (fun tile -> (countMatchingTiles tile) = 2)
    |> List.map (fun { ID = id } -> id)
    |> List.reduce (*)
    |> printfn "%i"

    0 // return an integer exit code
