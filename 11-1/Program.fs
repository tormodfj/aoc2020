open System.IO

type Tile = | Floor | EmptySeat | OccupiedSeat

[<EntryPoint>]
let main _ =

    let input = File.ReadAllLines("input")

    let parseLine line =
        line
        |> Array.ofSeq
        |> Array.map (fun tile ->
            match tile with | '.' -> Floor | 'L' -> EmptySeat | '#' -> OccupiedSeat | _ -> failwith "Unrecognized tile")

    let initialFloor = input |> Array.map parseLine

    let getTile (floor:Tile array array) x y dx dy =
        match x+dx,y+dy with
        | x,_ when x < 0 || x >= floor.[0].Length -> Floor
        | _,y when y < 0 || y >= floor.Length -> Floor
        | x,y -> floor.[y].[x]

    let occupiedAdjacentSeats (floor:Tile array array) x y  =
        let t = getTile floor x y
        [ t -1 -1; t 0 -1; t +1 -1
          t -1 0;          t +1 0
          t -1 +1; t 0 +1; t +1 +1 ]
        |> List.filter (fun t -> t = OccupiedSeat)
        |> List.length

    let simulate (floor:Tile array array) =
        let occ = occupiedAdjacentSeats floor
        [| for y in [0..floor.Length-1] -> 
            [| for x in [0..floor.[y].Length-1] -> 
                match floor.[y].[x] with
                | EmptySeat when 0 = occ x y -> OccupiedSeat
                | OccupiedSeat when 4 <= occ x y -> EmptySeat
                | tile -> tile
            |]
        |]

    let rec simulateUntilStable prev current =
        if prev = current then current
        else simulateUntilStable current (simulate current)

    simulateUntilStable initialFloor (simulate initialFloor)
    |> Array.concat
    |> Array.filter (fun t -> t = OccupiedSeat)
    |> Array.length
    |> printfn "%i"

    0 // return an integer exit code
