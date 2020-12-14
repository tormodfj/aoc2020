open System
open System.IO
open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

type Ship = 
    { Position:int*int
      Heading:int*int }

type Action =
    | Move of int*int
    | Turn of int
    | GoForward of int

[<EntryPoint>]
let main _ =

    let input = File.ReadAllLines("input")

    let ship =
        { Position = (0,0)
          Heading = (1,0) }
    
    let parse line =
        match line with
        | Regex "N(\d+)" [ north ] -> Move(0, int north)
        | Regex "S(\d+)" [ south ] -> Move(0, -int south)
        | Regex "E(\d+)" [ east ] -> Move(int east, 0)
        | Regex "W(\d+)" [ west ] -> Move(-int west, 0)
        | Regex "R(\d+)" [ right ] -> Turn(int right)
        | Regex "L(\d+)" [ left ] -> Turn(360 - int left)
        | Regex "F(\d+)" [ dist ] -> GoForward(int dist)
        | _ -> failwith "Unknown action"

    let perform action ship =
        let move ship (dx,dy) =
            let (x,y) = ship.Position
            { ship with Position = (x+dx,y+dy) }
        let rec turnRight ship times =
            if times <= 0 then ship
            else
                let (dx,dy) = ship.Heading
                turnRight { ship with Heading = (dy,-1*dx) } (times-1)
        let goForward ship dist =
            let (x,y),(dx,dy) = ship.Position,ship.Heading
            { ship with Position = (x+dist*dx,y+dist*dy) }

        match action with
        | Move (dx,dy) -> move ship (dx,dy)
        | Turn deg -> turnRight ship (deg/90)
        | GoForward dist -> goForward ship dist

    let manhattanDist ship =
        let (x,y) = ship.Position
        Math.Abs x + Math.Abs y

    input
    |> List.ofArray
    |> List.map parse
    |> List.fold (fun ship action -> ship |> perform action) ship
    |> manhattanDist
    |> printfn "%i"

    0 // return an integer exit code
