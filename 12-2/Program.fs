open System
open System.IO
open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

type Ship = 
    { Position:int*int
      Waypoint:int*int }

type Action =
    | MoveWaypoint of int*int
    | RotateWaypoint of int
    | SailTowardsWaypoint of int

[<EntryPoint>]
let main _ =

    let input = File.ReadAllLines("input")

    let ship =
        { Position = (0,0)
          Waypoint = (10,1) }
    
    let parse line =
        match line with
        | Regex "N(\d+)" [ north ] -> MoveWaypoint(0, int north)
        | Regex "S(\d+)" [ south ] -> MoveWaypoint(0, -int south)
        | Regex "E(\d+)" [ east ] -> MoveWaypoint(int east, 0)
        | Regex "W(\d+)" [ west ] -> MoveWaypoint(-int west, 0)
        | Regex "R(\d+)" [ right ] -> RotateWaypoint(int right)
        | Regex "L(\d+)" [ left ] -> RotateWaypoint(360 - int left)
        | Regex "F(\d+)" [ times ] -> SailTowardsWaypoint(int times)
        | _ -> failwith "Unknown action"

    let perform action ship =
        let moveWaypoint ship (dx,dy) =
            let (x,y) = ship.Waypoint
            { ship with Waypoint = (x+dx,y+dy) }
        let rec rotateWaypointCW ship times =
            if times <= 0 then ship
            else
                let (dx,dy) = ship.Waypoint
                rotateWaypointCW { ship with Waypoint = (dy,-1*dx) } (times-1)
        let sailTowardsWaypoint ship times =
            let (x,y),(dx,dy) = ship.Position,ship.Waypoint
            { ship with Position = (x+times*dx,y+times*dy) }

        match action with
        | MoveWaypoint (dx,dy) -> moveWaypoint ship (dx,dy)
        | RotateWaypoint deg -> rotateWaypointCW ship (deg/90)
        | SailTowardsWaypoint dist -> sailTowardsWaypoint ship dist

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
