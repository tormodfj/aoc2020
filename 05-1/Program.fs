open System
open System.IO

type Seat = { Row: int; Column: int }

[<EntryPoint>]
let main argv =

    let input = File.ReadAllLines("input")
    
    let parseSeat (s:string) =
        let binary = s.Replace("F", "0").Replace("B", "1").Replace("L", "0").Replace("R", "1")
        { Row = Convert.ToInt32(binary.[..6], 2); 
          Column = Convert.ToInt32(binary.[7..], 2) }
    
    let seatId (seat:Seat) =
        seat.Row * 8 + seat.Column

    input
    |> Array.map (parseSeat >> seatId)
    |> Array.max
    |> printf "%i"

    0 // return an integer exit code
