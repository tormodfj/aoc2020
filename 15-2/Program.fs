open System.IO

[<EntryPoint>]
let main _ =

    let input = 
        File.ReadAllText("input").Split(',')
        |> Array.mapi (fun i num -> (int num, i+1))

    let nthNumber (n:int) (input:(int*int) array) =
        let rec solve (prevCallMap:Map<int,int>) (last:int) (turn:int) =
            let nth =
                match prevCallMap |> Map.tryFind last with
                | Some(prev) -> (turn-1) - prev
                | None -> 0
            if turn = n then nth
            else solve (prevCallMap |> Map.add last (turn-1)) nth (turn+1)
        
        let (lastNum, lastTurn) = Array.last input
        solve (Map.ofArray input) lastNum (lastTurn+1)

    input
    |> nthNumber 30000000
    |> printfn "%i"

    0 // return an integer exit code
