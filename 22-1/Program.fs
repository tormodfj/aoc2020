open System.IO

[<EntryPoint>]
let main _ =

    let input = File.ReadAllText "input"
    let players = input.Split("\n\n")

    let parse (str:string) =
        str.Split("\n")
        |> List.ofArray
        |> List.tail
        |> List.map int

    let player1 = players.[0] |> parse
    let player2 = players.[1] |> parse

    let rec getWinnersDeck p1 p2 =
        match p1,p2 with
        | [],winner -> winner
        | winner,[] -> winner
        | h1::t1,h2::t2 ->
            if h1>h2 then
                getWinnersDeck (t1@[h1;h2]) t2
            else
                getWinnersDeck t1 (t2@[h2;h1])

    let getScore player =
        player
        |> List.rev
        |> List.mapi (fun i card -> (i+1)*card)
        |> List.sum

    getWinnersDeck player1 player2
    |> getScore
    |> printfn "%i"

    0 // return an integer exit code
