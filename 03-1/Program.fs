open System.IO

[<EntryPoint>]
let main argv =

    let input = File.ReadAllLines("input")
    let height = input.Length
    let width = input.[0].Length
    let position = (0,0)

    let move (x,y) = 
        ((x + 3) % width, y + 1)

    let rec countTrees acc position =
        
        let increaseIfTree n (x,y) =
            if input.[y].[x] = '#' then
                n + 1
            else
                n
        
        let nextPosition = position |> move

        match position with
        | (_,y) when y = height -> acc
        | p -> countTrees (increaseIfTree acc p) nextPosition

    countTrees 0 position
    |> printf "%i"

    0 // return an integer exit code
