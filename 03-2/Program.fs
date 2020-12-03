open System.IO

[<EntryPoint>]
let main argv =

    let input = File.ReadAllLines("input")
    let height = input.Length
    let width = input.[0].Length
    let position = (0,0)

    let move (dx,dy) (x,y) = 
        ((x + dx) % width, y + dy)

    let rec countTrees acc position mv =
        
        let increaseIfTree n (x,y) =
            if input.[y].[x] = '#' then
                n + 1L
            else
                n
        
        let nextPosition = position |> mv

        match position with
        | (_,y) when y >= height -> acc
        | p -> countTrees (increaseIfTree acc p) nextPosition mv

    let solution =
        countTrees 0L position (move (1,1)) *
        countTrees 0L position (move (3,1)) *
        countTrees 0L position (move (5,1)) *
        countTrees 0L position (move (7,1)) *
        countTrees 0L position (move (1,2))
    
    printf "%i" solution

    0 // return an integer exit code
