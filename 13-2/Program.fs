open System
open System.IO

[<EntryPoint>]
let main _ =

    let input = File.ReadAllLines("input")

    let parseId (id:string) =
        match Int32.TryParse id with
        | true,int -> Some int
        | _ -> None

    let ids = input.[1].Split(',')

    let constraints =
        [0..ids.Length-1]
        |> List.choose (fun i ->
            match parseId ids.[i] with
            | Some id -> Some(int64 i, int64 id)
            | None -> None)
    
    let rec solve timestamp step constraints =
        // Assuming bus IDs are primes, we can brute-force for two IDs at a time
        // and easily re-calculate LCM (step) by multiplication (step*id)
        let isConstraintSatisfied (offset,id) =
            (timestamp + offset) % id = 0L

        match constraints with
        | [] -> timestamp
        | (offset, id)::rest when isConstraintSatisfied (offset, id) ->
            solve timestamp (step*id) rest
        | constraints ->
            solve (timestamp+step) (step) constraints

    constraints
    |> solve 0L 1L
    |> printfn "%i"      

    0 // return an integer exit code
