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
        
        // Assuming bus IDs are primes, we can brute-force for one constraint at a 
        // time. As long as we keep the step size equal to LCM of previous IDs, we
        // keep those constraints satisfied. LCM for a set of primes is simply the 
        // product, hence the (step*id)

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
