open System.IO

[<EntryPoint>]
let main _ =

    let input = 
        File.ReadAllLines("input")
        |> Array.map int
        |> List.ofArray

    let outlet, device, adapters = 0, 3 + List.max input, input
    let nodes = List.sort (outlet::device::adapters)
    let graph = 
        let getCandidates i =
            let isPredecessor x = x <= i
            let isCandidate x = x <= i + 3
            nodes
            |> List.skipWhile isPredecessor
            |> List.takeWhile isCandidate
        nodes 
        |> List.map (fun i -> (i, getCandidates i))

    let rec solve (pathCountMap:Map<int,int64>) graph =
        match graph with
        | [] -> pathCountMap.[0]    
        | (node, candidates)::tail -> 
            let pathCount = 
                candidates 
                |> List.sumBy (fun n -> pathCountMap.[n])
            solve (pathCountMap |> Map.add node pathCount) tail

    graph 
    |> List.rev 
    |> List.tail
    |> solve (Map[device, 1L]) 
    |> printfn "%i"

    0 // return an integer exit code
