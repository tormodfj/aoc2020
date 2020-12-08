open System
open System.IO
open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

type Op =
    | Acc of int
    | Jmp of int
    | Nop

[<EntryPoint>]
let main _ =

    let input = File.ReadAllLines("input")

    let parse line =
        match line with
        | Regex "acc (.*)" [ value ] -> value |> Int32.Parse |> Acc
        | Regex "jmp (.*)" [ value ] -> value |> Int32.Parse |> Jmp
        | Regex "nop" [] -> Nop
        | _ -> failwith "Unknown instruction"

    let program = 
        input
        |> Array.map parse

    let execute ops =
        let rec eval (ip:int) (acc:int) (visited:Set<int>) (ops:Op array) =
            match ops.[ip] with
            | _ when visited.Contains ip -> acc // Loop detected
            | Acc value -> 
                ops |> eval (ip + 1) (acc + value) (visited.Add ip)
            | Jmp value -> 
                ops |> eval (ip + value) (acc) (visited.Add ip)
            | Nop -> 
                ops |> eval (ip + 1) (acc) (visited.Add ip)
        ops 
        |> eval 0 0 (set[])
    
    program
    |> execute
    |> printfn "%i"

    0 // return an integer exit code
