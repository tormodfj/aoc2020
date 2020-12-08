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
    | Nop of int

[<EntryPoint>]
let main _ =

    let input = File.ReadAllLines("input")

    let parse line =
        match line with
        | Regex "acc (.*)" [ value ] -> value |> Int32.Parse |> Acc
        | Regex "jmp (.*)" [ value ] -> value |> Int32.Parse |> Jmp
        | Regex "nop (.*)" [ value ] -> value |> Int32.Parse |> Nop
        | _ -> failwith "Unknown instruction"

    let program = 
        input
        |> Array.map parse

    let execute (ops:Op array) =
        let rec eval (ip:int) (acc:int) (visited:Set<int>) (ops:Op array) (flip:int) =
            let evalAcc dAcc = eval (ip + 1)   (acc + dAcc) (visited.Add ip) ops flip
            let evalJmp dIp  = eval (ip + dIp) (acc)        (visited.Add ip) ops flip
            let evalNop ()   = eval (ip + 1)   (acc)        (visited.Add ip) ops flip

            if ip = ops.Length then Some(acc)
            else match ops.[ip] with
                 | _ when visited.Contains ip -> None // Loop detected
                 | Acc value -> evalAcc value
                 | Nop value when ip = flip -> evalJmp value
                 | Jmp _ when ip = flip -> evalNop ()
                 | Jmp value -> evalJmp value
                 | Nop _ -> evalNop ()

        [0..(ops.Length - 1)]
        |> List.pick (eval 0 0 (set[]) ops)
    
    program
    |> execute
    |> printfn "%i"

    0 // return an integer exit code
