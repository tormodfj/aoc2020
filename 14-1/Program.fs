open System
open System.IO
open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

type Machine =
    { Memory : Map<int,int64>
      Bitmask : string }

type Instruction =
    | SetBitmask of string
    | StoreValue of int * int64

[<EntryPoint>]
let main _ =

    let input = File.ReadAllLines("input")
    let machine = { Memory = Map.empty; Bitmask = String.Empty }

    let parse line =
        match line with
        | Regex "mask = (.*)" [ mask ] -> SetBitmask mask
        | Regex "mem\[(\d+)\] = (\d+)" [ addr; value ] -> StoreValue (int addr, int64 value)
        | _ -> failwith "Unrecognized instruction"

    let setBitmask mask m = 
        { m with Bitmask = mask }
        
    let storeValue addr (value:int64) m =
        let mask = m.Bitmask
        let binValue = Convert.ToString(value, 2).PadLeft(36, '0')
        let newValue =
            mask
            |> Seq.zip binValue
            |> Seq.map (fun (v,m) -> if m = 'X' then v else m)
            |> String.Concat
        { m with Memory = Map.add addr (Convert.ToInt64(newValue, 2)) m.Memory }

    let execute instruction machine =
        match instruction with
        | SetBitmask mask -> machine |> setBitmask mask
        | StoreValue (addr,value) -> machine |> storeValue addr value

    let sumOfValuesInMemory { Memory = mem; Bitmask = _ } =
        mem
        |> Map.fold (fun sum _ value -> sum + value) 0L

    input
    |> List.ofArray
    |> List.map parse
    |> List.fold (fun m instruction -> m |> execute instruction) machine
    |> sumOfValuesInMemory
    |> printfn "%i"

    0 // return an integer exit code
