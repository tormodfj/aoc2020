open System
open System.IO
open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

type Machine =
    { Memory : Map<int64,int64>
      Bitmask : string }

type Instruction =
    | SetBitmask of string
    | StoreValue of int64 * int64

[<EntryPoint>]
let main _ =

    let input = File.ReadAllLines("input")
    let machine = { Memory = Map.empty; Bitmask = String.Empty }

    let parse line =
        match line with
        | Regex "mask = (.*)" [ mask ] -> SetBitmask mask
        | Regex "mem\[(\d+)\] = (\d+)" [ addr; value ] -> StoreValue (int64 addr, int64 value)
        | _ -> failwith "Unrecognized instruction"

    let getAddresses (mask:string) (baseAddress:int64) =
        let binaryBaseAddress = Convert.ToString(baseAddress, 2).PadLeft(36, '0')
        let floatingAddress =
            mask
            |> Seq.zip binaryBaseAddress
            |> Seq.map (fun (b,m) -> if m = '0' then b else m)
            |> List.ofSeq
        
        let rec generateFromFloating address = // not tail recursive...
            match address with
            | [] -> [""]
            | c::tail when c = 'X' -> ['0';'1'] |> List.collect (fun c -> 
                (generateFromFloating tail) |> List.map (fun rest -> String.Concat(c, rest)))
            | c::tail -> 
                (generateFromFloating tail) |> List.map (fun rest -> String.Concat(c, rest))

        floatingAddress
        |> generateFromFloating
        |> List.map (fun binary -> Convert.ToInt64(binary, 2))

    let setBitmask mask m = 
        { m with Bitmask = mask }
        
    let storeValue addr (value:int64) m =
        let updatedMemory =
            addr
            |> getAddresses m.Bitmask
            |> List.fold (fun mem addr -> mem |> Map.add addr value) m.Memory
        { m with Memory = updatedMemory }

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
