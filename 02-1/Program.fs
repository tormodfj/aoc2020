open System
open System.IO
open System.Text.RegularExpressions

type Line = { Min: int; Max: int; Char: char; Pass: string }

[<EntryPoint>]
let main argv =

    let parseLine line =
        let pattern = "(\d+)-(\d+) (.): (.+)"
        let m = Regex.Match(line, pattern)
        let [min;max;char;pass] = List.tail [for g in m.Groups -> g.Value]
        { Min = Int32.Parse min; Max = Int32.Parse max; Char = char.[0]; Pass = pass}

    let isValid line =
        let count =
            line.Pass
            |> Seq.filter (fun c -> c = line.Char)
            |> Seq.length
        line.Min <= count && count <= line.Max

    File.ReadAllLines("input")
    |> Array.map parseLine
    |> Array.filter isValid
    |> Array.length
    |> printf "%i"

    0 // return an integer exit code
