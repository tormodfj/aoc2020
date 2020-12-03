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
        let fst = line.Pass.[line.Min - 1]
        let snd = line.Pass.[line.Max - 1]
        (fst = line.Char || snd = line.Char) && (fst <> snd)

    File.ReadAllLines("input")
    |> Array.map parseLine
    |> Array.filter isValid
    |> Array.length
    |> printf "%i"

    0 // return an integer exit code
