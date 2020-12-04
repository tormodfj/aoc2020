open System
open System.IO

[<EntryPoint>]
let main argv =

    let input = File.ReadAllText("input")
    
    let passportFromString (str:string) =
        str.Split([| "\n"; " " |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun field -> field.Split(':').[0])
        |> Set.ofArray

    let isValid passport =
        let requiredFields = set [ "byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid" ]
        Set.isSuperset passport requiredFields

    input.Split([| "\n\n" |], StringSplitOptions.None)
    |> Array.map passportFromString
    |> Array.filter isValid
    |> Array.length
    |> printf "%i"

    0 // return an integer exit code
