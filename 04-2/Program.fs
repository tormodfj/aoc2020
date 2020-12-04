open System
open System.IO
open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

[<EntryPoint>]
let main argv =

    let input = File.ReadAllText("input")
    
    let passportFromString (str:string) =
        str.Split([| "\n"; " " |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun field -> field.Split(':').[0], field.Split(':').[1])
        |> Map.ofArray

    let isFieldValid key value =
        let isBetween lo hi num = lo <= num && num <= hi

        let isByrValid v = Int32.Parse v |> isBetween 1920 2002
        let isIyrValid v = Int32.Parse v |> isBetween 2010 2020
        let isEyrValid v = Int32.Parse v |> isBetween 2020 2030
        let isHgtValid v =
            match v with
            | Regex "(\d+)cm" [ num ] -> Int32.Parse num |> isBetween 150 193
            | Regex "(\d+)in" [ num ] -> Int32.Parse num |> isBetween 59 76
            | _ -> false
        let isHclValid v = Regex.IsMatch(v, "^#[0-9a-f]{6}$")
        let isEclValid v = set ["amb";"blu";"brn";"gry";"grn";"hzl";"oth"] |> Set.contains v
        let isPidValid v = Regex.IsMatch(v, "^[0-9]{9}$")

        value 
        |> match key with
            | "byr" -> isByrValid
            | "iyr" -> isIyrValid
            | "eyr" -> isEyrValid
            | "hgt" -> isHgtValid
            | "hcl" -> isHclValid
            | "ecl" -> isEclValid
            | "pid" -> isPidValid
            | _ -> (fun _ -> true)

    let isValid (passport:Map<string,string>) =
        let hasRequiredFields = 
            set [ "byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid" ]
            |> Set.forall passport.ContainsKey

        let hasValidFields =
            passport 
            |> Map.forall isFieldValid

        hasRequiredFields && hasValidFields

    input.Split([| "\n\n" |], StringSplitOptions.None)
    |> Array.map passportFromString
    |> Array.filter isValid
    |> Array.length
    |> printf "%i"

    0 // return an integer exit code
