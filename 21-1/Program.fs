open System.IO
open System.Text.RegularExpressions

type Product = { Ingredients:Set<string>; Allergens:Set<string> }
module Product =

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let ofString str =
        match str with
        | Regex "(.*) \(contains (.*)\)" [ ingredients; allergens ] ->
            { Ingredients = ingredients.Split(" ") |> set
              Allergens = allergens.Split(", ") |> set }
        | _ -> failwithf "Unrecognized product: %s" str

[<EntryPoint>]
let main _ =

    let products =
        File.ReadAllLines("input")
        |> List.ofArray
        |> List.map Product.ofString

    let allAllergens =
        products
        |> List.map (fun { Allergens = a } -> a)
        |> Set.unionMany

    let allergenMap =
        allAllergens
        |> Seq.map (fun allergen ->
            let candidateIngredients =
                products
                |> List.filter (fun { Allergens=allergens} -> allergens |> Set.contains allergen)
                |> List.map (fun { Ingredients=ingredients } -> ingredients)
                |> Set.intersectMany
            (allergen, candidateIngredients))

    let allergenIngredients =
        allergenMap
        |> Seq.map (fun (_,allergens) -> allergens)
        |> Set.unionMany
            
    products
    |> List.collect (fun { Ingredients = i } -> List.ofSeq i)
    |> List.filter (fun s -> allergenIngredients |> Set.contains s |> not)
    |> List.length
    |> printfn "%A"

    0 // return an integer exit code
