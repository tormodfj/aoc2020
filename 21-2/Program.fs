open System
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

module String =
    let join (separator:char) (strings:string seq) =
        String.Join(separator, strings)

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

    let allergenCandidateMap =
        allAllergens
        |> Seq.map (fun allergen ->
            let candidateIngredients =
                products
                |> List.filter (fun { Allergens=allergens} -> allergens |> Set.contains allergen)
                |> List.map (fun { Ingredients=ingredients } -> ingredients)
                |> Set.intersectMany
            (allergen, candidateIngredients))
        |> Seq.sortBy (fun (_,ingredients) -> Set.count ingredients)

    let allergenMap =
        let rec solve acc determined rest =
            let sorted = 
                rest
                |> List.map (fun (a,is) -> (a,is - determined))
                |> List.sortBy (fun (_,is) -> Seq.length is)
            match sorted with
            | [] -> acc
            | (a,is)::tail -> 
                let i = is |> Seq.exactlyOne
                solve ((a,i)::acc) (determined |> Set.add i) tail

        allergenCandidateMap
        |> List.ofSeq
        |> solve [] Set.empty
    
    allergenMap
    |> Seq.sortBy (fun (a,_) -> a)
    |> Seq.map (fun (_,i) -> i)
    |> String.join ','
    |> printfn "%s"

    0 // return an integer exit code
