open System.IO
open System.Text.RegularExpressions

type Rule =
    | Character of string
    | RuleRef of int
    | RuleRefs of int * int
    | Either of Rule * Rule
module Rule =

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let ofString str =
        match str with
        | Regex "(\d+): \"(.)\"$" [ ruleNo; char ] 
            -> (int ruleNo, Character(char))
        | Regex "(\d+): (\d+)$" [ ruleNo; ref] 
            -> (int ruleNo, RuleRef(int ref))
        | Regex "(\d+): (\d+) (\d+)$" [ ruleNo; ref1; ref2 ] 
            -> (int ruleNo, RuleRefs(int ref1, int ref2))
        | Regex "(\d+): (\d+) \| (\d+)" [ ruleNo; ref1; ref2 ]
            -> (int ruleNo, Either(RuleRef(int ref1), RuleRef(int ref2)))
        | Regex "(\d+): (\d+) (\d+) \| (\d+) (\d+)" [ ruleNo; ref1; ref2; ref3; ref4 ]
            -> (int ruleNo, Either(RuleRefs(int ref1, int ref2), RuleRefs(int ref3, int ref4)))
        | _ -> failwithf "Unrecognized rule: %s" str

[<EntryPoint>]
let main _ =

    let input = File.ReadAllLines("input")

    let rules =
        input
        |> Seq.takeWhile (fun line -> line <> "")
        |> Seq.map Rule.ofString
        |> Map.ofSeq

    let expand ruleNo =
        let rec expandRuleNo i = expandRule rules.[i]
        and expandRule r =
            match r with
            | Character(c) -> seq { c }
            | RuleRef(ruleNo) -> expandRuleNo ruleNo
            | RuleRefs(ruleNo1, ruleNo2) -> 
                Seq.allPairs (expandRuleNo ruleNo1) (expandRuleNo ruleNo2)
                |> Seq.map (fun (a,b) -> a+b)
            | Either(rule1, rule2) ->
                Seq.concat [(expandRule rule1);(expandRule rule2)]

        expandRuleNo ruleNo
        |> Set.ofSeq

    let validMessagesForRule0 = expand 0

    input
    |> Seq.skipWhile (fun line -> line <> "")
    |> Seq.skip 1
    |> Seq.filter (fun msg -> validMessagesForRule0 |> Set.contains msg)
    |> Seq.length
    |> printfn "%i"

    0 // return an integer exit code
