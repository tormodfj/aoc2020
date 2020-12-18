open System.IO
open FParsec

type Expr =
    | Int of int64
    | Add of Expr * Expr
    | Mul of Expr * Expr
module Expr =
    // Cheating slightly by using FParsec's OperatorPrecedenceParser:
    // https://www.quanttec.com/fparsec/reference/operatorprecedenceparser.html#members.OperatorPrecedenceParser
    let private opp = new OperatorPrecedenceParser<Expr, unit, unit>()

    let private ws = spaces
    let private str s = pstring s .>> ws
    let private exprParser = opp.ExpressionParser
    let private intParser = pint64 .>> ws |>> Int
    let private parenParser = between (str "(") (str ")") exprParser

    opp.TermParser <- intParser <|> parenParser
    opp.AddOperator(InfixOperator("+", ws, 2, Associativity.Left, (), (fun _ l r -> Add(l,r))))
    opp.AddOperator(InfixOperator("*", ws, 1, Associativity.Left, (), (fun _ l r -> Mul(l,r))))

    let private parser = exprParser .>> eof

    let ofString str =
        match run parser str with
        | Success(result,_,_) -> result
        | Failure(error,_,_) -> failwith error

[<EntryPoint>]
let main _ =

    let input = File.ReadAllLines("input")

    let rec eval expr =
        match expr with
        | Int(int) -> int
        | Add(x,y) -> (eval x)+(eval y)
        | Mul(x,y) -> (eval x)*(eval y)

    input
    |> Seq.sumBy (Expr.ofString >> eval)
    |> printfn "%i"

    0 // return an integer exit code
