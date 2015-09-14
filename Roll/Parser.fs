[<WebSharper.Core.Attributes.JavaScript>]
module mdw.Parser 

open DataDefs

[<AutoOpen>]
module Impl =
    (* Throughout this file we use string * int as a data type for parse inputs,
    representing a string and a position within it. I can't figure out how to get
    the type inference to work with type aliases though so I'm just using a raw
    string * int tuple. Wherever you see "string * int" in a type, think "parser
    input."
    *)
    let alpha = Set<char>['A'..'Z'] + Set<char>['a'..'z']
    let numeric = Set<char>['0'..'9']
    let arithmeticOperators = Set<_>['+'; '-']
    let alphanumeric = alpha + numeric

    let (|Next|Empty|) = function
        | (input : string), pos when pos < input.Length -> Next(input.[pos], (input, pos+1))
        | _ -> Empty

    let (|Char|_|) alphabet = function
        | Empty -> None
        | s, i when Set.contains s.[i] alphabet -> Some(s, i+1)
        | _ -> None

    let rec (|MaybeChars|) alphabet = function
        | Char alphabet (MaybeChars alphabet it)
        | it -> it

    let rec (|Chars|_|) alphabet = function
        | Char alphabet (MaybeChars alphabet it) -> Some it
        | it -> None

    let sub (s: string, i0) (_, i1) =
        s.Substring(i0, i1-i0)

    let (|NextWord|_|) (word : string) input =
        let letters = word |> List.ofSeq
        let rec (|NextChar|_|) letters = function
            | Next(x, rest) when x = List.head letters ->
                match List.tail letters with
                | [] -> Some rest
                | letters -> 
                    match rest with
                    | NextChar letters rest -> Some rest
                    | _ -> None
            | _ -> None
        match input with
        | NextChar letters rest -> Some rest
        | _ -> None

    let rec (|Number|_|) = function
        | Chars numeric i1 as i0 -> (System.Int32.Parse(sub i0 i1), i1) |> Some
        | _ -> None
    and (|CompoundExpression|_|) = function
        | Number(n, Next('.', CompoundExpression(v, next))) -> 
            Some(Repeat(n, v), next)
        | SumSimplesExpression(v, next) -> Some(v, next)
        | _ -> None
    and (|SumSimplesExpression|_|) = function
        | SimpleExpression(lhs, next) ->
            match next with
            | Next('+', (SumSimplesExpression(rhs, next))) ->
                Some(Sum(Single(lhs), rhs), next)
            | Next('-', (SumSimplesExpression(rhs, next))) ->
                let reverseFirst clause = MultByConstant(-1, clause)
                Some(Sum(Single(lhs), reverseFirst rhs), next)
            | _ ->
                Some(Single(lhs), next)
        | SimpleExpression(lhs, Next('-', (SumSimplesExpression(rhs, next)))) ->
            Some(Sum(Single(lhs), rhs), next)
        | SimpleExpression(lhs, next) ->
            Some(Single(lhs), next)
        | _ -> None
    and (|SimpleExpression|_|) = function
        | Next('d', Number(dieSize, next)) -> Some (Simple(1,dieSize), next)
        | Number (n, Next('d', Number(dieSize, next))) -> Some (Simple(n, dieSize), next)
        | Number (n, Next('d', next)) -> Some (Simple(n, 6), next)
        | Number (n, next) -> Some (Simple(n, 1), next)
        | _ -> None
    and (|CommandExpression|_|) = function
        | NextWord "avg." (CompoundExpression(v, next)) -> Some (Average(v), next)
        | CompoundExpression(v, next) -> Some (Roll(v), next)
        | _ -> None
        
    let parseCompound txt =
        match (txt, 0) with
        | CompoundExpression(cmd, Empty) -> cmd
        | _ -> failwithf "failed to parse '%s'" txt

    let parseCommand txt =
        match (txt, 0) with
        | CommandExpression(cmd, Empty) -> cmd
        | _ -> failwithf "failed to parse '%s'" txt

let Parse txt = 
    parseCompound txt
let ParseCommand = parseCommand