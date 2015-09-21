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
    let advantageDisadvantage = Set<_>['A'; 'D'; 'a'; 'd']
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
        | Number(threshold, Next('?', Number(critThreshold, Next('?', next)))) -> 
            Some(Check(Single(Simple(1, 20)), [critThreshold, Single(Simple(2, 1)); threshold, Single(Simple(1, 1)) ], 0), next)
        | Number(threshold, Next('?', next)) -> 
            Some(Check(Single(Simple(1, 20)), [threshold, Single(Simple(1, 1))], 0), next)
        | SumSimplesExpression(v, next) -> Some(v, next)
        | _ -> None
    and (|SumSimplesExpression|_|) input = 
        // Use helper for recursion so sum can be left-associative
        let rec (|Helper|_|) positive = function
            | SimpleExpression(lhs, next) ->
                let lhs = Single(lhs)
                let lhs = if positive then lhs
                          else MultByConstant(-1, lhs)
                match next with
                | Next('+', Helper true (rhs : Compound list, next : string * int)) ->
                    Some(lhs :: rhs, next)
                | Next('-', Helper false (rhs, next)) ->
                    Some(lhs :: rhs, next)
                | _ ->
                    Some([lhs], next)
            | _ -> None
        match input with
        | Helper true (terms, next) ->
            let node = 
                match terms with 
                | [term] -> term
                | head::tail -> tail |> List.fold (fun lhs rhs -> Sum(lhs, rhs)) head
                | _ -> Util.nomatch()
            Some(node, next)
        | _ -> None
    and (|SimpleExpression|_|) input = 
        let makeRoll n d input = 
            match input with
            | Char advantageDisadvantage (Char arithmeticOperators _) as rest ->
                let (s, i) = input
                let roll = match s.[i] with
                            | 'A' | 'a' -> Adv(n, d)
                            | 'D' | 'd' -> Disadv(n, d)
                            | _ -> Util.nomatch()
                Some (roll, (s, i+1))
            | Char advantageDisadvantage Empty ->
                let (s, i) = input
                let roll = match s.[i] with
                            | 'A' | 'a' -> Adv(n, d)
                            | 'D' | 'd' -> Disadv(n, d)
                            | _ -> Util.nomatch()
                Some (roll, (s, i+1))
            | rest -> Some(Simple(n, d), rest)
        match input with
        | Next('d', Number(dieSize, next)) -> makeRoll 1 dieSize next
        | Number (n, Next('d', Number(dieSize, next))) -> makeRoll n dieSize next
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