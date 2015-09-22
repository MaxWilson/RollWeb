[<WebSharper.Core.Attributes.JavaScript>]
module mdw.Parser 

#nowarn "40" // recursive object references from memoization

open DataDefs

type Impl() =
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
    let memo: Map<_,_> ref = ref (Map.empty)
    // memoization provides better perf (linear time) AND allows left-recursive grammars to terminate
    let memoize pattern input =
        let key = (pattern.ToString(), input)
        match Map.tryFind key !memo with
        | Some(v) -> v
        | None ->
            let rec growSeed prev =
                let result = pattern input
                memo := Map.add key result !memo
                match result, prev with
                | None, _  ->
                    prev
                | Some(_, new'), Some(_, prev') when new' <= prev' ->
                    prev
                | _ -> growSeed result
            memo := Map.add key None !memo // set initial seed to FAIL
            growSeed None

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
    and (|CompoundExpression|_|) = memoize (function
        | Next('(', CompoundExpressionSum(lhs, Next(')', next))) -> Some(lhs, next)
        | CompoundExpressionSum(lhs, next) -> Some(lhs, next)
        | _ -> None)
    and (|CompoundExpressionSum|_|) = memoize (function
        | CompoundExpressionSum(lhs, Next('+', CompoundExpression(rhs, next))) -> Some(Sum(lhs, rhs), next)
        | CompoundExpressionSum(lhs, Next('-', CompoundExpression(rhs, next))) -> Some(Sum(lhs, MultByConstant(-1, rhs)), next)
        | CompoundExpressionTerm(lhs, next) -> Some(lhs, next)
        | _ -> None)
    and (|CompoundExpressionTerm|_|) = memoize (function
        | Number(n, Next('.', CompoundExpression(v, next))) -> 
            Some(Repeat(n, v), next)
        | Number(threshold, Next('?', Number(critThreshold, Next('?', next)))) -> 
            Some(Check(Single(Simple(1, 20)), [critThreshold, Single(Simple(2, 1)); threshold, Single(Simple(1, 1)) ], 0), next)
        | Number(threshold, Next('?', next)) -> 
            Some(Check(Single(Simple(1, 20)), [threshold, Single(Simple(1, 1))], 0), next)
        | SimpleExpression(v, next) -> Some(Single(v), next)
        | _ -> None)
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
        
    member this.parseCompound txt =
        match (txt, 0) with
        | CompoundExpression(cmd, Empty) -> cmd
        | _ -> failwithf "failed to parse '%s'" txt

    member this.parseCommand txt =
        match (txt, 0) with
        | CommandExpression(cmd, Empty) -> cmd
        | _ -> failwithf "failed to parse '%s'" txt

let Parse txt = 
    (Impl()).parseCompound txt
let ParseCommand txt = (Impl()).parseCommand txt