module mdw.Parser

#nowarn "40" // recursive object references from memoization

open DataDefs
open Packrat

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
    let memoize name = mdw.Packrat.pack

    let (|Next|Empty|) = function
        | ({ input = input } : ParseContext as ctx), pos when pos < input.Length -> Next(input.[pos], (ctx, pos+1))
        | v -> Empty

    let (|Char|_|) alphabet = function
        | Empty -> None
        | s, i when Set.contains s.input.[i] alphabet -> Some(s, i+1)
        | _ -> None

    let rec (|MaybeChars|) alphabet = function
        | Char alphabet (MaybeChars alphabet it)
        | it -> it

    let rec (|Chars|_|) alphabet = function
        | Char alphabet (MaybeChars alphabet (ctx, endpos)) as input ->
          let _, startpos = input
          Some (ctx.input.Substring(startpos, endpos - startpos), (ctx, endpos))
        | it -> None

    let (|NextChar|_|) alphabet = function
        | Empty -> None
        | s, i when Set.contains s.input.[i] alphabet -> Some(s.input.[i], (s, i+1))
        | _ -> None

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
        | Chars numeric (chars, rest) -> (System.Int32.Parse(chars), rest) |> Some
        | _ -> None
    and (|CompoundExpression|_|) = memoize "CompoundExpression" (function
        | CompoundExpression(lhs, Next('+', CompoundExpressionTerm(rhs, next))) -> Some(Sum(lhs, rhs), next)
        | CompoundExpression(lhs, Next('-', CompoundExpressionTerm(rhs, next))) -> Some(Sum(lhs, MultByConstant(-1, rhs)), next)
        | CheckTerm(v, next) -> Some(v, next)
        | CompoundExpressionTerm(lhs, next) -> Some(lhs, next)
        | _ -> None)
    and (|CompoundExpressionTerm|_|) = memoize "CompoundExpressionTerm"  (function
        | CompoundExpressionTerm(v, Next('/', Number(n, next))) -> Some(DivByConstant(n, v), next)
        | Next('(', CompoundExpression(lhs, Next(')', next))) -> Some(lhs, next)
        | Number(n, Next('.', CompoundExpression(v, next))) ->
            Some(Repeat(n, v), next)
        | SimpleExpression(v, next) -> Some(Single(v), next)
        | _ -> None)
    and (|CheckTerm|_|) =
        let (|Predicate|_|) = function
            | CompoundExpression(roll, Next('?', Number(target, Next(':', next)))) ->
                Some(roll, target, next)
            | CompoundExpression(roll, Next('?', Number(target, next))) ->
                Some(roll, target, next)
            | Number(target, NextChar advantageDisadvantage (c, Next('?', next))) ->
                let ctor = (if c = 'a' || c = 'A' then Adv else Disadv) >> Single
                Some(ctor(1, 20), target, next)
            | Number(target, Next('?', next)) ->
                Some(Single(Simple(1, 20)), target, next)
            | _ -> None
        let (|ResultTerm|_|) = function
            | CompoundExpression(result, next) ->
                Some(result, next)
            | next -> Some(Single(Simple(1, 1)), next)
        memoize "CheckTerm" (function
            | Predicate(roll, target, ResultTerm(consequent, next)) ->
                let rec double = function
                    | Single(Simple(n, 1)) as constant -> constant
                    | Single(Simple(n, d)) ->
                        Single(Simple(n*2, d))
                    | Single(Adv(n, d)) ->
                        Single(Adv(n*2, d))
                    | Single(Disadv(n, d)) ->
                        Single(Disadv(n*2, d))
                    | Sum(lhs, rhs) -> Sum(double lhs, double rhs)
                    | MultByConstant(k, rhs) -> MultByConstant(k, double rhs)
                    | DivByConstant(k, rhs) -> DivByConstant(k, double rhs)
                    | Check(_) -> Util.nomatch()
                    | Repeat(_) -> Util.nomatch()
                let rec maximize = function
                    | Single(Simple(n, d))
                    | Single(Adv(n, d))
                    | Single(Disadv(n, d)) ->
                        n*d
                    | Sum(lhs, rhs) -> maximize lhs + maximize rhs
                    | MultByConstant(k, rhs) ->
                        if k > 0 then k * maximize rhs
                        else k * minimize rhs
                    | DivByConstant(k, rhs) -> maximize rhs / k
                    | Repeat(k, rhs) -> k * maximize rhs
                    | Check(_) -> Util.nomatch()
                and minimize = function
                    | Single(Simple(n, d))
                    | Single(Adv(n, d))
                    | Single(Disadv(n, d)) ->
                        n
                    | _ -> Util.nomatch()
                Some(Check(roll, [maximize roll, double consequent; target, consequent], 0), next)
            | _ -> None
        )
    and (|SimpleExpression|_|) input =
        let makeRoll n d input =
            match input with
            | Char advantageDisadvantage (Char arithmeticOperators _) as rest ->
                let (s, i) = input
                let roll = match s.input.[i] with
                            | 'A' | 'a' -> Adv(n, d)
                            | 'D' | 'd' -> Disadv(n, d)
                            | _ -> Util.nomatch()
                Some (roll, (s, i+1))
            | Char advantageDisadvantage _ ->
                let (s, i) = input
                let roll = match s.input.[i] with
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
        match ParseContext.Init txt with
        | CompoundExpression(cmd, Empty) -> cmd
        | _ -> failwithf "failed to parse '%s'" txt

    member this.parseCommand txt =
        match ParseContext.Init txt with
        | CommandExpression(cmd, Empty) -> cmd
        | _ -> failwithf "failed to parse '%s'" txt

let Parse txt =
    (Impl()).parseCompound txt
let ParseCommand txt = (Impl()).parseCommand txt