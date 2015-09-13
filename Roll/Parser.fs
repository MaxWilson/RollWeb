module Parser

open DataDefs

[<AutoOpen>]
module Impl =
    open DataDefs

    (* Throughout this file we use string * int as a data type for parse inputs,
    representing a string and a position within it. I can't figure out how to get
    the type inference to work with type aliases though so I'm just using a raw
    string * int tuple. Wherever you see "string * int" in a type, think "parser
    input."
    *)

    let alpha = set['A'..'Z'] + set['a'..'z']
    let numeric = set['0'..'9']
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

    let rec (|CompoundExpression|_|) = function
        | SimpleExpression(v, next) -> Some(Single(v), next)
        | _ -> None
    and (|SimpleExpression|_|) = function
        | Next('d', Number(dieSize, next)) -> Some (Simple(1,dieSize), next)
        | Number (n, Next('d', Number(dieSize, next))) -> Some (Simple(n, dieSize), next)
        | Number (n, Next('d', next)) -> Some (Simple(n, 6), next)
        | Number (n, next) -> Some (Simple(n, 1), next)
        | _ -> None
    and (|Number|_|) = function
        | Chars numeric i1 as i0 -> (System.Int32.Parse(sub i0 i1), i1) |> Some
        | _ -> None

    let parse txt =
        match (txt, 0) with
        | CompoundExpression(cmd, Empty) -> cmd
        | _ -> failwithf "failed to parse '%s'" txt

let Parse txt = parse txt