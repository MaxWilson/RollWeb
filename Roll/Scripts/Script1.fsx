#I __SOURCE_DIRECTORY__
#load @"load-references.fsx"
#load @"..\Util.fs"
      @"..\Packrat.fs"
      @"..\DataDefs.fs"
      @"..\Parser.fs"
      @"..\Dice.fs"
open mdw
open mdw.DataDefs
#nowarn "0040"
#nowarn "0058"

let numeric = Set<char>['0'..'9']
let ctx = mdw.Packrat.ParserContext()
let memoize = mdw.Packrat.memoize ctx

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

let rec (|Number|_|) = function
    | Chars numeric i1 as i0 -> (System.Int32.Parse(sub i0 i1), i1) |> Some
    | _ -> None
and (|CompoundExpression|_|) = memoize "CompoundExpression" (function
    | CompoundExpressionTerm(lhs, next) -> Some(lhs, next)
    | _ -> None)
and (|CompoundExpressionTerm|_|) = memoize "CompoundExpressionTerm"  (function
    | CheckTerm(v, next) -> Some(v, next)
    | SimpleExpression(v, next) -> Some(v, next)
    | _ -> None)
and (|CheckTerm|_|) = memoize "CheckTerm" (function
        | CompoundExpression(roll, Next(':', Number(target, Next('?', CompoundExpression(_, next))))) -> 
            let consequent = Some(Single(Simple(1, 1)), next)
            let retval = Some(Check(roll, [], 0), next)
            printfn "CheckTerm=%A" retval
            retval
        | _ -> None
    )
and (|SimpleExpression|_|) = memoize "SimpleExpression" (function
    | Next('d', Number(dieSize, next)) -> Some(Single(Simple(1, dieSize)), next)
    | _ -> None)

ctx.Reset()
match (|CompoundExpression|_|) ("d20:14?d8", 0) with
| Some(v, _) -> ()
| None -> failwith "Failed too early to be useful"

(Map.tryFind ("CheckTerm", ("d20:14?d8", 0)) (snd ctx.Debug)) |>
    function 
    | None -> "Never analyzed it"
    | Some({ ans = Result }) -> 
        // We expect this to be a fragment of text, an Ans
        match Result with
        | mdw.Packrat.Ans(Some(v)) -> "Correct"
        | err -> sprintf "Error! %A" err
