
#I __SOURCE_DIRECTORY__
#load @"load-references.fsx"
#load @"..\Util.fs"
      @"..\DataDefs.fs"
      @"..\Parser.fs"
      @"..\Dice.fs"
open mdw
open mdw.DataDefs
open mdw.Parser.Impl
let spec = Parser.Parse("20d6-d4-d4") |> Dice.Instance.Average
makeRoll 1 2 ("d2+", 2)

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
let rec (|Number|_|) = function
    | Chars numeric i1 as i0 -> (System.Int32.Parse(sub i0 i1), i1) |> Some
    | _ -> None
and (|Expr|_|) = memoize (function
    | Expr(lhs, Next('+', Expr(rhs, next))) -> Some(sprintf "(%s, %s)" lhs rhs, next)
    | Number(v, next) -> Some(v.ToString(), next)
    )
match ("4+3+2+1", 0) with
| Expr(v, Empty) -> printfn "%A" v
| _ -> Util.nomatch()
