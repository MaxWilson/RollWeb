module Debug
open mdw
open Xunit
open mdw.DataDefs

(* 
Because VisualStudio cannot detect which Xunit test the cursor is on, 
it's convenient to have a separate file to isolate single tests in order to debug them.
This test should be kept empty in public checkins.
*)

#nowarn "0040"
#nowarn "0058"

open mdw.Packrat

[<Fact>]
let ``Grammar with multiple heads``() =
    let (|Next|Empty|) = function
    | (input : string), pos when pos < input.Length -> Next(input.[pos], (input, pos+1))
    | _ -> Empty

    let c = ParserContext()
    let rec (|Ks|_|) = memoize c "CompoundExpression" (function
        | Next('K', E(v, next)) -> Some("K"+v, next)
        | Next('x', next) -> Some("x", next)
        | _ -> None)
    and (|E|_|) = memoize c "E" (function
        | Next('K', Next('K', Next('K', next))) -> Some("KKK", next)
        | Ks(v,next) -> Some(v, next)
        | _ -> None)
    match("KKKK",0) with
    | Ks(v, Empty) -> Assert.Equal("KKKK", v)
    | _ -> failwith "Could not parse"
    match("KKK",0) with
    | E(v, Empty) -> Assert.Equal("KKK", v)
    | _ -> failwith "Could not parse"

[<Fact>]
let Repro1() =
    Parser.Parse("20.d20a-d20d:0?")