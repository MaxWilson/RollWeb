#I __SOURCE_DIRECTORY__
#load @"load-references.fsx"
#load @"..\Util.fs"
      @"..\Packrat.fs"
      @"..\DataDefs.fs"
      @"..\Parser.fs"
      @"..\Dice.fs"
open mdw
open mdw.DataDefs
#r @"..\..\packages\xunit.runner.visualstudio.2.0.1\build\_common\xunit.abstractions.dll"
#r @"..\..\packages\xunit.assert.2.0.0\lib\portable-net45+win+wpa81+wp80+monotouch+monoandroid+Xamarin.iOS\xunit.assert.dll"
#r @"..\..\packages\xunit.extensibility.core.2.0.0\lib\portable-net45+win+wpa81+wp80+monotouch+monoandroid+Xamarin.iOS\xunit.core.dll"
open Xunit
#nowarn "0040"
#nowarn "0058"

type ParserContext() = 
    class end

let memoize ctx =
    fun name rule input ->
        rule input

type Expr = Leaf of char | Interior of Expr * Expr
#nowarn "0040" // Allow object recursion without warnings so we can write recursive memoized rules
[<Fact>]
let (|Next|Empty|) = function
| (input : string), pos when pos < input.Length -> Next(input.[pos], (input, pos+1))
| _ -> Empty
let show = sprintf "%A" 
let c = ParserContext()
let rec (|Xs|_|) = memoize c "Xs" (function
    | Xs(lhs, Next('+', Number(rhs, next))) -> Some(Interior(lhs, rhs), next)
    | Number(v, next) -> Some(v, next)
    | _ -> None)
and (|Number|_|) = memoize c "Number" (function
    | Next(c, next) when System.Char.IsDigit(c) -> Some(Leaf c, next)
    | _ -> None)
match("1+2+3",0) with
| Xs(v, Empty) -> 
    // Result should be left-associative
    Assert.Equal(show <| Interior(Interior(Leaf('1'),Leaf('2')),Leaf('3')), show v)
| _ -> failwith "Could not parse"
