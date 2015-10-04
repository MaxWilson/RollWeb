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

type Input = string * int
type Result<'a> = | Memo of ('a * Input) option | Func of (unit -> ('a * Input) option)
type ParserContext<'a>() = 
    let mutable mem = Map.empty
    let mutable callStack = []
    member this.Memo(name : string, input : Input) : Result<'a> option =
        Map.tryFind (name, input) mem
    member this.Memorize(name, input, value) =
        mem <- Map.add (name, input) value mem
    member this.Reset() =
        mem <- Map.empty
    member this.Begin(name, input) =
        callStack <- (name, input) :: callStack
    member this.Return(name, input, result) =
        match callStack with
        | (n,i)::tail when n = name && i = input ->
            callStack <- tail
        | _ ->
            Util.nomatch() // should never happen if memoize is working correctly
        result // return result for convenience
    member this.CallStack = callStack
    member val RecordInvolved = None with get, set

let memoize (ctx : ParserContext<'a>)=
    let rec firstTime name rule input () =
        // grow a seed, collecting all left recursions as involved
        let involved = ref Set.empty
        ctx.RecordInvolved <- 
            Some(fun callStack ->
                let getCycles = function
                | (h,_)::t -> Seq.takeWhile (fst >> ((<>)h)) t
                | _ -> Util.nomatch()
                involved := Set.union (Set.ofSeq (getCycles callStack)) !involved)
        let seed = rule input
        // tidy up to make debugging easier
        ctx.RecordInvolved <- None
        if Set.isEmpty !involved then
            ctx.Memorize(name, input, Memo(seed))
            seed
        else
            grow(name, rule, input)
    and grow(name, rule, input) =
        None

    let fail() =
        None
    let getRule(name, rule, input) =
        match ctx.Memo(name, input) with
        // Already computed and memoized an answer
        | Some(Memo(v)) ->
            Memo(v)
        // Already have a continuation specified
        | Some(Func(f)) ->
            Func(f)
        | None ->
            // First time at this position. Either it's the first time for this production
            // period, or the production is already on the call stack higher up, which
            // means we're growing a seed.
            if List.exists (fst >> ((=) name)) ctx.CallStack then
                ctx.RecordInvolved.Value(ctx.CallStack)
                Func fail
            else
                Func (firstTime name rule input)               
            
    let rec eval name rule input =
        ctx.Begin(name, input)
        match getRule(name, rule, input) with
        | Memo(ans) -> 
            ctx.Return(name, input, ans)
        | Func(f) ->
            ctx.Return(name, input, f())
    eval
        
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
