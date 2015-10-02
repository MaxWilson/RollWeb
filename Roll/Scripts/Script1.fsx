﻿#I __SOURCE_DIRECTORY__
#load @"load-references.fsx"
#load @"..\Util.fs"
      @"..\Packrat.fs"
      @"..\DataDefs.fs"
      @"..\Parser.fs"
      @"..\Dice.fs"
open mdw
open mdw.DataDefs

#nowarn "0040"

type Input = string * int
type Rule<'a> = string * (Input -> ('a * Input) option)
type Head<'a> = { headRule: Rule<'a> ref; involved: Set<string> ref; eval: Set<string> ref}
type LR<'a> = { seed: ('a * Input) option ref; rule: Rule<'a>; head: Head<'a> option ref; rest: LR<'a> option ref }
type Ans<'a> = LR of LR<'a> | Ans of 'a option
type Memo<'a> = { ans: Ans<'a> ref ; pos : Input ref }

type ctx<'a>() =
    let heads: Map<Input, Head<'a>> ref = ref Map.empty
    let memorized = ref Map.empty
    member this.InitializeHead(input: Input, head: Head<'a>) =
        heads := !heads |> Map.add input head
    member this.ClearHead(input: Input) =
        heads := !heads |> Map.remove input
    member this.Head(input: Input) =
        Map.tryFind input !heads
    member this.Memo(name : string, input : Input) : Memo<'a> option =
        Map.tryFind (name, input) !memorized
    member this.Memorize(name : string, input : Input, memo: Memo<'a>) =
        memorized := Map.add (name, input) memo !memorized
    member val LRStack : LR<'a> option ref = ref None with get, set
    member this.Debug = heads, memorized

let memoize (ctx: ctx<'a>) =
    let grow((name, rule): Rule<'a>, input, memo, head) = 
        ctx.InitializeHead(input, head)
        let mutable break1 = false
        while not break1 do
            head.eval := !head.involved
            match rule input with
            | None ->
                break1 <- true
            | Some(v, (s, i)) when i <= (snd !memo.pos) ->
                break1 <- true
            | Some(v, (s, i)) ->
                memo.ans := Ans(Some v)
                memo.pos := (s, i)
        ctx.ClearHead(input)
        match !memo.ans with
        | LR(_) -> Util.nomatch()
        | Ans(None) -> None
        | Ans(Some(v)) -> Some(v, !memo.pos)
    let recall((name, rule): Rule<'a>, input) : Memo<'a> option =
        let m = ctx.Memo(name, input)
        let h = ctx.Head(input)
        match m, h with
        // if not growing, just return whatever is memoized
        | _, None -> m
        // do not evaluate any rule that is not involved
        | None, Some(h) when not (name = (fst (!h.headRule)) || (Set.contains name !h.involved)) ->
            None
        // when growing left-recursively, evaluate involved rules, but only once per parser position
        | _, Some(h) when Set.contains name !h.eval ->
            h.eval := Set.remove name !h.eval
            let ans = ((!h.headRule) |> snd) input
            match m, ans with
            | Some(m), Some(v, rest) -> 
                m.ans := Ans(Some v)
                m.pos := rest
            | Some(m), None -> 
                m.ans := Ans None
            | _ -> Util.nomatch() // should always have a memoized entry available when growing left-recursively
            m
        | _ -> m
    // (re-)initialize lr based on LRStack variable; compute involved set and head rule
    let setupLeftRecursion((name, rule):Rule<'a>, lr: LR<'a>) =
        let h = 
            match (!lr.head) with
            | None ->
                let h = { headRule = ref (name, rule); involved = ref Set.empty; eval = ref Set.empty }
                lr.head := Some(h)
                h
            | Some(h) -> h
        // Use 's' to walk LRStack until a loop end is detected, collecting results in h.involved
        let mutable s = !ctx.LRStack
        let getHead (lro : LR<'a> option) = 
            lro 
            |> Option.bind (fun lro -> 
                !lro.head 
                |> function 
                    | Some(h) -> Some(h.headRule.Value |> fst) 
                    | None -> None)
        while s.IsSome && (getHead s <> (fst (!h.headRule) |> Some)) do
            s.Value.head := Some h
            h.involved := Set.add name !h.involved
            s <- !s.Value.rest
    // Calls grow if this rule is the head of left recursion, otherwise just
    // participate by returning the seed while the head grows instead.
    // Seed = largest parse with no left recursion.
    let growOrDefer((name,rule) : Rule<'a>, input : Input, memo : Memo<'a>) =
        match !memo.ans with
        | LR(lr) ->
            let h = !lr.head
            match h with
            | Some(h) when fst !h.headRule <> name ->
                !lr.seed
            | Some(h) ->
                memo.ans := Ans (Option.map fst !lr.seed)
                match !memo.ans with
                | Ans None -> None
                | _ -> grow((name,rule), input, memo, h)
            | _ -> Util.nomatch()
        | _ -> Util.nomatch()
                 
    fun name rule input -> 
        match recall((name, rule), input) with
        | None ->
            let lr = { seed = ref None; rule = (name, rule); head = ref None; rest=ref !ctx.LRStack }
            ctx.LRStack := Some lr
            let m = { ans = ref (LR lr); pos = ref input }
            ctx.Memorize(name, input, m)
            let ans = rule input
            ctx.LRStack := match (!ctx.LRStack) with 
                            | Some(stack) -> !stack.rest 
                            | _ -> Util.nomatch()
            match ans with
            | Some(v, pos) ->
                m.pos := pos
            | _ -> ()
            match !lr.head with
            | Some(h) ->
                lr.seed := ans
                growOrDefer((name, rule), input, m)
            | None ->
                m.ans := Ans (Option.map fst ans)
                ans
        | Some(m) ->
            match !m.ans with
            | LR(lr) ->
                setupLeftRecursion((name, rule), lr)
                !lr.seed
            | Ans(None) -> None
            | Ans(Some(v)) -> Some(v, !m.pos)

let (|Next|Empty|) = function
    | (input : string), pos when pos < input.Length -> Next(input.[pos], (input, pos+1))
    | _ -> Empty

let c = ctx()
let rec (|E|_|) = memoize c "E" (function
    | Xs(v, next) -> Some(v, next)
    | _ -> None)
and (|Xs|_|) = memoize c "Xs" (function
    | E(v, Next('+', Next('x', next))) -> Some(v+1, next)
    | Next('x', next) -> Some(1, next)
    | _ -> None)
// memoization provides better perf (linear time) AND allows left-recursive grammars to terminate
try
    match("x+x",0) with
    | Xs(v, Empty) -> 
            printfn "%A" v
    | _ -> printfn "Fail"
with e -> printfn "%s" e.Message
c.Debug
Parser.Parse "10-4-4"
Parser.Parse "20.d20:14?3d8+4"
c.Tracer.Current
c.IsLR("E", ("x+x", 0))
