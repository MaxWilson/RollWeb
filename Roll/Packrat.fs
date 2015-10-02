[<WebSharper.Core.Attributes.JavaScript>]
module mdw.Packrat
open mdw.Util

(* DEEP MAGIC BEGINS HERE
The following memo function allows construction of left-recursive grammars using a stateful
iterated algorithm. Don't mess with the code in this file unless you've read the computer
science papers involved and thoroughly understand them, and also have unit tests for the change
you want to make.
*)


type Input = string * int
type Rule<'a> = string * (Input -> ('a * Input) option)
type Head<'a> = { headRule: Rule<'a>; mutable involved: Set<string>; mutable eval: Set<string> }
type LR<'a> = { mutable seed: ('a * Input) option; rule: Rule<'a>; mutable head: Head<'a> option; mutable rest: LR<'a> option }
type Ans<'a> = LR of LR<'a> | Ans of 'a option
type Memo<'a> = { mutable ans: Ans<'a>; mutable pos : Input }

/// ParserContext holds all the "global" state for the parser. Memoized results, LR stack, etc.
/// Not thread-safe for multiple threads--you'll get wrong answers if more than once logic stream
/// is writing to these variables simultaneously.
type ParserContext<'a>() =
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
    member val LRStack : LR<'a> option = None with get, set
    member this.Debug = heads, memorized
    member this.PushLR(lr) =
        this.LRStack <- Some lr
    member this.PopLR() =
        if this.LRStack.IsSome then
            this.LRStack <- this.LRStack.Value.rest

let memoize (ctx: ParserContext<'a>) =
    let grow((name, rule): Rule<'a>, input, memo, head) = 
        ctx.InitializeHead(input, head)
        let mutable break1 = false
        while not break1 do
            head.eval <- head.involved
            match rule input with
            | None ->
                break1 <- true
            | Some(v, (s, i)) when i <= (snd memo.pos) ->
                break1 <- true
            | Some(v, (s, i)) ->
                memo.ans <- Ans(Some v)
                memo.pos <- (s, i)
        ctx.ClearHead(input)
        match memo.ans with
        | LR(_) -> Util.nomatch()
        | Ans(None) -> None
        | Ans(Some(v)) -> Some(v, memo.pos)
    let recall((name, rule): Rule<'a>, input) : Memo<'a> option =
        let m = ctx.Memo(name, input)
        let h = ctx.Head(input)
        match m, h with
        // if not growing, just return whatever is memoized
        | _, None -> m
        // do not evaluate any rule that is not involved
        | None, Some(h) when not (name = (fst (h.headRule)) || (Set.contains name h.involved)) ->
            None
        // when growing left-recursively, evaluate involved rules, but only once per parser position
        | _, Some(h) when Set.contains name h.eval ->
            h.eval <- Set.remove name h.eval
            let ans = ((h.headRule) |> snd) input
            match m, ans with
            | Some(m), Some(v, rest) -> 
                m.ans <- Ans(Some v)
                m.pos <- rest
            | Some(m), None -> 
                m.ans <- Ans None
            | _ -> Util.nomatch() // should always have a memoized entry available when growing left-recursively
            m
        | _ -> m
    // (re-)initialize lr based on LRStack variable; compute involved set and head rule
    let setupLeftRecursion((name, rule):Rule<'a>, lr: LR<'a>) =
        let LRHead = 
            match (lr.head) with
            | None ->
                let h = { headRule = (name, rule); involved = Set.empty; eval = Set.empty }
                lr.head <- Some(h)
                h
            | Some(h) -> h
        // Use 's' to walk LRStack until a loop end is detected, collecting results in h.involved
        let mutable s = ctx.LRStack
        let getHead (lro : LR<'a> option) = 
            lro 
            |> Option.bind (fun lro -> 
                                lro.head 
                                |> function 
                                    | Some(h) -> Some(h.headRule |> fst) 
                                    | None -> None)
        // Note: Does not have to be the same as name
        let headName = getHead (Some lr)
        while s.IsSome && (getHead s <> headName) do
            s.Value.head <- lr.head
            LRHead.involved <- Set.add (fst s.Value.rule) LRHead.involved
            s <- s.Value.rest

    // Calls grow if this rule is the head of left recursion, otherwise just
    // participate by returning the seed while the head grows instead.
    // Seed = largest parse with no left recursion.
    let growOrDefer((name,rule) : Rule<'a>, input : Input, memo : Memo<'a>) =
        match memo.ans with
        | LR(lr) ->
            let h = lr.head
            match h with
            | Some(h) when fst h.headRule <> name ->
                lr.seed
            | Some(h) ->
                memo.ans <- Ans (Option.map fst lr.seed)
                match memo.ans with
                | Ans None -> None
                | _ -> grow((name,rule), input, memo, h)
            | _ -> Util.nomatch()
        | _ -> Util.nomatch()
                 
    fun name rule input -> 
        match recall((name, rule), input) with
        | None ->
            let lr = { seed = None; rule = (name, rule); head = None; rest=ctx.LRStack }
            ctx.PushLR(lr)
            let m = { ans = (LR lr); pos = input }
            ctx.Memorize(name, input, m)
            let ans = rule input
            ctx.PopLR()
            match ans with
            | Some(v, pos) ->
                m.pos <- pos
            | _ -> ()
            match lr.head with
            | Some(h) ->
                lr.seed <- ans
                growOrDefer((name, rule), input, m)
            | None ->
                m.ans <- Ans (Option.map fst ans)
                ans
        | Some(m) ->
            match m.ans with
            | LR(lr) ->
                setupLeftRecursion((name, rule), lr)
                lr.seed
            | Ans(None) -> None
            | Ans(Some(v)) -> Some(v, m.pos)
