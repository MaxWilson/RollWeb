[<WebSharper.Core.Attributes.JavaScript>]
module mdw.Packrat
open mdw.Util

(* DEEP MAGIC BEGINS HERE
The following memo function allows construction of left-recursive grammars using a stateful
iterated algorithm. Don't mess with the code in this file unless you've read the computer
science papers involved and thoroughly understand them, and also have unit tests for the change
you want to make.

Okay, it's somewhat less deep magic now, since I rewrote it in continuation-passing style.
*)

type Input = string * int
type Result<'a> = 
    // Memoized parse result: either an AST and the rest of the input, or failure
    | Memo of ('a * Input) option 
    // A transformer which takes (name, rule, input) and produces a parse result 
    | Func of ((string * (Input -> ('a * Input) option) * Input) -> ('a * Input) option)
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
    member this.Memory = mem

let rec firstTime (ctx: ParserContext<'a>) (name, rule, input) =
    // grow a seed, collecting all left recursions as involved
    let involved = ref Set.empty
    let leftRecursion = ref false
    let prevInvolved = ctx.RecordInvolved
    ctx.RecordInvolved <- 
        Some(fun callStack ->
            match callStack with
            | (h,_)::t when h = name -> 
                leftRecursion := true
            | _ -> ()
            if !leftRecursion then
                // we have a left recursion!
                let cycles = Seq.takeWhile (fst >> ((<>)name)) callStack.Tail
                involved := Set.union (Set.ofSeq cycles) !involved
            elif prevInvolved.IsSome then
                prevInvolved.Value callStack)
    let seed = rule input
    // tidy up
    ctx.RecordInvolved <- prevInvolved
    if !leftRecursion then
        grow ctx !involved (name, rule, input, seed)
    else
        ctx.Memorize(name, input, Memo(seed))
        seed
and grow (ctx: ParserContext<'a>) involved (name, rule, input, seed) =
    for (rule, startpos) in involved do
        ctx.Memorize(rule, startpos, Func (evalOnce (ref true)))
    let rec loop prev =
        ctx.Memorize(name, input, Memo(prev))
        match rule input, prev with
        | Some(v, next) as ans, None ->
            // We grew! Keep growing
            loop ans
        | Some(v, ((_, i) as next)) as ans, Some(_, (_, prevNext)) when i > prevNext ->
            // We grew! Keep growing
            loop ans
        | _ -> 
            // I'm not sure about this cleanup phase
            for (rule, startpos) in involved do
                ctx.Memorize(rule, startpos, Func (firstTime ctx))
            // Done. Clean up and return
            prev
    loop seed
and evalOnce eval (name, rule, input)=
    if !eval then
        eval := false
        let retval = rule input
        eval := true
        retval    
    else
        None
and lrAnswer (ctx: ParserContext<'a>)  (name, rule, input)=
    // Since we're now done with seed-growing, memoize the current answer
    // I'm not sure if this is actually the right thing to do... may not account well
    // for multiple heads. But I don't have a solid repro in my mind for where this
    // would cause problems.
    let ans = rule input
    ctx.Memorize(name, input, Memo(ans))
    ans      
and fail(name : string, rule: (Input -> ('a * Input) option), input : Input) =
    None

let getProcessor (ctx: ParserContext<'a>) (name, rule, input) =
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
        let parentFrames = ctx.CallStack.Tail
        if List.exists ((=) (name, input)) parentFrames then
            ctx.RecordInvolved.Value(ctx.CallStack)
            Func fail
        else
            Func (firstTime ctx)

let memoize (ctx : ParserContext<'a>) =
    // Depending on what we're doing right now, we could handle input in one of several ways
            
    let rec eval name rule input =
        ctx.Begin(name, input)
        let p = getProcessor ctx (name, rule, input)
        match p with
        | Memo(ans) -> 
            ctx.Return(name, input, ans)
        | Func(f) ->
            ctx.Return(name, input, f(name, rule, input))
    eval
