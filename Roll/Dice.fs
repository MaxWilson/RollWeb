[<WebSharper.Core.Attributes.JavaScript>]
module mdw.Dice 

open DataDefs
open System
open System.Diagnostics.Contracts
open WebSharper

type Resolver(?random) =
    let r = defaultArg random (new Random())    
    member this.Resolve cmd = 
        match cmd : Simple with
        | Simple(d, size) -> 
            [for _ in 1..d do yield 1 + r.Next(size)]
            |> Seq.sum
        | Adv(d, size) -> 
            [for _ in 1..d do yield 1 + max (r.Next(size)) (r.Next(size))]
            |> Seq.sum
        | Disadv(d, size) -> 
            [for _ in 1..d do yield 1 + min (r.Next(size)) (r.Next(size))]
            |> Seq.sum
    member this.Resolve cmd = 
        match cmd : Compound with
        | Single(cmd) -> this.Resolve(cmd)
        | Sum(lhs, rhs) -> this.Resolve(lhs) + this.Resolve(rhs)
        | Repeat(n, rhs) -> Seq.sum [for _ in 1..n do yield this.Resolve(rhs)]
    member this.Average cmd =
        match cmd: Compound with
        | Single(Simple(n, d)) -> (float n) * (float (d + 1))/2.
        | Repeat(n, inner) -> (float n) * (this.Average(inner))
        | _ -> Util.nomatch()
let Instance = Resolver()

