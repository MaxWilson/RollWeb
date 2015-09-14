﻿[<WebSharper.Core.Attributes.JavaScript>]
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
    member this.Resolve cmd = 
        match cmd : Compound with
        | Single(cmd) -> this.Resolve(cmd)
        | Sum(single, rest) -> this.Resolve(single) + this.Resolve(rest)
    member this.Average cmd =
        match cmd: Compound with
        | Single(Simple(n, d)) -> (float n) * (float (d + 1))/2.
        | _ -> Util.nomatch()
let Instance = Resolver()

