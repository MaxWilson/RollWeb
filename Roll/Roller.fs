[<WebSharper.Core.Attributes.JavaScript>]
module Roller

open DataDefs
open System
open System.Diagnostics.Contracts
open WebSharper

type Resolver(?random) =
    let r = defaultArg random (new Random())
    member this.Resolve cmd = 
        match cmd : Simple with
        | (d, size) -> 
            [for _ in 1..d do yield 1 + r.Next(size)]
            |> Seq.sum
    member this.Resolve cmd = 
        match cmd : Compound with
        | Single(cmd) -> this.Resolve(cmd)
        | Plus(single, rest) -> this.Resolve(single) + this.Resolve(rest)

let Instance = Resolver()

