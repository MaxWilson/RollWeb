[<WebSharper.Core.Attributes.JavaScript>]
module mdw.Dice 

open DataDefs
open System
open System.Diagnostics.Contracts
open WebSharper

type Resolver(?random) =
    let r = defaultArg random (new Random())    
    // sum up a probability distribution
    let sumTerms = Seq.groupBy fst
                   >> (Seq.map (fun (n, terms) -> (n, terms |> Seq.sumBy snd)))
    // enumerate the die pool, represented as a sequence of value/count pairs
    let rec enumerateSimple = function
        | Simple(n, 1) -> [(n,1)] // primitive case
        | Simple(n, d) ->
            let add range dieSize =   
                let min = 1 + (Seq.map fst >> Seq.min) range  
                let max = dieSize + (Seq.map fst >> Seq.max) range      
                [for x in min..max ->
                    (x, range |> Seq.sumBy (fun (n, count) ->
                        let roll = x - n
                        if 1 <= roll && roll <= dieSize then
                            count
                        else
                            0
                    ))
                ] 
            Seq.fold (fun range _ -> add range d) [(0,1)] [1..n]
        | Adv(n, d) ->
            let sum = [ for (n0, c0) in enumerateSimple (Simple(n,d)) do
                            for (n1, c1) in enumerateSimple (Simple(n,d)) do
                                yield
                                    if n1 > n0 then
                                        (n1, c0 + c1)
                                    else
                                        (n0, c0 + c1)
                      ] |> sumTerms
                        // make it (unnecessarily) prettier by eliminating 2, which is a common factor
                        |> Seq.map (fun (n, x) -> (n, x / 2)) // will always be even because adv is symmetric
            sum |> List.ofSeq
        | Disadv(n, d) ->
            let sum = [ for (n0, c0) in enumerateSimple (Simple(n,d)) do
                            for (n1, c1) in enumerateSimple (Simple(n,d)) do
                                yield
                                    if n1 < n0 then
                                        (n1, c0 + c1)
                                    else
                                        (n0, c0 + c1)
                      ] |> sumTerms
                        // make it (unnecessarily) prettier by eliminating 2, which is a common factor
                        |> Seq.map (fun (n, x) -> (n, x / 2)) // will always be even because adv is symmetric
            sum |> List.ofSeq
    let rec enumerate = function
        | Single(simple) -> enumerateSimple simple
        | Sum(lhs, rhs) -> 
            let sum = [ for (n0, c0) in enumerate lhs do
                            for (n1, c1) in enumerate rhs do
                                yield (n0 + n1), (c0 + c1)
                      ]
                      |> sumTerms
            sum |> List.ofSeq
        | _ -> Util.nomatch()
    let sumBy spec sumCalculator = 
        let seq = enumerate spec
        let total = Seq.sumBy snd seq |> float
        seq |> Seq.sumBy (fun (n, count) -> sumCalculator n (float count / total))  
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
        | MultByConstant(k, rhs) -> k * this.Resolve(rhs)
        | Repeat(n, rhs) -> Seq.sum [for _ in 1..n do yield this.Resolve(rhs)]
        | Check(roll, resultOptions, fallback) ->
            let result = this.Resolve(roll)
            resultOptions 
            |> Seq.tryPick (function 
                            | (threshold, roll) when result >= threshold ->
                                this.Resolve(roll) |> Some
                            | _ -> None)
            |> function 
                | Some(v) -> v 
                | None -> fallback
    member this.Resolve cmd =
        match cmd : Command with
        | Roll(spec) -> this.Resolve(spec).ToString()
        | Average(spec) -> this.Average(spec).ToString()
            
    member this.Average cmd =
        match cmd: Compound with
        | Single(inner) -> 
            match inner with
            | Simple(n, d) -> (float n) * (float (d + 1))/2.
            | _ -> 
                let dist = enumerateSimple inner
                let total = dist |> Seq.sumBy snd
                let sum = dist |> Seq.sumBy (fun (n, count) -> n * count)
                (float sum) / (float total)
        | Repeat(n, inner) -> (float n) * (this.Average(inner))
        | MultByConstant(k, rhs) -> (float k) * (this.Average(rhs))
        | Sum(lhs, rhs) -> this.Average(lhs) + this.Average(rhs)
        | Check(roll, resultOptions, fallback) ->
            let resolve result = 
                resultOptions 
                |> Seq.tryPick (function 
                                | (threshold, roll) when result >= threshold ->
                                    this.Resolve(roll) |> Some
                                | _ -> None)
                |> function 
                    | Some(v) -> v 
                    | None -> fallback
            sumBy roll (fun n weight -> (resolve n |> float) * weight)
let Instance = Resolver()

