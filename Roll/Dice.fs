[<WebSharper.Core.Attributes.JavaScript>]
module mdw.Dice 

open DataDefs
open System
open System.Diagnostics.Contracts
open WebSharper

type Resolver(?random) =
    let r = defaultArg random (new Random())    
    // enumerate the die pool, represented as a sequence of value/count pairs
    let enumerateSimple = function
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
        | _ -> Util.nomatch()
    let rec enumerate = function
        | Single(simple) -> enumerateSimple simple
        | Sum(lhs, rhs) -> 
            let sum = [for (n0, c0) in enumerate lhs do
                            for (n1, c1) in enumerate rhs do
                                yield (n0 + n1), (c0 + c1)
                        ]
                        |> Seq.groupBy fst
                        |> Seq.map (fun (n, terms) -> (n, terms |> Seq.sumBy snd))
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
            | Adv(n, d) -> 
                let sum = 
                    [for x in 1..d do
                        for y in 1..d do
                            yield max x y]
                    |> Seq.sum |> float 
                sum * (float n) / (float (d*d))
            | Disadv(n, d) -> 
                let sum = 
                    [for x in 1..d do
                        for y in 1..d do
                            yield min x y]
                    |> Seq.sum |> float 
                sum * (float n) / (float (d*d))
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

