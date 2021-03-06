﻿[<WebSharper.Core.Attributes.JavaScript>]
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
                                yield (n0 + n1), (c0 * c1)
                      ]
                      |> sumTerms
            sum |> List.ofSeq
        | MultByConstant(k, roll) ->
            List.map (fun (n, count) -> (n*k, count)) (enumerate roll)
        | _ -> Util.nomatch()
    let sumBy spec sumCalculator = 
        let seq = enumerate spec
        let total = Seq.sumBy snd seq |> float
        seq |> Seq.sumBy (fun (n, count) -> sumCalculator n (float count / total))  
    member this.Resolve cmd = 
        let result = 
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
        result
    member this.Resolve cmd = 
        match cmd : Compound with
        | Single(roll) -> 
            let v = this.Resolve(roll)
            Audit(cmd, v, List.empty)
        | Sum(lhs, rhs) -> 
            let Audit(_, lhs, _) as lexplain, (Audit(_, rhs, _) as rexplain) = this.Resolve(lhs), this.Resolve(rhs)
            Audit(cmd, lhs + rhs, [lexplain; rexplain])
        | MultByConstant(k, rhs) -> 
            let Audit(_, result, _) as explain = this.Resolve(rhs)
            Audit(cmd, k * result, [explain])
        | DivByConstant(k, rhs) -> 
            let Audit(_, result, _) as explain = this.Resolve(rhs)
            Audit(cmd, result/k, [explain])
        | Repeat(n, rhs) -> 
            let results = [for _ in 1..n do yield this.Resolve(rhs)]
            let result = Seq.sum (results |> Seq.map (function Audit(_, r, _) -> r))
            Audit(cmd, result, results)
        | Check(roll, resultOptions, fallback) ->
            let Audit(_, result, _) as explain = this.Resolve(roll)
            resultOptions 
            |> Seq.tryPick (function 
                            | (threshold, roll) when result >= threshold ->
                                this.Resolve(roll) |> Some
                            | _ -> None)
            |> function 
                | Some(Audit(_, v, _) as explainResult) -> 
                    Audit(cmd, v, [explain; explainResult])
                | None -> 
                    if fallback = 0 then
                        Audit(cmd, fallback, [explain])
                    else
                        Audit(cmd, fallback, [explain; Audit(mdw.DataDefs.Single(Simple(fallback, 1)), fallback, List.empty)])

    member this.Resolve cmd =
        match cmd : Command with
        | Roll(spec) -> 
            let Audit(_, result, _) as explain = this.Resolve(spec)
            let rec render = function
            | Audit(_, v, []) -> v.ToString()
            | Audit(Repeat(_), v, explanations) -> sprintf "(%s)" (System.String.Join(",", Seq.map render explanations))
            | Audit(Sum(_), v, explanations) -> 
                v.ToString()
            | Audit(Check(_), v, explanations) -> 
                match explanations with
                | [roll; result] ->
                    sprintf "%s->%s" (render roll) (render result)
                | [roll] ->
                    sprintf "%s->%d" (render roll) v
                | _ -> Util.nomatch()
            | Audit(MultByConstant(-1, _), v, explanations) ->
                "-" + (render explanations.Head)
            | Audit(DivByConstant(_), v, explanations) ->
                sprintf "%d(%s)" v (render explanations.Head)
            | _ -> Util.nomatch()
            result.ToString(), render explain
        | Average(spec) -> 
            let result : float = this.Average(spec)
            // My ad-hoc attempt to make decimals come out "naturally", 5 or 4.35
            let result = match sprintf "%.2f" result with
                         | v when v.EndsWith(".00") -> sprintf "%.0f" result
                         | v -> v
            result, "Computed"
            
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
        | DivByConstant(k, rhs) -> 
            sumBy rhs (fun n weight -> (n / k |> float) * weight) 
        | Sum(lhs, rhs) -> this.Average(lhs) + this.Average(rhs)
        | Check(roll, resultOptions, fallback) ->
            let resolve result = 
                resultOptions 
                |> Seq.tryPick (function 
                                | (threshold, roll) when result >= threshold ->
                                    this.Average(roll) |> Some
                                | _ -> None)
                |> function 
                    | Some(v) -> v 
                    | None -> fallback |> float
            sumBy roll (fun n weight -> (resolve n |> float) * weight)
let Instance = Resolver()

