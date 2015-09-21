
#I __SOURCE_DIRECTORY__
#load @"load-references.fsx"
#load @"..\Util.fs"
      @"..\DataDefs.fs"
      @"..\Parser.fs"
      @"..\Dice.fs"
open mdw
open mdw.DataDefs
open mdw.Parser.Impl
let spec = Parser.Parse("d2+") |> Dice.Instance.Average
makeRoll 1 2 ("d2+", 2)

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
let sumTerms = Seq.groupBy fst
                >> (Seq.map (fun (n, terms) -> (n, terms |> Seq.sumBy snd)))
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
                    // make it prettier by eliminating 2, which is a common factor
                    |> Seq.map (fun (n, x) -> (n, x / 2)) // will always be even because adv is symmetric

        sum |> List.ofSeq
    | _ -> Util.nomatch()

let lhs = enumerateSimple (Adv(1, 20))
let rhs = enumerateSimple (Simple(8, 1))
let cross = Seq.zip lhs rhs
    [for (n0, c0) in lhs do
                    for (n1, c1) in rhs do
                        yield (n0 + n1), (c0 + c1)
                ]
                |> Seq.groupBy fst
                |> Seq.map (fun (n, terms) -> (n, terms |> Seq.sumBy snd))
enumerate (Sum(Single(Simple(8, 1)), Single(Simple(2,6))))

enumerate (Sum(Single(Simple(8, 1)), Single(Simple(2,6))))
