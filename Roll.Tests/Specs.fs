module mdw.Roll.Tests
open Xunit
open FsCheck
open mdw
open DataDefs
open Dice
open mdw.Packrat

[<Fact>]
let ``Test rolls``() =
    Assert.Equal(8, Dice.Instance.Resolve(Simple(8,1)))

[<Fact>]
let ``Test rolls by property``() =
    let between lower upper x =
        lower <= x && x <= upper
    let isWithinBounds d size =
        (d > 0 && size > 0) ==>
            lazy (between (d) (d*size) (Dice.Instance.Resolve(Simple(d, size))))
    Check.QuickThrowOnFailure(isWithinBounds)
    
[<Fact>]
let Parsing() =
    Assert.Equal(Single(Simple(3,6)), Parser.Parse "3d6")
    Assert.Equal(Single(Simple(1,8)), Parser.Parse "d8")
    Assert.Equal(Single(Simple(4,6)), Parser.Parse "4d")
    Assert.Equal(Single(Simple(11,1)), Parser.Parse "11")

[<Fact>]
let Rolling() =
    let withinBounds low high spec seed =
        let roller = Resolver(new System.Random(seed))
        let result = roller.Resolve(spec : Compound) |> function Audit(_, v, _) -> v
        low <= result && result <= high
    Check.QuickThrowOnFailure (withinBounds 3 18 (Single(Simple(3,6))))

[<Fact>]
let Average() =
    let eq(expected, actual) = Assert.InRange(actual, expected - 0.0001, expected + 0.0001)
    Assert.Equal(3.5, Dice.Instance.Average(Single(Simple(1,6))))
    Assert.Equal(10.5, Dice.Instance.Average(Single(Simple(3,6))))
    eq (4./216., Dice.Instance.Average(Check(Sum(Single(Simple(1,6)), Single(Simple(2,6))), [17, Single(Simple(1,1))], 0)))

[<Theory>]
[<InlineData("3d6", 10.5)>] // Basic roll
[<InlineData("10.3d6", 105.)>] // Multiple rolls
[<InlineData("10.3d6+4", 145.)>] // Multiple rolls with complex result
[<InlineData("10.3d6-2", 85.)>] // Multiple rolls with subtraction
[<InlineData("1d2A", 1.75)>] // Roll with advantage
[<InlineData("1d2D", 1.25)>] // Roll with disadvantage
[<InlineData("20.18?", 3.)>] // Multiple checks
[<InlineData("(d20+11)-(d20+8)", 3.)>] // Subtraction
[<InlineData("10.18A?", 2.775)>] // Simple check with advantage
[<InlineData("10.18D?", 0.225)>] // Simple check with disadvantage
[<InlineData("20.18A?100", 555.)>] // Check with advantage and result
[<InlineData("20.20?1d10+d6-2", 16.)>] // Check with result
[<InlineData("20.d20:14?", 7)>] // Check with explicit roll syntax
[<InlineData("20.(d20a-d20d):0?", 16.984)>] // Check with explicit roll syntax and complex roll
let ``Complete-ish list of example roll specs``(input: string, expectedAverage: float) =
    let spec = Parser.Parse(input)
    let round (x : float) = System.Math.Round(x, 3) // round to three places
    Assert.Equal(round expectedAverage, round (Dice.Instance.Average(spec)))
    Dice.Instance.Resolve(spec) |> ignore // must not throw

[<Fact>]
let ``Sums should be left-associative``() =
    Assert.Equal(65., Parser.Parse "20d6-d4-d4" |> Dice.Instance.Average)

[<Theory>]
[<InlineData("3.2d4", "(X,X,X)")>]
[<InlineData("3.4d6", "(X,X,X)")>]
[<InlineData("(3.4d6):1?3d10", "(X,X,X)->X")>]
[<InlineData("(d20a+d4+7):4?d10+d6+5", "X->X")>]
let ``Examples of explanations that should be checkable``(spec, expected) =
    let spec = Parser.ParseCommand(spec)
    let result, explain = Dice.Instance.Resolve(spec)
    let explain = System.Text.RegularExpressions.Regex.Replace(explain, "\d+", "X")
    Assert.Equal(expected, explain)

[<Theory>]
[<InlineData("3d6", null)>]
[<InlineData("avg.3d6", "10.50")>]
[<InlineData("avg.3d6-4", "6.50")>]
[<InlineData("avg.3d6-(d4-d4)", "10.50")>]
[<InlineData("avg.20d6-(d4-d4)", "70")>]
[<InlineData("avg.20d6-(d4-d4)", "70")>]
[<InlineData("avg.20d6-(d4-d4)", "70")>]
let ``Complete-ish list of example command specs``(input: string, expectedOutput: string) =
    let spec = Parser.ParseCommand(input)
    let output = Dice.Instance.Resolve(spec) |> fst
    if(expectedOutput <> null) then
        Assert.Equal<string>(expectedOutput, output)

[<Theory(Skip="Incomplete")>]
[<InlineData("20.18?20?", 4.)>]
[<InlineData("20.d4A+d10+d20D:18?d10+5+d6", 0.)>]
let ``Example roll specs that aren't working yet``(input: string, expectedAverage: float) =
    Assert.Equal(expectedAverage, Dice.Instance.Average(Parser.Parse(input)))

type Expr = Leaf of char | Interior of Expr * Expr
#nowarn "0040" // Allow object recursion without warnings so we can write recursive memoized rules
[<Fact>]
let ``Should be able to parse direct left-recursive left-associative grammers``() =
    let (|Next|Empty|) = function
    | (input : string), pos when pos < input.Length -> Next(input.[pos], (input, pos+1))
    | _ -> Empty
    let show = sprintf "%A" 
    let c = ParserContext()
    let rec (|Xs|_|) = memoize c "Xs" (function
        | Xs(lhs, Next('+', Number(rhs, next))) -> Some(Interior(lhs, rhs), next)
        | Number(v, next) -> Some(v, next)
        | _ -> None)
    and (|Number|_|) = memoize c "Number" (function
        | Next(c, next) when System.Char.IsDigit(c) -> Some(Leaf c, next)
        | _ -> None)
    match("1+2+3",0) with
    | Xs(v, Empty) -> 
        // Result should be left-associative
        Assert.Equal(show <| Interior(Interior(Leaf('1'),Leaf('2')),Leaf('3')), show v)
    | _ -> failwith "Could not parse"

#nowarn "0040" // Allow object recursion without warnings so we can write recursive memoized rules
[<Fact>]
let ``Should be able to parse indirect left-recursive grammers``() =
    let (|Next|Empty|) = function
    | (input : string), pos when pos < input.Length -> Next(input.[pos], (input, pos+1))
    | _ -> Empty

    let c = ParserContext()
    // define an intermediate production "E" to make recursion indirect
    let rec (|Xs|_|) = memoize c "Xs" (function
        | E(lhs, Next('+', Number(rhs, next))) -> Some(Interior(lhs, rhs), next)
        | Next(c, next) when System.Char.IsDigit(c) -> Some(Leaf c, next)
        | _ -> None)
    and (|E|_|) = memoize c "E" (function
        | Xs(v, next) -> Some(v, next)
        | _ -> None)
    and (|Number|_|) = memoize c "Number" (function
        | Next(c, next) when System.Char.IsDigit(c) -> Some(Leaf c, next)
        | _ -> None)
    // It's an Xs, and it's also an E
    match("1+2+3",0) with
    | Xs(v, Empty) -> 
        Assert.Equal(Interior(Interior(Leaf('1'),Leaf('2')),Leaf('3')), v)
    | _ -> failwith "Could not parse"
    match("1+2+3",0) with
    | E(v, Empty) -> 
        Assert.Equal(Interior(Interior(Leaf('1'),Leaf('2')),Leaf('3')), v)
    | _ -> failwith "Could not parse"

[<Fact>]
let ``More complex indirect left-recursive grammers``() =
    let (|Next|Empty|) = function
    | (input : string), pos when pos < input.Length -> Next(input.[pos], (input, pos+1))
    | _ -> Empty

    let c = ParserContext()
    // define an intermediate production "E" to make recursion indirect
    let rec (|CompoundExpression|_|) = memoize c "CompoundExpression" (function
        | E(v, Next('+', Next('x', next))) -> Some(v+1, next)
        | Next('x', next) -> Some(1, next)
        | _ -> None)
    and (|E|_|) = memoize c "E" (function
        | CompoundExpression(v, next) -> Some(v, next)
        | _ -> None)
    // It's an Xs, and it's also an E
    match("x+x",0) with
    | CompoundExpression(v, Empty) -> Assert.Equal(2, v)
    | _ -> failwith "Could not parse"
    match("x+x",0) with
    | E(v, Empty) -> Assert.Equal(2, v)
    | _ -> failwith "Could not parse"

