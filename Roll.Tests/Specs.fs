module mdw.Roll.Tests
open Xunit
open FsCheck
open mdw
open DataDefs
open Dice

[<Fact>]
let ``Test rolls``() =
    Assert.Equal(8, Dice.Instance.Resolve(Simple(8,1)) |> fst)

[<Fact>]
let ``Test rolls by property``() =
    let between lower upper x =
        lower <= x && x <= upper
    let isWithinBounds d size =
        (d > 0 && size > 0) ==>
            lazy (between (d) (d*size) (Dice.Instance.Resolve(Simple(d, size)) |> fst))
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
        let result = roller.Resolve(spec : Compound) |> fst
        low <= result && result <= high
    Check.QuickThrowOnFailure (withinBounds 3 18 (Single(Simple(3,6))))

[<Fact>]
let Average() =
    Assert.Equal(3.5, Dice.Instance.Average(Single(Simple(1,6))))
    Assert.Equal(10.5, Dice.Instance.Average(Single(Simple(3,6))))

[<Theory>]
[<InlineData("3d6", 10.5)>]
[<InlineData("10.3d6", 105.)>]
[<InlineData("10.3d6+4", 145.)>]
[<InlineData("10.3d6-2", 85.)>]
[<InlineData("1d2A", 1.75)>]
[<InlineData("1d2D", 1.25)>]
[<InlineData("20.18?", 3.)>]
[<InlineData("(d20+11)-(d20+8)", 3.)>]
[<InlineData("10.18A?", 2.775)>]
[<InlineData("10.18D?", 0.225)>]
[<InlineData("20.18A?100", 555.)>]
[<InlineData("20.20?2d10", 22.)>]
let ``Complete-ish list of example roll specs``(input: string, expectedAverage: float) =
    let spec = Parser.Parse(input)
    let round (x : float) = System.Math.Round(x, 3) // round to three places
    Assert.Equal(round expectedAverage, round (Dice.Instance.Average(spec)))
    Dice.Instance.Resolve(spec) |> ignore // must not throw

[<Fact>]
let ``Sums should be left-associative``() =
    Assert.Equal(65., Parser.Parse "20d6-d4-d4" |> Dice.Instance.Average)

[<Theory>]
[<InlineData("3.2d4", "(X,X,X)->X")>]
[<InlineData("3.4d6", "(X,X,X)->X")>]
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
let ``Complete-ish list of example command specs``(input: string, expectedOutput: string) =
    let spec = Parser.ParseCommand(input)
    let output = Dice.Instance.Resolve(spec) |> fst
    if(expectedOutput <> null) then
        Assert.Equal<string>(expectedOutput, output)

[<Theory(Skip="Incomplete")>]
[<InlineData("20.d20:14?", 6)>]
[<InlineData("20.18?20?", 4.)>]
[<InlineData("20.d4A+d10+d20D:18?d10+5+d6", 0.)>]
let ``Example roll specs that aren't working yet``(input: string, expectedAverage: float) =
    Assert.Equal(expectedAverage, Dice.Instance.Average(Parser.Parse(input)))
