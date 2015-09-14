module mdw.Roll.Tests
open Xunit
open FsCheck
open mdw
open DataDefs
open Dice

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
        let result = roller.Resolve(spec : Compound)
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
let ``Complete-ish list of example roll specs``(input: string, expectedAverage: float) =
    let spec = Parser.Parse(input)
    Assert.Equal(expectedAverage, Dice.Instance.Average(spec))
    Dice.Instance.Resolve(spec) |> ignore // must not throw

[<Fact(Skip="Not implemented)")>]
let ``Sums should be left-associative``() =
    Assert.Equal(65., Parser.Parse "20d6-d4-d4" |> Dice.Instance.Average)

[<Theory>]
[<InlineData("3d6", null)>]
[<InlineData("avg.3d6", "10.5")>]
let ``Complete-ish list of example command specs``(input: string, expectedOutput: string) =
    let spec = Parser.ParseCommand(input)
    let output = Dice.Instance.Resolve(spec)
    if(expectedOutput <> null) then
        Assert.Equal<string>(expectedOutput, output)

[<Theory(Skip="Incomplete")>]
[<InlineData("1d2+", 1.75)>]
[<InlineData("1d2-", 1.25)>]
[<InlineData("20.18?", 3.)>]
[<InlineData("20.18+?", 0.2775)>]
[<InlineData("20.18-?", 0.0225)>]
[<InlineData("20.18+?100", 27.75)>]
[<InlineData("20.d4++d10+d20-:18?d10+5+d6", 0.)>]
let ``Example roll specs that aren't working yet``(input: string, expectedAverage: float) =
    Assert.Equal(expectedAverage, Dice.Instance.Average(Parser.Parse(input)))
