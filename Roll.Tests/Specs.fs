module Roll.Tests
open Xunit
open FsCheck
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
