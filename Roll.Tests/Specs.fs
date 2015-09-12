module Roll.Tests
open Xunit
open FsCheck
open DataDefs

[<Fact>]
let ``Test rolls``() =
    Assert.Equal(8, Roller.Instance.Resolve((8,1)))

[<Fact>]
let ``Test rolls by property``() =
    let between lower upper x =
        lower <= x && x <= upper
    let isWithinBounds d size =
        (d > 0 && size > 0) ==>
            lazy (between (d) (d*size) (Roller.Instance.Resolve((d, size))))
    Check.QuickThrowOnFailure(isWithinBounds)
    