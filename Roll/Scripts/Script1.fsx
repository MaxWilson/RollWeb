
#I __SOURCE_DIRECTORY__
#load @"load-references.fsx"
#load @"..\DataDefs.fs"
      @"..\Parser.fs"
      @"..\Roller.fs"

Roller.Instance.Resolve((1,4))
Roller.Instance.Resolve((4,1))
System.Random().Next(2)

let withinBounds high low spec seed =
    let roller = Resolver(new System.Random(seed))
    let result = roller.Resolve(spec : Compound)
    low <= result && result <= high
Check.QuickThrowOnFailure (withinBounds 3 18 (Single(Simple(3,6))))
