
#I __SOURCE_DIRECTORY__
#load @"load-references.fsx"
#load @"..\Util.fs"
      @"..\DataDefs.fs"
      @"..\Parser.fs"
      @"..\Dice.fs"
open mdw
open mdw.Parser.Impl
let spec = Parser.Parse("10.3d6+4")
let x = Dice.Instance.Average(spec)
match ("3d6+4", 0) with
| SumSimplesExpression(v, Empty) -> v
| _ -> Util.nomatch()
