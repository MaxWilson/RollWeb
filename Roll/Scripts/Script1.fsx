
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
