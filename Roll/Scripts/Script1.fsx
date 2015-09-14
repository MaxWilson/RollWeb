
#I __SOURCE_DIRECTORY__
#load @"load-references.fsx"
#load @"..\Util.fs"
      @"..\DataDefs.fs"
      @"..\Parser.fs"
      @"..\Dice.fs"
open mdw
open mdw.Parser.Impl
let spec = Parser.ParseCommand("avg.20d6-d4-d4") |> Dice.Instance.Resolve
