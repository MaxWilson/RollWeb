
#I __SOURCE_DIRECTORY__
#load @"load-references.fsx"
#load @"..\Util.fs"
      @"..\DataDefs.fs"
      @"..\Parser.fs"
      @"..\Dice.fs"
open mdw
open mdw.DataDefs


(|CompoundExpression|_|) ("avg.20d6-(d4-d4)", 0)
System.Text.RegularExpressions.Regex.Replace("333d4->22", "\d+", "X")