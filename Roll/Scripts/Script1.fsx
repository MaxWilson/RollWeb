
#I __SOURCE_DIRECTORY__
#load @"load-references.fsx"
#load @"..\DataDefs.fs"
      @"..\Parser.fs"
      @"..\Roller.fs"

Roller.Instance.Resolve((1,4))
Roller.Instance.Resolve((4,1))
System.Random().Next(2)