#I __SOURCE_DIRECTORY__
#load @"load-references.fsx"
#load @"..\Util.fs"
      @"..\Packrat.fs"
      @"..\DataDefs.fs"
      @"..\Parser.fs"
      @"..\Dice.fs"
open mdw
open mdw.DataDefs
#r @"..\..\packages\xunit.runner.visualstudio.2.0.1\build\_common\xunit.abstractions.dll"
#r @"..\..\packages\xunit.assert.2.0.0\lib\portable-net45+win+wpa81+wp80+monotouch+monoandroid+Xamarin.iOS\xunit.assert.dll"
#r @"..\..\packages\xunit.extensibility.core.2.0.0\lib\portable-net45+win+wpa81+wp80+monotouch+monoandroid+Xamarin.iOS\xunit.core.dll"
#r "System.Runtime"
open Xunit
#nowarn "0040"
#nowarn "0058"
