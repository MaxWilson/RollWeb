
#I __SOURCE_DIRECTORY__
#load @"load-references.fsx"
#load @"..\Util.fs"
      @"..\DataDefs.fs"
      @"..\Parser.fs"
      @"..\Dice.fs"
open mdw
open mdw.DataDefs

(|CompoundExpression|_|) ("d20:14?", 0)

(function
        | Next('(', CompoundExpression(lhs, Next(')', next))) -> Some(lhs, next)
        | CheckTerm(v, next) -> Some(v, next)
        | Number(n, Next('.', CompoundExpression(v, next))) -> 
            Some(Repeat(n, v), next)
        | SimpleExpression(v, next) -> Some(Single(v), next)
        | _ -> None) ("20.d20:14?", 0)

match ("d20:14?", 0) with
| Predicate(roll, target, ResultTerm(consequent, next)) -> Some(roll, target, consequent, next)
| _ -> None
System.Text.RegularExpressions.Regex.Replace("333d4->22", "\d+", "X")