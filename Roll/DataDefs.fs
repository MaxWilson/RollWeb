[<WebSharper.Core.Attributes.JavaScript>]
module DataDefs

type Simple = Simple of int * int
type Compound = Single of Simple | Plus of Simple * Compound
