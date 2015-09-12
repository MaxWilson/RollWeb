[<WebSharper.Core.Attributes.JavaScript>]
module DataDefs

type Simple = int * int
type Compound = Single of Simple | Plus of Simple * Compound
