[<WebSharper.Core.Attributes.JavaScript>]
module mdw.DataDefs 
type Simple = Simple of int * int
type Compound = Single of Simple | Sum of Simple * Compound
