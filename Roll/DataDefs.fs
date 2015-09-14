[<WebSharper.Core.Attributes.JavaScript>]
module mdw.DataDefs 

type Simple = 
| Simple of int * int
| Adv of int * int
| Disadv of int * int

type Compound = 
| Single of Simple 
| Sum of Compound * Compound
| MultByConstant of int * Compound
| Repeat of int * Compound

 