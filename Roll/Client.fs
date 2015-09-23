namespace mdw.Roll

open WebSharper
open WebSharper.JavaScript
open WebSharper.JQuery
open WebSharper.UI.Next
open WebSharper.UI.Next.Notation
open WebSharper.UI.Next.Client
open mdw

[<JavaScript>]
module Client =    
    open System.Web.UI.WebControls
    open WebSharper.UI.Next.Html

    let helpMessage = """Some usage examples:
3d8+2 means "roll 3d8+2 and give me the result"
2.3d8+2 means "roll 3d8+2 twice and give me the sum"
avg.3d8+2 means "what is the average value of 3d8+2?"
13?3d8+2 means "Roll d20, and if it's at least 13, roll 3d8+2 (doubling on crits as usual) and give me the result"
avg.13?3d8+2 means "Do the 13?3d8+2 computation but give me the average instead of any particular instance"
d20A means "d20 at advantage". Can also do d8A, or d20D for disadvantage, etc. Not case-sensitive.
13D? means roll 13 or better at disadvantage. (It's similar to but not the same as asking for d20D.)

The "Explain" button shows which rolls led to that result.

Concrete example: say I want an easy way to compute the average DPR of a 17th level Oath of Vengeance paladin with Hunter's Mark up against an AC 22 Ancient Red Dragon. Paladin has advantage and +11 to hit, so he hits on an 11 or better, with two attacks. I can ask for "avg.2.11A?
You can also ask for "avg.3.15A?d8+d6+5d8" and the answer is 55.59.
"""

    type RollRecord = 
        { Key : Key; Description : string; Value: string; Explain: string}
        static member Create(descr, value, explain) =
            { Key = Key.Fresh(); Description = descr; Value = value; Explain = explain}
    let Rolls =
        ListModel.FromSeq<RollRecord> [ ]
        
    let renderItem m =
        tr [
            td [
                Doc.TextNode m.Description
            ]
            td [
                Doc.TextNode m.Value
            ]
            td[
                Doc.Button "Explain" [] (fun _ -> JS.Alert(m.Explain))
            ]
        ]
    let RollForm =
        // We make a variable to contain the new to-do item.
        let rvInput = Var.Create ""
        form [
            div [
                label [Doc.TextNode "New entry: "]
                // Here, we make the Input box, backing it by the reactive variable.
                Doc.Input [attr.autofocus "autofocus"] rvInput
            ]
            // Once the user clicks the submit button...
            Doc.Button "Submit" [] (fun _ ->
                // We construct a new ToDo item
                let spec = Var.Get rvInput
                try
                    let result, explain = Parser.ParseCommand spec |> Dice.Instance.Resolve
                    let todo = RollRecord.Create (spec, result, explain)
                    // This is then added to the collection, which automatically
                    // updates the presentation.
                    Rolls.Add todo
                with e ->
                    JS.Alert (sprintf "Error: %s. Try a simple expression like '2d6' or 'avg.5d4+2'." (e.Message))
                )
        ]
    let renderRolls rolls =
        div [        
            Doc.Link "Help" [attr.style "float: right"] (fun _ -> JS.Alert helpMessage)
            Doc.Element "h1" [] [Doc.TextNode "Recent rolls:"]
            RollForm
            table [
                tbody [
                    ListModel.View rolls 
                    |> View.Map (List.ofSeq >> List.rev >> Seq.ofList)
                    |> Doc.ConvertBy (fun i -> i.Key) renderItem                    
                ]
            ]
        ]
    let Main =
        JQuery.Of("#main").Empty().Ignore
        renderRolls Rolls |> Doc.RunById "main"
