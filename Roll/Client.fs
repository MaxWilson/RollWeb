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

    type RollRecord = 
        { Key : Key; Description : string; Value: string}
        static member Create(descr, value) =
            { Key = Key.Fresh(); Description = descr; Value = value}
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
                let result = Parser.ParseCommand spec |> Dice.Instance.Resolve
                let todo = RollRecord.Create (spec, result)
                // This is then added to the collection, which automatically
                // updates the presentation.
                Rolls.Set(Seq.append [todo] Rolls.Value)
                )
        ]
    let renderRolls rolls =
        div [        
            Doc.Element "h1" [] [Doc.TextNode "Recent rolls:"]
            RollForm
            table [
                tbody [
                    ListModel.View rolls 
                    // How to do |> List.rev here?
                    |> Doc.ConvertBy (fun i -> i.Key) renderItem                    
                ]
            ]
        ]
    let Main =
        JQuery.Of("#main").Empty().Ignore
        renderRolls Rolls |> Doc.RunById "main"
