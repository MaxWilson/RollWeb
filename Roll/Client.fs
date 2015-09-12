namespace Roll

open WebSharper
open WebSharper.JavaScript
open WebSharper.JQuery
open WebSharper.UI.Next
open WebSharper.UI.Next.Notation

[<JavaScript>]
module Client =    
    let [<Literal>] TemplateHtmlPath = __SOURCE_DIRECTORY__ + "/index.html"

    type IndexTemplate = Templating.Template<TemplateHtmlPath> 

    let Rolls =
        ListModel.FromSeq [
            "2d6+3", 7
        ]

    let Main =
        JQuery.Of("#main").Empty().Ignore

        let nextRoll = Var.Create ""

        IndexTemplate.Main.Doc(
            ListContainer =
                (ListModel.View Rolls |> Doc.Convert (fun (roll, result) ->
                    IndexTemplate.ListItem.Doc(LastRoll = View.Const roll, Result = View.Const (result.ToString())))
                ),
            NextRoll = nextRoll,
            Roll = (fun e ->
                Rolls.Add(("2d6", Roller.Instance.Resolve((2,6))))
                Var.Set nextRoll "")
        )
        |> Doc.RunById "main"
