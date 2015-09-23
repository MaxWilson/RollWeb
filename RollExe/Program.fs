open mdw

// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.



[<EntryPoint>]
let main argv = 
    let (|StringMatch|) (args: string list) (input : string) =
        if List.exists((=) (input.ToLowerInvariant())) args then
            true
        else
            false
    let rec loop() =
        printf ">"
        match System.Console.ReadLine().Trim() with
        | StringMatch [""] true -> printfn "Enter a command, e.g. avg.2d9+3."; loop()
        | StringMatch ["q"; "quit"] true -> ()
        | v -> 
            try
                let result, explain = (mdw.Parser.ParseCommand v |> mdw.Dice.Instance.Resolve)
                if (explain <> result) then
                    printfn "Computing: %s" explain
                printfn "%s" result
            with e -> printfn "%s" (e.Message)
            loop()
    loop()
    0
