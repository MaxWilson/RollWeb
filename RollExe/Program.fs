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
        | StringMatch [""] true -> printfn "Eh?"; loop()
        | StringMatch ["q"; "quit"] true -> ()
        | v -> 
            try
                printfn "%d" (mdw.Parser.Parse v |> mdw.Dice.Instance.Resolve)
            with e -> printfn "%s" (e.Message)
            loop()
    loop()
    0
