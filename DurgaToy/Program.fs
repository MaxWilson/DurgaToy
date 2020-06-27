// Learn more about F# at https://fsharp.org
// See the 'F# Tutorial' project for more help.

[<EntryPoint>]
let main argv =
    match argv with
    | [|programTxt|] ->
        programTxt
        |> Model.Parse.parse
        |> Model.Execution.execute
    | _ ->
        printfn """Usage: DurgaToy.exe "<program>" """
    0 // return an integer exit code
