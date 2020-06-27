// Learn more about F# at https://fsharp.org
// See the 'F# Tutorial' project for more help.

[<EntryPoint>]
let main argv =
    match argv with
    | [|fileName|] when System.IO.File.Exists fileName ->
        System.IO.File.ReadAllText fileName
        |> Model.Parse.parse
        |> Model.Execution.execute
    | [|programTxt|] ->
        programTxt
        |> Model.Parse.parse
        |> Model.Execution.execute
    | _ ->
        printfn """Usage: DurgaToy.exe "<file name or program as a string>" """
    0 // return an integer exit code
