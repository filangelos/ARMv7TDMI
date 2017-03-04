module App.Main

// This must be before everything else
Fable.Core.JsInterop.importAll("babel-polyfill") |> ignore

open Fable.Import
open App.Message

let main () =
    Browser.console.log message

do
    main ()
