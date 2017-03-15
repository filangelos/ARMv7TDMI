module App.Main

// This must be before everything else
Fable.Core.JsInterop.importAll("babel-polyfill") |> ignore

open ARM7TDMI.MachineState
open ARM7TDMI.Instructions
open ARM7TDMI.Tokeniser
open ARM7TDMI.AST
open ARM7TDMI.Parser

open Fable.Import
open App.Message

let main () =
    Browser.console.log message
    Browser.console.log (x(5))

do
    main ()
