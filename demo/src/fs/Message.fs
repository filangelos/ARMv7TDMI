module App.Message

open Fable.Core
open Fable.Import.Browser
open Fable.Core.JsInterop

let getById<'T when 'T :> HTMLElement> id =
    document.getElementById(id) :?> 'T

let x = importMember<int->int> "../js/Editor.js"
let run = getById<HTMLButtonElement>("run")
let editor = getById<HTMLDivElement>("editor")

let event : unit = run.addEventListener_click(fun _ -> console.log(x(10)); null)

let message = importMember<string> "../js/Editor.js"
let editorR = importMember<obj> "../js/Editor.js"