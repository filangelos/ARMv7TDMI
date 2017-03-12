module App.Message

open Fable.Core.JsInterop

let message = importMember<string> "../js/Editor.js"
let editorR = importMember<function> "../js/Editor.js"