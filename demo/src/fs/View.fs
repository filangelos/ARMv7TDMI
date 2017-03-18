namespace ARM7TDMI

(*
    High Level Programming @ Imperial College London # Spring 2017
    Project: A user-friendly ARM7TDMI assembler and simulator in F# and Web Technologies ( Github Electron & Fable Compliler )

    Contributors: Angelos Filos

    Module: Instructions
    Description:
*)

module View =

    open Fable.Core
    open Fable.Import.Browser
    open Fable.Core.JsInterop
    open MachineState

    // Get Referencence on DOM Elements
    let exploreBtn = document.getElementById("explore")     // open button
    let saveBtn = document.getElementById("save")           // save button
    let indentBtn = document.getElementById("indent")       // auto-indent button
    let debugBtn = document.getElementById("debug")         // debug button
    let runBtn = document.getElementById("run")             // run button
    let docsBtn = document.getElementById("docs")           // docs button
    let warnBtn = document.getElementById("warn")           // warn button
    let themeBtn = document.getElementById("theme")         // warn button
    let editorDiv = document.getElementById("editor")       // editor div
    let dashboardDiv = document.getElementById("dashboard") // dashboard div

    // Event Listeners
    let actions : unit -> unit = fun _ ->
        // run button
        runBtn.onclick <- (fun _ -> console.log(window?code?getValue()); null)
        // theme button
        themeBtn.onclick <- (fun _ ->
            let options = 
                createObj [
                    "theme" ==> "vs-dark"
                ]
            console.log("theme changed to: 'vs-dark'")
            window?code?updateOptions(options))
        ()
