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
    open Instructions
    open Tokeniser
    open AST
    open Parser
    open MemoryInstructions
    open InstructionsInterfaces
    open Update

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
    let flagsDiv = document.getElementById("flags")         // flags div

    // Event Listeners
    let actions : unit -> unit = fun _ ->
        // explore button
        exploreBtn.onclick <- ( fun _ -> null )
        // save button
        saveBtn.onclick <- ( fun _ -> null )
        // indent button
        indentBtn.onclick <- ( fun _ -> null )
        // debug button
        debugBtn.onclick <- ( fun _ ->
            let assembly : string = sprintf "%O" (window?code?getValue())
            document.getElementById("R0").innerHTML <- "0x" + assembly
            null )
        // run button
        runBtn.onclick <- ( fun _ -> 
            let assembly : string = sprintf "%O" (window?code?getValue()) + "\n"
            let program = tokenise >> Parse >> buildAST >> init >> execute
            updateUI (program assembly)
            null )
        // docs button
        docsBtn.onclick <- ( fun _ -> null )
        // warn button
        warnBtn.onclick <- ( fun _ -> null )
        // theme button
        themeBtn.onclick <- ( fun _ ->
            let options = 
                createObj [
                    "theme" ==> "vs-dark"
                ]
            window?code?updateOptions(options) )
        ()
