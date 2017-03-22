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

    let mutable debug = MachineState.make ()
    let mutable maxpc = 0

    // Get Referencence on DOM Elements
    let exploreBtn = document.getElementById("explore")     // open button
    let saveBtn = document.getElementById("save")           // save button
    let indentBtn = document.getElementById("indent")       // auto-indent button
    let debugBtn = document.getElementById("debug")         // debug button
    let stepBtn = document.getElementById("step")           // step button
    let backBtn = document.getElementById("back")           // back button
    let stopBtn = document.getElementById("stop")           // stop button
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
            let assembly : string = sprintf "%O" (window?code?getValue()) + "\n"
            debug <- assembly |> tokenise |> Parse |> buildAST |> init
            maxpc <- debug |> (Optics.get MachineState.AST_) |> List.rev |> List.head |> snd
            updateUI debug
            null )
        stepBtn.onclick <- ( fun _ ->
            debug <- step debug
            updateUI debug
            let r15 = Optics.get MachineState.Register_ R15 debug
            match r15 = maxpc with
            | false -> debug <- Optics.set MachineState.Register_ R15 (r15+4) debug
            | true -> ()
            null )
        stopBtn.onclick <- ( fun _ ->
            debug <- MachineState.make ()
            maxpc <- 0
            updateUI (step debug)
            null )
        // run button
        runBtn.onclick <- ( fun _ -> 
            let assembly : string = sprintf "%O" (window?code?getValue()) + "\n"
            let execute = tokenise >> Parse >> buildAST >> init >> execute
            updateUI (execute assembly)
            null )
        // docs button
//        docsBtn.onclick <- ( fun _ -> null)
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
