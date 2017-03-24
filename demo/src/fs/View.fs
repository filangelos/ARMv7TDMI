namespace ARM7TDMI

(*
    High Level Programming @ Imperial College London # Spring 2017
    Project: A user-friendly ARM7TDMI assembler and simulator in F# and Web Technologies ( Github Electron & Fable Compliler )

    Contributors: Angelos Filos

    Module: Instructions
    Description:
*)

module View =

    open System.Text.RegularExpressions
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
    open Electron

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
    let themeBtn = document.getElementById("theme")         // warn button
    let editorDiv = document.getElementById("editor")       // editor div
    let dashboardDiv = document.getElementById("dashboard") // dashboard div
    let flagsDiv = document.getElementById("flags")         // flags div

    // Attach Event Listeners
    let actions : unit -> unit = fun _ ->
        // explore button
        exploreBtn.onclick <- ( fun _ -> 
            match readFromFileDialog() with
            | Some (fName,data) ->
                window?code?setValue(data) |> ignore
            | None -> printfn "No file name given!"
            null )
        // save button
        saveBtn.onclick <- ( fun _ -> 
            let data = sprintf "%O" (window?code?getValue())
            let fn = writeFromFileDialog data
            printfn "%A" fn
            null )
        // indent button
        indentBtn.onclick <- ( fun _ -> 
            let current : string = sprintf "%O" (window?code?getValue())
            let indented = Regex.Replace(current, "[\t]", " ")
            window?code?setValue(indented) |> ignore
            null )
        // debug button
        debugBtn.onclick <- ( fun _ ->
            let assembly : string = sprintf "%O" (window?code?getValue()) + "\n"
            debug <- assembly |> tokenise |> Parse |> buildAST |> init
            maxpc <- debug |> (Optics.get MachineState.AST_) |> List.rev |> List.head |> snd
            updateUI debug
            stepBtn.className <- "btn btn-positive"
            stopBtn.className <- "btn btn-negative"
            null )
        stepBtn.onclick <- ( fun _ ->
            let r15 = Optics.get MachineState.Register_ R15 debug
            match r15 = maxpc with
            | false ->
                debug <- step debug
                updateUI debug
                debug <- Optics.set MachineState.Register_ R15 (r15+4) debug
                printfn "at line %i" (r15 / 4 + 1)
            | true ->
                stepBtn.className <- "btn btn-default"
                stopBtn.className <- "btn btn-default"
                ()
            null )
        stopBtn.onclick <- ( fun _ ->
            debug <- MachineState.make ()
            maxpc <- 0
            updateUI (step debug)
            stepBtn.className <- "btn btn-default"
            stopBtn.className <- "btn btn-default"
            null )
        // run button
        runBtn.onclick <- ( fun _ ->
            let assembly : string = sprintf "%O" (window?code?getValue()) + "\n"
            let execute = tokenise >> Parse >> buildAST >> init >> execute
            updateUI (execute assembly)
            null )
        // docs button
        docsBtn.onclick <- ( fun _ ->
            shell.openExternal("https://github.com/filangel/HLP/tree/master/demo") |> ignore
            null)
        // theme button
        themeBtn.onclick <- ( fun _ ->
            let current : string = sprintf "%O" (window?code?getRawConfiguration()?theme)
            let theme = 
                match current with
                | "vs" -> "vs-dark"
                | _ -> "vs"
            let options = 
                createObj [
                    "theme" ==> theme
                ]
            window?code?updateOptions(options) )
        ()
