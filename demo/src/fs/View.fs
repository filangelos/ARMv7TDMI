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
        // 
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

    // Render Dashboard
    let renderDashboard (state: MachineState) : string =
        // render a single register
        let renderReg ((id, content): RegisterID * Data) : string =
              "<tr><td><div class=\"btn-group full-width\">"
            + sprintf "<button id=\"%O\" class=\"btn btn-reg\">%O</button>" id?Case id?Case
            + sprintf "<button class=\"btn btn-con\">%X</button>" content
            + "<button class=\"btn btn-enc\">HEX</button>"
            + "<button class=\"btn btn-enc\">BIN</button>"
            + "<button class=\"btn btn-enc\">DEC</button>"
            + "</div></td></tr>"

        // core HTML
        let core : string =
            Optics.get MachineState.Registers_ state
            |> Map.toList
// This is necessary under Fable because in F# D.Us implement IComparable
// according to the order the cases are defined, while with Fable they are
// transpiled to a "Cases" object with string properties, therefore
// they follow string comparison
            |> List.map ( snd )
            |> List.zip 
                [ R0 ; R1 ; R2 ; R3 ; R4
                  R5 ; R6 ; R7 ; R8 ; R9
                  R10 ; R11 ; R12 ; R13 ; R14 ; R15 ]
// End of "mess"
            |> List.map renderReg
            |> List.fold (+) ""
        // glue everything together
        [
            "<table class=\"table text-center\"><tbody><tr><br></tr>"
            core
            "</tbody></table>"
        ]
        |> List.fold (+) ""

    let initUI (state: MachineState) : unit =
        dashboardDiv.innerHTML <- renderDashboard state