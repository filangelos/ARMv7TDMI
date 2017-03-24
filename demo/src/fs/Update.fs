namespace ARM7TDMI

(* 
    High Level Programming @ Imperial College London # Spring 2017
    Project: A user-friendly ARM7TDMI assembler and simulator in F# and Web Technologies ( Github Electron & Fable Compliler )

    Contributors: Angelos Filos

    Module: Update
    Description: Update UI Elm Architecture
*)

module Update =

    open Fable.Core
    open Fable.Import.Browser
    open Fable.Core.JsInterop
    open MachineState

    // Update UI
    let updateUI (state: MachineState) : unit =
        // update a single register
        let updateRegister ((id, content): RegisterID * Data) : unit =
            let fid = sprintf "%O" id?Case
            let el = document.getElementById(fid)
            match content <> 0 with
            | false ->
                el.setAttribute("style", "background: #fcfcfc")
                el.innerHTML <- sprintf "0x%X" content
            | true  ->
                el.setAttribute("style", "background: #fbbc05")
                el.innerHTML <- sprintf "0x%X" content

        let updateFlag ((id, bit): FlagID * bool) : unit =
            let fid = sprintf "%O" id?Case
            let el = document.getElementById(fid)
            match bit with
                | false ->
                    el.setAttribute("style", "background: #fcfcfc")
                    el.innerHTML <- sprintf "%i" 0
                | true -> 
                    el.setAttribute("style", "background: #4285f4")
                    el.innerHTML <- sprintf "%i" 1

        Map.toList (Optics.get MachineState.Registers_ state)
        |> List.map updateRegister
        |> ignore

        Map.toList (Optics.get MachineState.Flags_ state)
        |> List.map updateFlag
        |> ignore

        let pc : int = state |> (Optics.get MachineState.Register_ R15)

        let decorations =
            createObj [ 
                        "options" ==> createObj [ "isWholeLine" ==> true
                                                  "className" ==> "myLineDecoration" ] ]

        window?code?deltaDecorations([], decorations) |> ignore