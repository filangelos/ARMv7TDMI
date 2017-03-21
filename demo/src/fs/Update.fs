namespace ARM7TDMI

(* 
    High Level Programming @ Imperial College London # Spring 2017
    Project: A user-friendly ARM7TDMI assembler and simulator in F# and Web Technologies ( Github Electron & Fable Compliler )

    Contributors: Angelos Filos

    Module: Update
    Description:
*)

module Update =

    open Fable.Core
    open Fable.Import.Browser
    open Fable.Core.JsInterop
    open MachineState
//    open View

    // Update UI
    let updateUI (state: MachineState) : unit =
        // update a single register
        let updateRegister ((id, content): RegisterID * Data) : unit =
            let fid = sprintf "%O" id?Case
            let el = document.getElementById(fid)
            el.innerHTML <- sprintf "0x%X" content
(*        let renderRegister ((id, content): RegisterID * Data) : string =
              "<tr><td><div class=\"btn-group full-width\">"
            + sprintf "<button class=\"btn btn-reg\">%O</button>" id?Case
            + sprintf "<button id=\"%O\" class=\"btn btn-reg-con\">Ox%x</button>" id?Case  content
            + "<button class=\"btn btn-enc\">HEX</button>"
            + "<button class=\"btn btn-enc\">BIN</button>"
            + "<button class=\"btn btn-enc\">DEC</button>" 
            + "</div></td></tr>" *)

        let updateFlag ((id, bit): FlagID * bool) : unit =
            let fid = sprintf "%O" id?Case
            let el = document.getElementById(fid)
            let boolToZeroOne = function
                | false -> 0 | true -> 1
            el.innerHTML <- sprintf "%i" (boolToZeroOne bit)

        Map.toList (Optics.get MachineState.Registers_ state)
        |> List.map updateRegister
        |> ignore

        Map.toList (Optics.get MachineState.Flags_ state)
        |> List.map updateFlag
        |> ignore
(*        let renderFlag ((id, bit): FlagID * bool) : string =
            let boolToZeroOne = function
                | false -> 0 | true -> 1
            sprintf "<button class=\"btn btn-mini btn-flag\">%O</button>" id?Case
            + sprintf "<button id=\"%O\" class=\"btn btn-mini btn-flag-con\">%i</button>" id?Case (boolToZeroOne bit) *)

(*        // Registers HTML
        let registers : string =
            Optics.get MachineState.Registers_ state
            |> Map.toList
             This is necessary under Fable because in F# D.Us implement IComparable
               according to the order the cases are defined, while with Fable they are
               transpiled to a "Cases" object with string properties, therefore
               they follow string comparison 
            |> List.map ( snd )
            |> List.zip 
                [ R0 ; R1 ; R2 ; R3 ; R4
                  R5 ; R6 ; R7 ; R8 ; R9
                  R10 ; R11 ; R12 ; R13 ; R14 ; R15 ] 
            |> List.map renderRegister
            |> List.fold (+) ""

        // glue everything together
        [
            "<table class=\"table text-center\"><tbody><tr><td><br></td></tr>"
            registers
//            renderFlags state
            "</tbody></table>"
        ]
        |> List.fold (+) "" *)