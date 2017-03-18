namespace ARM7TDMI

(* 
    High Level Programming @ Imperial College London # Spring 2017
    Project: A user-friendly ARM7TDMI assembler and simulator in F# and Web Technologies ( Github Electron & Fable Compliler )

    Contributors: Angelos Filos

    Module: Instructions
    Description:
*)

module Update =

    open Fable.Core
    open Fable.Import.Browser
    open Fable.Core.JsInterop
    open MachineState
    open View

    // Render Dashboard
    let renderDashboard (state: MachineState) : string =
        // render a single register
        let renderRegister ((id, content): RegisterID * Data) : string =
              "<tr><td><div class=\"btn-group full-width\">"
            + sprintf "<button id=\"%O\" class=\"btn btn-reg\">%O</button>" id?Case id?Case
            + sprintf "<button class=\"btn btn-reg-con\">Ox%x</button>" content
(*            + "<button class=\"btn btn-enc\">HEX</button>"
            + "<button class=\"btn btn-enc\">BIN</button>"
            + "<button class=\"btn btn-enc\">DEC</button>" *)
            + "</div></td></tr>"

        let renderFlags (state: MachineState) : string =
            let boolToZeroOne = function
                | false -> 0 | true -> 1
            "<tr><td><div class=\"btn-group full-width\">"
            + sprintf "<button id=\"N\" class=\"btn btn-flag\">N</button>"
            + sprintf "<button class=\"btn btn-flag-con\">%i</button>" (boolToZeroOne (state.StatusBits.Item N))
            + sprintf "<button id=\"Z\" class=\"btn btn-flag\">Z</button>"
            + sprintf "<button class=\"btn btn-flag-con\">%i</button>" (boolToZeroOne (state.StatusBits.Item Z))
            + sprintf "<button id=\"C\" class=\"btn btn-flag\">C</button>"
            + sprintf "<button class=\"btn btn-flag-con\">%i</button>" (boolToZeroOne (state.StatusBits.Item C))
            + sprintf "<button id=\"V\" class=\"btn btn-flag\">V</button>"
            + sprintf "<button class=\"btn btn-flag-con\">%i</button>" (boolToZeroOne (state.StatusBits.Item V))


        // core HTML
        let registers : string =
            Optics.get MachineState.Registers_ state
            |> Map.toList
            (* This is necessary under Fable because in F# D.Us implement IComparable
               according to the order the cases are defined, while with Fable they are
               transpiled to a "Cases" object with string properties, therefore
               they follow string comparison *)
            |> List.map ( snd )
            |> List.zip 
                [ R0 ; R1 ; R2 ; R3 ; R4
                  R5 ; R6 ; R7 ; R8 ; R9
                  R10 ; R11 ; R12 ; R13 ; R14 ; R15 ]
            |> List.map renderRegister
            |> List.fold (+) ""
        // glue everything together
        [
            "<table class=\"table text-center\"><tbody>"
            registers
            "<tr><td><br></td></tr>"
            renderFlags state
            "</tbody></table>"
        ]
        |> List.fold (+) ""

    let initUI (state: MachineState) : unit =
        dashboardDiv.innerHTML <- renderDashboard state