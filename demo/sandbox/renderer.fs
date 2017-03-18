// Render Dashboard
    let renderDashboard (state: MachineState) : string =
        // render a single register
        let renderReg ((id, content): RegisterID * Data) : string =
              "<tr><td><div class=\"btn-group full-width\">"
            + sprintf "<button id=\"%O\" class=\"btn btn-reg\">%O</button>" id?Case id?Case
            + sprintf "<button class=\"btn btn-con\">Ox%x</button>" content
            + "<button class=\"btn btn-enc\">HEX</button>"
            + "<button class=\"btn btn-enc\">BIN</button>"
            + "<button class=\"btn btn-enc\">DEC</button>"
            + "</div></td></tr>"

        // core HTML
        let core : string =
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
            |> List.map renderReg
            |> List.fold (+) ""
        // glue everything together
        [
            "<table class=\"table text-center\"><tbody><tr><br></tr>"
            core
            "</tbody></table>"
        ]
        |> List.fold (+) ""