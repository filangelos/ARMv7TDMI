namespace Machine

module Program =

    open MachineState

    [<EntryPoint>]
    let main argv =
        let x: MachineState = MachineState.make ()
        let y: Data = (^.) R0 x
        let z: MachineState = (^=) R0 5 x
        printfn "%A" z
        0 // return an integer exit code