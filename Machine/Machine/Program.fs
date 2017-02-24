namespace Machine

module Program =

    open MachineState

    [<EntryPoint>]
    let main argv =
        let x: MachineState = MachineState.make ()
        let y: Data = (^.) R0 x
        let z: MachineState = (^=) R0 5 x
        let u: MachineState = Optics.set MachineState.Flag_ N true z
        printfn "%A" u
        0 // return an integer exit code