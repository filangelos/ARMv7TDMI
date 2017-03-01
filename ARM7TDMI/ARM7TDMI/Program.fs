namespace ARM7TDMI

module Program =

    open MachineState
    open Instructions
    open Tokeniser

    [<EntryPoint>]
    let main argv =
        printfn "Running tokeniseTest:"
        Tokeniser.tokeniseTest
        printfn "Finished tokeniseTest"
        let x: MachineState = MachineState.make ()
        let y: Data = (^.) R0 x
        let z: MachineState = (^=) R0 5 x
        let u: MachineState = Optics.set MachineState.Flag_ N true z
        printfn "%A" x
        0