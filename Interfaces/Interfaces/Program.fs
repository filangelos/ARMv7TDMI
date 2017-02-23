namespace Interfaces

module Program =

    open Register
    open Flag
    open MemoryLocation
    open MachineState

    [<EntryPoint>]
    let main argv =
        let R: Register = Register.make ( "R0" )
        // new Register record with updated value 
        let X = Optics.set Register.value_ R 12u
        let Y = Optics.get Register.value_ X
        let O: Operand = OLiteral 13u
        let F: Flag = Flag.make ( 'N' )
        let f1 = Optics.set Flag.bit_ F false
        printfn "Registers\nR: %A \nX: %A \nR: %A \nY: %A" R X R Y
        printfn "Flags\nF: %A\nf1: %A" F f1

        // Machine State Tests
        let state: MachineState = MachineState.make ()
        state.

        0 // return an integer exit code