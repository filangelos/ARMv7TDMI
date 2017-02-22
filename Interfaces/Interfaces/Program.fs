namespace Interfaces

module Program =

    open Register
    open Flags

    [<EntryPoint>]
    let main argv =
        let R: Register = { id = "R0"; value = 13u }
        // new Register record with updated value 
        let X = Optics.set Register.value_ R 12u
        let Y = Optics.get Register.value_ X
        let O: Operand = OLiteral 13u
        printfn "R: %A \nX: %A \nR: %A \nY: %A" R X R Y
        0 // return an integer exit code