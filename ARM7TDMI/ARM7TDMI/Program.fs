namespace ARM7TDMI

module Program =

    open MachineState
    open Instructions
    open Tokeniser
    open AST
    open Parser
    open MemoryInstructions
    open InstructionsInterfaces

    [<EntryPoint>]
    let main argv =
//      Tokeniser.testTokeniser ()
    //    MemoryInstructions.simpleLDRSTRtest
//      AST.testAST ()
//        Parser.testParser ()
        InstructionsInterfaces.test()
        let x: MachineState = MachineState.make ()
        let y: Data = (^.) R0 x
        let z: MachineState = (^=) R0 5 x
        let u: MachineState = Optics.set MachineState.Flag_ N true z
        printfn "%A" x


        printfn "Testing pipeline:"
        let testInput = "MOV R0, #5\nMOV R1, #0x9\nADD R2, R1, R0\nSUBS R3, R0, R1"
        let result = (testInput |> tokenise |> Parse |> buildAST |> init |> execute)
        printfn "%A \n\n These instructions give the following final state:\n%A" testInput result
        0