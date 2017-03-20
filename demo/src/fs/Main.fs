namespace ARM7TDMI

(* 
    High Level Programming @ Imperial College London # Spring 2017
    Project: A user-friendly ARM7TDMI assembler and simulator in F# and Web Technologies ( Github Electron & Fable Compliler )

    Contributors: Youssef Rizk

    Module: Instructions
    Description: 
*)

module Main =

    // Fable Configuration
    Fable.Core.JsInterop.importAll("babel-polyfill") |> ignore

    // Back End
    open MachineState
    open Instructions
    open Tokeniser
    open AST
    open Parser
    open MemoryInstructions
    open InstructionsInterfaces

    // Front End
    open Fable.Import
    open View
    open Update

    let xx = 0

    let main () =
(*        let t =
            [TokInstr1 MOV ; TokReg R0 ; TokComma; TokReg R3]
//            [TokInstr3 ADD; TokReg R1; TokComma; TokReg R2; TokComma; TokReg R3; TokNewLine; TokInstr1 MOV; TokReg R2; TokComma; TokReg R1; TokEOF]
            |> Parse
            |> printfn "%A" *)
(*        let test = 
            printfn "Testing pipeline:"
            let testInput = "MOV R1, #72\nMOV R0, R1\nMOV R3, #1341\n"
            let program = tokenise >> Parse >> buildAST >> init >> execute
            let result = (testInput |> tokenise |> Parse |> buildAST |> init |> execute)
            printfn "%A \n\n These instructions give the following final state:\n%A" testInput result *)
//        let sand : MachineState = MachineState.make ()
//        Browser.console.log (R2 > R10)
//        testAST ()
//        test
        //t
        actions ()

    do
        main ()
