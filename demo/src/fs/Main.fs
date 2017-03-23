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
    open Electron

    let xx = 0

    let main () =
        actions ()
//        render ()

    do
        main ()
