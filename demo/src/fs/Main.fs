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

    // Front End
    open Fable.Import
    open View
    open Update

    let main () =
        let sand : MachineState = MachineState.make ()
//        initUI sand
//        Browser.console.log (R2 > R10)
        testAST ()
        actions ()

    do
        main ()
