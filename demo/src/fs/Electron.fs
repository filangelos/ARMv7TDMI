namespace ARM7TDMI

(* 
    High Level Programming @ Imperial College London # Spring 2017
    Project: A user-friendly ARM7TDMI assembler and simulator in F# and Web Technologies ( Github Electron & Fable Compliler )

    Contributors: Angelos Filos

    Module: Electron
    Description:
*)

open Fable.Import
open Fable.Core
open Fable.Core.JsInterop

[<Import("*", "electron")>]
module electron =
    type YY = X of int
