namespace ARM7TDMI

(* 
    High Level Programming @ Imperial College London # Spring 2017
    Project: A user-friendly ARM7TDMI assembler and simulator in F# and Web Technologies ( Github Electron & Fable Compliler )

    Contributors: Youssef Rizk

    Module: Instructions Interfaces
    Description: All the interfaces to the various instructions, pulling from ALU, Memory, and Branching instructions
*)

module InstructionsInterfaces

open Instructions
open MemoryInstructions

let executeOrNot (cond: option ConditionCode) (state: Machine State) =
    match cond with
    | Some EQ when ((^*) Z state) = true -> true
    | Some NE when ((^*) Z state) = false -> true
    | Some CS | Some HS when ((^*) C state) = true -> true
    | Some CC | Some LO when ((^*) C state) = false -> true
    | Some MI when ((^*) N state) = true -> true
    | Some EQ when ((^*) N state) = false -> true
    | Some VS when ((^*) V state) = true -> true
    | Some VC when ((^*) V state) = false -> true
    | Some HI when (((^*) C state) = true) && (((^*) Z state) = false) -> true
    | Some LS when (((^*) C state) = false) || (((^*) Z state) = true) -> true
    | Some GE when ((^*) N state) = ((^*) V state) -> true
    | Some LT when ((^*) N state) <> ((^*) V state) -> true
    | Some GT when (((^*) Z state) = false) && (((^*) N state) = ((^*) V state)) -> true
    | Some LE when (((^*) Z state) = true) || (((^*) N state) <> ((^*) V state)) -> true
    | Some AL -> true
    | None -> true
    | _ -> false

