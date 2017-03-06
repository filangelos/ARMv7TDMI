namespace ARM7TDMI

(* 
    High Level Programming @ Imperial College London # Spring 2017
    Project: A user-friendly ARM7TDMI assembler and simulator in F# and Web Technologies ( Github Electron & Fable Compliler )

    Contributors: Pranav Prabhu

    Module: Parser
    Description: Parse individual instruction and initiate correct function call
*)
<<<<<<< HEAD
=======

open System
//open Instructions
//open Common
>>>>>>> a77e12d18eecd1bd7efd3ee99970408f8df9f5e1

module Parser =

    open System
    open Instructions
    open Common

    type FlagSet =
        | Twoflags of bool*bool 
        | Oneflag of bool

    type Expr = 
        | Instruction of string*RegisterID*RegisterID*Operand*FlagSet
        