﻿namespace ARM7TDMI 

(* 
    High Level Programming @ Imperial College London # Spring 2017
    Project: A user-friendly ARM7TDMI assembler and simulator in F# and Web Technologies ( Github Electron & Fable Compliler )

    Contributors: Angelos Filos, Baron Khan, Youssef Rizk, Pranav Prabhu

    Module: Common
    Description: 
*)

[<AutoOpen>]
module Common =

    /// enumerate all values of a D.U. 
    let enumerator<'T> =
        FSharp.Reflection.FSharpType.GetUnionCases(typeof<'T>)
        |> Array.map (fun c ->  Reflection.FSharpValue.MakeUnion(c,[||]) :?> 'T)

    /// Raw data Type
    type Data = int
    /// Cast Function
    let Data = int

    //Direction for shift in instruction
    type ShiftDirection = 
        | Left of int
        | RightL of int
        | RightA of int
        | NoShift

    /// Register ID D.U
    type RegisterID =
        | R0  | R1  | R2  | R3  | R4
        | R5  | R6  | R7  | R8  | R9
        | R10 | R11 | R12 | R13 | R14
        | R15

    /// Registers Type Abbreviation
    type Registers = Map<RegisterID, Data>

    /// Status Flags ID D.U
    type FlagID =
        | N // Negative Flag
        | Z // Zero Flag
        | C // Carry Flag
        | V // Stack Overflow Flag

    /// Flags Type Abbreviation
    type Flags = Map<FlagID, bool>

    /// Operand D.U Type
    type Input =
        | ID of RegisterID // Pass Register ID for data access
        | Literal of Data // Pass literal

    type Operand = 
        | Operand of Input*ShiftDirection

    /// Instruction Keyword type (please update when new instructions are added whenever possible!)
    type InstructionKeyword =
        | ADD | ADC
        | MOV | MVN
        | ORR | AND
        | EOR | BIC
        | SUB | SBC
        | RSB | RSC
        | CMP | CMN
        | TST | TEQ
        | LSL | LSR
        | ASR

    /// Conditional code types (for reading flags)
    type ConditionCode = 
        | EQ | NE | CS | HS | CC | LO | MI | PL
        | VS | VC | HI | LS | GE | LT | GT | LE
        | AL

    /// Token Type
    type Token =
        | TokInstr of InstructionKeyword
        | TokS
        | TokCond of ConditionCode
        | TokLabel of string
        //| TokReg of RegisterID
        //| TokConst of int
        | TokOperand of Input
        | TokComma
        | TokExclam
        | TokSquareLeft
        | TokSquareRight
        | TokCurlyLeft
        | TokCurlyRight
        | TokNewLine
        | TokError of string

    type Operation = 
        | SevenOp of Token*Token*Token*Operand*bool*bool*Operand
        | SixOp of Token*Token*Token*Operand*bool*bool
        | FourOp of Token*Token*Token*bool
        | FourOp2 of Token*Token*bool*Operand
        | FourOp3 of Token*Operand*bool*Operand



    ///different parameters based on instruction functions, please add more if required! (last update: 06/03/17 23:32). Used in Parser and AST.
    type Parameters =
        | Param_Rd_Op_Bool of (RegisterID * Operand * bool)                                //(regD, op2, setFlags)
        | Param_Rd_Rn_Op_Bool of (RegisterID * RegisterID * Operand * bool)                //(regD, regN, op2, setFlags)
        | Param_Rd_Input_Int_Bool of (RegisterID * Input * int * bool)                      //(regD, regN, op2, shift, setFlags)
        | Param_Rd_Op of (RegisterID * Operand)                                            //(regD, op2)

    ///type representing the memory location (an int value in bytes) of the instruction or data (incr. addr by 4 bytes for each instruction parsed).
    type Address = int                  //Maybe replace int with MemoryLocation when Memory is done.

    ///type used for specifying a conditional code of the form, FlagID = bool. Usage: Z = 1 -> (Z, true); V = 0 -> (V, false)
    type Condition = FlagID * bool

    ///type representing the possible nodes in the AST
    type Node = InstructionKeyword * Parameters * (Condition option) * Address

    ///type representing the mapping of labels to memory addresses
    type LabelMap = Map<string, Address>

    ///type representing the AST (just a list of nodes as well as the map for the label mappings)
    type AST = (Node list) * LabelMap