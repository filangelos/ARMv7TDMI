namespace ARM7TDMI 

(* 
    High Level Programming @ Imperial College London # Spring 2017
    Project: A user-friendly ARM7TDMI assembler and simulator in F# and Web Technologies ( Github Electron & Fable Compliler )

    Contributors: Angelos Filos, Baron Khan, Youssef Rizk, Pranav Prabhu

    Module: Common
    Description: Common types definitions, top-level interface definitions between modules.
*)

[<AutoOpen>]
module Common =

    /// Raw data Type
    type Data = int
    /// Cast Function
    let Data = int

    //Direction for shift in instruction
    type ShiftDirection = 
        | Left of int
        | RightL of int
        | RightA of int
        | ROR of int
        | RRX
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

    /// Representing op2 since it can be a register and a literal
    type Input =
        | ID of RegisterID // Pass Register ID for data access
        | Literal of Data // Pass literal

    // Encapsulates shift direction into op2
    type Operand = 
        | Operand of Input*ShiftDirection

    //Memory address offsetting
    type Offset = 
        | TempOffset of Input // LDR     R8, [R10, #4] 
        | PreIndex of Input // LDR     R8, [R10, #4]!
        | PostIndex of Input // LDR     R8, [R10], #4
        | NoOffset // syntax: LDR     R8, [R10] 

    //Addressing modes for LDM and STM instructions (both normal and stack-oriented conventions)
    type AddressMode = 
        | IA | IB | DA | DB 
        | ED | FD | EA | FA

    type StackDirection = AddressMode

    //encapsulates address resgister and offset
    type AddressRegister = 
        {register: RegisterID; offset: Offset}

    //represents memory-related expressions
    type Expression = 
        | Lab of string
        | Number of int


    /// Conditional code types (for reading flags)
    type ConditionCode = 
        | EQ | NE | CS | HS | CC | LO | MI | PL
        | VS | VC | HI | LS | GE | LT | GT | LE
        | AL | NV

    /// Instruction Types

    ///dest, op1 {, SHIFT_op #expression}
    type InstrType1 = 
        | MOV | MVN

    ///dest, expression
    type InstrType2 = 
        | ADR

    ///dest, op1, op2 {, SHIFT_op #expression}
    type InstrType3 = 
        | ADD | ADC | SUB | SBC | RSB | RSC | AND
        | EOR | BIC | ORR
    
    ///dest, op1, op2
    type InstrType4 = 
        | LSL | LSR | ASR | ROR_

    ///op1, op2
    type InstrType5 = 
        | RRX_

    ///op1, op2 {, SHIFT_op #expression}
    type InstrType6 =
        | CMP | CMN | TST | TEQ

    ///dest, [source/dest {, OFFSET}]
    type InstrType7 = 
        | LDR | STR

    ///source/dest, {list of registers}
    type InstrType8 = 
        | LDM | STM

    type InstrType9 = 
        | B_ | BL

    type InstrDCD = | DCD

    type InstrEQU = | EQU

    type InstrFILL = | FILL

    type InstrEND = | END

    type SType = | S
    type BType = | B

    /// Token Types for Parser
    type Token =
        | TokInstr1 of InstrType1 
        | TokInstr2 of InstrType2
        | TokInstr3 of InstrType3
        | TokInstr4 of InstrType4
        | TokInstr5 of InstrType5
        | TokInstr6 of InstrType6
        | TokInstr7 of InstrType7
        | TokInstr8 of InstrType8
        | TokInstr9 of InstrType9
        | TokDCD of InstrDCD
        | TokEQU of InstrEQU
        | TokFILL of InstrFILL
        | TokEND of InstrEND
        | TokS of SType
        | TokB of BType
        | TokCond of ConditionCode
        | TokStackDir of StackDirection
        | TokLabel of string
        | TokReg of RegisterID
        | TokLiteral of int         //#5, #0xB, etc.
        | TokLiteralNoHash of int   //5, 10, 
        | TokComma
        | TokExclam
        | TokSquareLeft
        | TokSquareRight
        | TokCurlyLeft
        | TokCurlyRight
        | TokDash
        | TokEquals
        | TokNewLine
        | TokError of string
        | TokEOF

    type Instr =
    |  JInstr1 of ((((InstrType1*Option<SType>)*Option<ConditionCode>)*RegisterID)*Operand)
    |  JInstr2 of (((InstrType2*Option<ConditionCode>)*RegisterID)*Expression) 
    |  JInstr3 of (((((InstrType3*Option<SType>)*Option<ConditionCode>)*RegisterID)*RegisterID)*Operand)
    |  JInstr4 of (((((InstrType4*Option<SType>)*Option<ConditionCode>)*RegisterID)*RegisterID)*Input)
    |  JInstr5 of ((((InstrType5* Option<SType>)*Option<ConditionCode>)*RegisterID)*Input)
    |  JInstr6 of (((InstrType6*Option<ConditionCode>)*RegisterID)*Operand)
    |  JInstr7 of ((((InstrType7*Option<BType>)*Option<ConditionCode>)*RegisterID)*AddressRegister)
    |  JInstr8 of (((((InstrType8*StackDirection)*Option<ConditionCode>)*RegisterID)*bool)*(RegisterID list))   //bool: true if ! is next to reg
    |  JInstr9 of ((InstrType9*Option<ConditionCode>)*string)
    |  JInstrDCD of ((string*InstrDCD)* int list)
    |  JInstrEQU of ((string*InstrEQU) *int)
    |  JInstrFILL of(InstrFILL*int)   //int must be multiple of 4
    |  JInstrEND of (InstrEND*Option<SType>)
    |  JLabel of string
    |  JError of string
    |  JInstrEOF

    (*
    Tokenisation (please remove once done):

    JInstr2 (has changed):
    In ADR, change TokLiteral to TokLiteralNoHash

    
    JInstrDCD:
    TokLabel TokDCD VAL_LIST
    
    VAL_LIST: VAL_LIST TokComma TokLiteralNoHash
            | TokLiteralNoHash


    JInstrEQU:
    TokLabel TokEQU TokLiteralNoHash

    JInstrFILL:
    Option(TokLabel) TokFILL TokLiteralNoHash

    JInstrEND:
    TokEnd Option(TokS)
    
    *)


    ///type representing the memory location (an int value in bytes) of the instruction or data (incr. addr by 4 bytes for each instruction parsed).
    type Address = int                  

    ///type representing the possible nodes in the AST
    type Node = Instr * Address

    ///type representing the mapping of labels to memory addresses
    type LabelMap = Map<string, Address>

    ///type representing the AST (just a list of nodes as well as the map for the label mappings)
    type AST = (Node list)