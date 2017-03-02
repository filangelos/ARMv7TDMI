namespace ARM7TDMI

module AST =

    open MachineState

    //different paramters based on instruction functions
    type Parameters =
        | Parameters6 of (RegisterID * RegisterID * Operand * MachineState * bool * bool)   //(regD, regN, op2, state, includeCarry, setFlags)
        | Parameters5 of (RegisterID * RegisterID * Operand * MachineState * bool)          //(regD, regN, op2, state, setFlags)
        | Parameters4 of (RegisterID * Operand * MachineState * bool)                       //(regD, op2, state, setFlags)

    type InstructionFunc = Parameters -> MachineState

    type Address = int  //replace with MemoryLocation when Memory is done

    type Node =
        | Instruction of InstructionFunc * Parameters * Address
        | Label of string * Address

    type AST = Node list

    //NB: build the AST in the parser

    