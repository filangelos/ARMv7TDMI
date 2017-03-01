namespace ARM7TDMI

module AST =

    open MachineState

    type Parameters = (RegisterID * RegisterID * Operand * MachineState * bool * bool)

    type InstructionFunc = Parameters -> MachineState

    type Address = int  //replace with MemoryLocation when Memory is done

    type Node =
        | Instruction of InstructionFunc * Parameters * Address
        | Label of string * Address

    type AST = Node list

    //NB: build the AST in the parser

    