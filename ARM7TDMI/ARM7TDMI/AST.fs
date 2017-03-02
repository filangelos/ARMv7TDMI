namespace ARM7TDMI

module AST =

    open MachineState

    ///different parameters based on instruction functions
    type Parameters =
        | Parameters6 of (RegisterID * RegisterID * Operand * MachineState * bool * bool)   //(regD, regN, op2, state, includeCarry, setFlags)
        | Parameters5 of (RegisterID * RegisterID * Operand * MachineState * bool)          //(regD, regN, op2, state, setFlags)
        | Parameters4 of (RegisterID * Operand * MachineState * bool)                       //(regD, op2, state, setFlags)

    ///type representing the functions in the Instruction module. Set this value to the functions themselves (just the name, no parameters attached)
    type InstructionFunc = Parameters -> MachineState

    ///type representing the memory location (an int value in bytes) of the instruction or data (incr. addr by 4 bytes for each instruction parsed).
    type Address = int                  //Replace int with MemoryLocation when Memory is done.

    ///type representing the possible nodes in the AST
    type Node =
        | Instruction of InstructionFunc * Parameters * Address
        | Label of string * Address

    ///type representing the mapping of labels to memory addresses
    type LabelMap = Map<string, Address>

    ///type representing the AST (just a list of nodes as well as the map for the label mappings)
    type AST = Node list * LabelMap

    ////Build the AST in the parser using these functions////

    ///adds an instruction node to the AST, address should be an int and assigned in the parser (increase addr by 4 bytes for each instr. parsed).
    ///usage: addInstructionNode ast addWithCarryS Parameters4(R1, R2, op2, MachineState, false, true) [-current address int value-]
    let addInstructionNode ((ast,labelMap):AST) (f:InstructionFunc) (p:Parameters) (addr:Address) =
        (ast @ [Instruction (f, p, addr)], labelMap)

    ///adds a label node to the AST, address should be an int and assigned in the parser (increase addr by 4 bytes for each instr. parsed).
    ///usage: addLabelNode ast LABEL1 [-current address int value-]
    let addLabelNode ((ast,labelMap):AST) (name:string) (addr:Address) =
        (ast @ [Label (name, addr)], labelMap.Add (name, addr))



    (*--------------------------------------------------------TESTING--------------------------------------------------------*)
