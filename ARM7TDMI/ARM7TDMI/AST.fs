namespace ARM7TDMI

(* 
    High Level Programming @ Imperial College London # Spring 2017
    Project: A user-friendly ARM7TDMI assembler and simulator in F# and Web Technologies ( Github Electron & Fable Compliler )

    Contributors: Baron Khan

    Module: AST
    Description: Functions and type definitions for building and reducing an AST.
*)

module AST =

    open MachineState
    open Instructions

    // shortcuts
    let ( ^. ) = Optics.get MachineState.Register_
    let ( ^= ) = Optics.set MachineState.Register_
    let ( ^* ) = Optics.get MachineState.Flag_
    let ( ^- ) = Optics.set MachineState.Flag_

    ///different parameters based on instruction functions
    type Parameters =
        | ParametersAdd of (RegisterID * RegisterID * Operand * bool * bool * ShiftDirection)            //(regD, regN, op2, includeCarry, setFlags, ShiftDirection)
        | ParametersSub of (RegisterID * RegisterID * Operand * bool * bool * bool * ShiftDirection)     //(regD, regN, op2, includeCarry, reverse, setFlags, ShiftDirection)
        | Parameters1RegShift of (RegisterID * Operand * bool * ShiftDirection)                          //(regD, op2, setFlags, ShiftDirection)
        | Parameters1Reg of (RegisterID * Operand * bool)                                                //(regD, op2, setFlags)
        | Parameters2Reg of (RegisterID * RegisterID * Operand * bool)                                   //(regD, regN, op2, setFlags)

    ///type representing the memory location (an int value in bytes) of the instruction or data (incr. addr by 4 bytes for each instruction parsed).
    type Address = int                  //Replace int with MemoryLocation when Memory is done.

    ///type used for specifying a conditional code. Usage: Z = 1 -> (Z, true); V = 0 -> (V, false)
    type Condition = FlagID * bool

    ///type representing the possible nodes in the AST
    type InstructionNode = InstructionKeyword * Parameters * (Condition option) * Address

    ///type representing the mapping of labels to memory addresses
    type LabelMap = Map<string, Address>

    ///type representing the AST (just a list of nodes as well as the map for the label mappings)
    type AST = (InstructionNode list) * LabelMap

    ////Build the AST in the parser using these functions////

    ///adds an instruction node to the AST, address should be an int and assigned in the parser (increase addr by 4 bytes for each instr. parsed).
    ///usage: addInstructionNode ast addWithCarryS Parameters4(R1, R2, op2, MachineState, false, true) [-current address int value-]
    let inline addInstructionNode (ast:AST) (f:InstructionKeyword) (p:Parameters) (c:Condition option) (addr:Address) =
        match ast with
        | lst, labelMap -> (lst @ [(f, p, c, addr)], labelMap)

    ///adds a label node to the AST, address should be an int and assigned in the parser (increase addr by 4 bytes for each instr. parsed).
    ///usage: addLabelNode ast LABEL1 [-current address int value-]
    let inline addLabelNode (ast:AST) (name:string) (addr:Address) =
        match ast with
        | lst, labelMap -> (lst, labelMap.Add (name, addr))

    ///evaluates a condtion of type Condition and returns true or false
    let evaluateCondition (cond:Condition option) (state:MachineState) =
        match cond with
        | Some(fID, eq) ->
            ( ^* ) fID state = eq
        | None -> true
        | _ -> failwithf "invalid condition"

    let rec reduce (ast:AST) (state:MachineState) =
        match ast with
        | (f, p, cond, addr)::t, lst ->
            if evaluateCondition cond state then
                match f, p with
                | MOV, Parameters1RegShift((regD, op2, setFlags, shiftDirection)) ->
                    let newState = Instructions.mov (regD, op2, state, setFlags, shiftDirection)
                    reduce (t,lst) newState
                | _ -> failwithf "Could not execute instruction"
            else
                reduce (t,lst) state
        | [], lst -> state
        | _ -> failwithf "Not a valid node"

    (*--------------------------------------------------------TESTING--------------------------------------------------------*)

    let testAST =
        printfn "Running testAST:"
        let emptyState = MachineState.make()
        printfn "empty machine state: %A" emptyState
        let myAst1 = ([], Map.empty<string, Address>)
        let  myAst2 = addInstructionNode myAst1 (MOV) (Parameters1RegShift(R1, Literal(0), true, NoShift)) (None) 2
        let  myAst3 = addInstructionNode myAst2 (MOV) (Parameters1RegShift(R2, Literal(342), false, NoShift)) (Some(Z, true)) 4
        printfn "ast is: %A" myAst3
        let resultState2 = reduce myAst3 emptyState
        printfn "new test state is %A" resultState2
        printfn "Finished testAST.\n"

    (*------------------------------------------------------------------------------------------------------------------------*)
    //IGNORE BELOW: if there's anything here, it should be removed
    (*









    *)
    (*------------------------------------------------------------------------------------------------------------------------*)

    (* OLD AST - ONLY WORKS WITH ONE TYPE OF INSTRUCTION. This method was much more efficient so maybe come back to this method later. *)
    (*
    ///different parameters based on instruction functions
    type Parameters =
        | ParametersAdd of (RegisterID * RegisterID * Operand * MachineState * bool * bool * ShiftDirection)            //(regD, regN, op2, state, includeCarry, setFlags, ShiftDirection)
        | ParametersSub of (RegisterID * RegisterID * Operand * MachineState * bool * bool * bool * ShiftDirection)     //(regD, regN, op2, state, includeCarry, reverse, setFlags, ShiftDirection)
        | Parameters1RegShift of (RegisterID * Operand * MachineState * bool * ShiftDirection)                          //(regD, op2, state, setFlags, ShiftDirection)
        | Parameters1Reg of (RegisterID * Operand * MachineState * bool)                                                //(regD, op2, state, setFlags)
        | Parameters2Reg of (RegisterID * RegisterID * Operand * MachineState * bool)                                   //(regD, regN, op2, state, setFlags)


    ///type representing the functions in the Instruction module. Set this value to the functions themselves (just the name, no parameters attached)
    type InstructionFunc<'a> = 'a -> MachineState

    ///type representing the memory location (an int value in bytes) of the instruction or data (incr. addr by 4 bytes for each instruction parsed).
    type Address = int                  //Replace int with MemoryLocation when Memory is done.

    ///type representing the possible nodes in the AST
    type Node<'a> =
        | InstructionNode of (InstructionFunc<'a>) * 'a * Address
        | LabelNode of string * Address
        | NullNode

    ///type representing the mapping of labels to memory addresses
    type LabelMap = Map<string, Address>

    ///type representing the AST (just a list of nodes as well as the map for the label mappings)
    type AST<'a> = (Node<'a> list) * LabelMap

    ////Build the AST in the parser using these functions////

    ///adds an instruction node to the AST, address should be an int and assigned in the parser (increase addr by 4 bytes for each instr. parsed).
    ///usage: addInstructionNode ast addWithCarryS Parameters4(R1, R2, op2, MachineState, false, true) [-current address int value-]
    let inline addInstructionNode (ast:AST<'a>) (f) (p) (addr:Address) =
        match ast with
        | lst, labelMap -> (lst @ [InstructionNode(f, p, addr)], labelMap)

    ///adds a label node to the AST, address should be an int and assigned in the parser (increase addr by 4 bytes for each instr. parsed).
    ///usage: addLabelNode ast LABEL1 [-current address int value-]
    let inline addLabelNode (ast:AST<'a>) (name:string) (addr:Address) =
        match ast with
        | lst, labelMap -> (lst, labelMap.Add (name, addr))


    let rec reduce (ast:AST<'a>) (state:MachineState) =
        match ast with
        | InstructionNode(f, p, addr)::t, lst ->
            let newState = f p
            reduce (t,lst) newState
        | NullNode::t, lst -> reduce (t, lst) state
        | [], lst -> state
        | _ -> failwithf "Not a valid node"

    (*--------------------------------------------------------TESTING--------------------------------------------------------*)
    let testAST =
        printfn "Running testAST:"
        let testState = MachineState.make()
        printfn "empty machine state: %A" testState
        let ast = ([], Map.empty<string, Address>)
        let  myAst = addInstructionNode ast (mov) (R1, Literal(13), testState, false, NoShift) 4
        //printfn "ast is: %A" myAst
        let result = reduce myAst testState
        //printfn "new test state is %A" result
        let  myAst2 = addInstructionNode myAst (mov) (R2, Literal(342), result, false, NoShift) 4
        printfn "ast is: %A" myAst2
        let result2 = reduce myAst2 testState
        printfn "new test state is %A" result2
        printfn "Finished testAST.\n"

    *)
