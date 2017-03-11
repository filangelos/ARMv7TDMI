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
    let private ( ^. ) = Optics.get MachineState.Register_
    let private ( ^= ) = Optics.set MachineState.Register_
    let private ( ^* ) = Optics.get MachineState.Flag_
    let private ( ^- ) = Optics.set MachineState.Flag_

    ////Build the AST in the parser using these functions////

    ///adds an instruction node to the AST, address should be an int and assigned in the parser (increase addr by 4 bytes for each instr. and label parsed).
    ///usage: addInstruction myAst1 (MOV) (Parameters1RegShift(R1, Literal(13), false, NoShift)) (None) [-current address int value-]
    let addInstruction (ast:AST) (f:InstructionKeyword) (p:Parameters) (c:Condition option) (addr:Address) =
        match ast with
        | lst, labelMap -> (lst @ [(f, p, c, addr)], labelMap)

    ///adds a label node to the AST, address should be an int and assigned in the parser (increase addr by 4 bytes for each instr. parsed).
    ///usage: addLabelNode ast LABEL1 [-current address int value-]
    let addLabel (ast:AST) (name:string) (addr:Address) =
        match ast with
        | lst, labelMap -> (lst, labelMap.Add (name, addr))

    ///evaluates a condtion of type Condition and returns true or false
    let private evaluateCondition (cond:Condition option) (state:MachineState) =
        match cond with
        | Some(fID, eq) ->
            ( ^* ) fID state = eq
        | None -> true

    ///executes instructions in an AST and returns the final MachineState (need to add all instructions)
    let rec reduce (ast:AST) (state:MachineState) (pc:int) (maxPC:int) =
        if pc <= maxPC then
            try 
                match ast with
                | (nodes, labels) ->
                    let currentNode = List.find (fun x -> match x with | (_,_,_,addr) -> addr = pc) nodes
                    printfn "executing node at pc=%A" pc
                    match currentNode with
                    | (f, p, cond, addr) -> 
                        if evaluateCondition cond state then
                            match f, p with
                            | MOV, Parameters1Reg((regD, op2, setFlags)) ->
                                let newState = Instructions.mov (regD, op2, state, setFlags)
                                reduce ast newState (pc+1) maxPC
                            | _ -> failwithf "Could not execute node: %A" currentNode
                        else
                            reduce ast state (pc+1) maxPC
            with
                | _ -> reduce ast state (pc+1) maxPC
        else
            state

    (*--------------------------------------------------------TESTING--------------------------------------------------------*)

    let testAST () =
        printfn "Running testAST:"
        let emptyState = MachineState.make()
        printfn "empty machine state:\n%A\n" emptyState
        let myAst1 = ([], Map.empty<string, Address>)
        let  myAst2 = addInstruction myAst1 (MOV) (Parameters1Reg(R1, Operand(Literal(13),NoShift), false)) (None) 2
        let  myAst3 = addInstruction myAst2 (MOV) (Parameters1Reg(R2, Operand(Literal(0),NoShift), true)) (None) 4
        let  myAst4 = addInstruction myAst3 (MOV) (Parameters1Reg(R3, Operand(Literal(342),NoShift), false)) (Some(Z, false)) 6    //shouldn't execute this
        let  myAst5 = addInstruction myAst4 (MOV) (Parameters1Reg(R4, Operand(Literal(999), Left(1)), false)) (Some(Z, true)) 8
        printfn "ast is:\n%A\n" myAst5
        printfn "Reducing AST..."
        let resultState2 = reduce myAst5 emptyState 0 10
        printfn "final result state for this ast is:\n%A\n" resultState2



        printfn "Finished testAST.\n"

        

    (*------------------------------------------------------------------------------------------------------------------------*)
    //IGNORE ANYTHING BELOW HERE
    (*

    let parse (lst:Token list) =
        let tokenLst = 







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
