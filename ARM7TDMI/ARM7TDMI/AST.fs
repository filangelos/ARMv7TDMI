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
    open InstructionsInterfaces

    // shortcuts
    let private ( ^. ) = Optics.get MachineState.Register_
    let private ( ^= ) = Optics.set MachineState.Register_
    let private ( ^* ) = Optics.get MachineState.Flag_
    let private ( ^- ) = Optics.set MachineState.Flag_
    let private ( ^% ) = Optics.get MachineState.AST_

    ///adds an instruction node to the AST, address should be an int and assigned in the parser (increase addr by 4 bytes for each instr. and label parsed).
    let private addInstruction (ast:AST) (f:Instruction) (p:Parameters) (addr:Address) =
        ast @ [(f, p, addr)]

    ///adds a label node to the AST, address should be an int and assigned in the parser (increase addr by 4 bytes for each instr. parsed).
    let private addLabel (labelMap: LabelMap) (name:string) (addr:Address) =
        labelMap.Add (name, addr)

    ///Build the AST and label map from the list given by Parser
    let buildAST (parseLst:((Instruction * Parameters) list)) =
        let rec addNode (lst:((Instruction * Parameters) list)) (ast:AST) (labelMap: LabelMap) (pc:int) =
            match lst with
            | [] -> ast, labelMap
            | node::t ->
                match node with
                | (Label(s), NoParam) -> addNode t ast (addLabel labelMap s pc) (pc+4)
                | (instr, p) -> addNode t (addInstruction ast instr p pc) labelMap (pc+4)
        addNode parseLst [] Map.empty<string, Address> 0
    
    let InstructionMap = dict[
                                Instr1(MOV), mov_;
                                Instr1(MVN), mvn_;
                             ]

    ///executes instructions in an AST and returns the final MachineState (need to add all instructions)
    let step (state:MachineState) =
        // try to find a node with address matching pc, if not found, catch exception from List.find
        let pc = ( ^. ) R15 state
        try 
            let currentNode = List.find (fun x -> match x with | (_,_,addr) -> addr = pc) (( ^% ) state)
            printfn "executing node at pc=%A" pc
            match currentNode with
            | (f, p, addr) -> 
                match f, p with
                    
                //| ADD, Param_Rd_Rn_Op_Bool((regD, regN, op2, setFlags)) ->
                //    reduce (Instructions.add_ (regD, regN, op2, state, setFlags)) (pc+1) maxPC
                //| ADC, Param_Rd_Rn_Op_Bool((regD, regN, op2, setFlags)) ->
                //    reduce (Instructions.adc_ (regD, regN, op2, state, setFlags)) (pc+1) maxPC
                | Instr1(MOV), Param_Rd_Op_Bool_Cond((regD, op2, setFlags, cond)) ->
                    InstructionsInterfaces.mov_ (regD, op2, state, setFlags, cond)
                | Instr1(MVN), Param_Rd_Op_Bool_Cond((regD, op2, setFlags, cond)) ->
                    InstructionsInterfaces.mvn_ (regD, op2, state, setFlags, cond)
                (*
                | ORR, Param_Rd_Rn_Op_Bool((regD, regN, op2, setFlags)) ->
                    reduce (Instructions.orr (regD, regN, op2, state, setFlags)) (pc+1) maxPC
                | AND, Param_Rd_Rn_Op_Bool((regD, regN, op2, setFlags)) ->
                    reduce (Instructions.andOp (regD, regN, op2, state, setFlags)) (pc+1) maxPC
                | EOR, Param_Rd_Rn_Op_Bool((regD, regN, op2, setFlags)) ->
                    reduce (Instructions.eOR (regD, regN, op2, state, setFlags)) (pc+1) maxPC
                | BIC, Param_Rd_Rn_Op_Bool((regD, regN, op2, setFlags)) ->
                    reduce (Instructions.bic (regD, regN, op2, state, setFlags)) (pc+1) maxPC
                | LSL, Param_Rd_Op_Bool((regD, op2, setFlags))
                | LSR, Param_Rd_Op_Bool((regD, op2, setFlags)) ->
                    reduce (Instructions.mov (regD, op2, state, setFlags)) (pc+1) maxPC
                | ASR, Param_Rd_Input_Int_Bool((regD, input, shift, setFlags)) ->
                    reduce (Instructions.arithmeticRightShift (regD, input, shift, state, setFlags)) (pc+1) (maxPC)
                | SUB, Param_Rd_Rn_Op_Bool((regD, regN, op2, setFlags)) ->
                    reduce (Instructions.sub_ (regD, regN, op2, state, setFlags)) (pc+1) maxPC
                | SBC, Param_Rd_Rn_Op_Bool((regD, regN, op2, setFlags)) ->
                    reduce (Instructions.sbc_ (regD, regN, op2, state, setFlags)) (pc+1) maxPC
                | RSB, Param_Rd_Rn_Op_Bool((regD, regN, op2, setFlags)) ->
                    reduce (Instructions.rsb_ (regD, regN, op2, state, setFlags)) (pc+1) maxPC
                | RSC, Param_Rd_Rn_Op_Bool((regD, regN, op2, setFlags)) ->
                    reduce (Instructions.rsc_ (regD, regN, op2, state, setFlags)) (pc+1) maxPC
                | CMP, Param_Rd_Op((regD, op2)) ->
                    reduce (Instructions.cmp_ (regD, op2, state)) (pc+1) maxPC
                | CMN, Param_Rd_Op((regD, op2)) ->
                    reduce (Instructions.cmn_ (regD, op2, state)) (pc+1) maxPC
                | TST, Param_Rd_Op((regD, op2)) ->
                    reduce (Instructions.tst_ (regD, op2, state)) (pc+1) maxPC
                | TEQ, Param_Rd_Op((regD, op2)) ->
                    reduce (Instructions.teq_ (regD, op2, state)) (pc+1) maxPC
                *)
                | _ -> failwithf "Could not execute node: %A" currentNode
                    
        with
            | _ -> state

    let rec reduce (state:MachineState) (pc:int) (maxPC) =
        if pc <= maxPC then
            let updateStateR15 = ( ^= ) R15 pc state
            let newState = step updateStateR15
            reduce (newState) (pc+1) maxPC
        else
            state

    (*--------------------------------------------------------TESTING--------------------------------------------------------*)

//    let testAST () =
        (*
        printfn "Running testAST:"
        let myAst1 = ([], Map.empty<string, Address>)
        let  myAst2 = addInstruction myAst1 (Instr1(MOV)) (Param_Rd_Op_Bool(R1, Operand(Literal(13),NoShift), false, None)) 2
        let  myAst3 = addInstruction myAst2 (MOV) (Param_Rd_Op_Bool(R2, Operand(Literal(0),NoShift), true, None)) 4
        let  myAst4 = addInstruction myAst3 (MOV) (Param_Rd_Op_Bool(R3, Operand(Literal(342),NoShift), false, Some(NE))) 6    //shouldn't execute this
        let  myAst5 = addInstruction myAst4 (MOV) (Param_Rd_Op_Bool(R4, Operand(Literal(999), Left(1)), false, Some(EQ))) 8
        printfn "ast is:\n%A\n" myAst5
        let stateWithAst = MachineState.init(myAst5)
        printfn "Reducing AST..."
        let resultState2 = reduce stateWithAst 0 10
        printfn "final result state for this ast is:\n%A\n" resultState2
        *)
        (*
        printfn "Running testAST:"
        let parseList = [
                            (Label("START"), NoParam);
                            (Instr1(MOV), Param_Rd_Op_Bool_Cond(R1, Operand(Literal(13),NoShift), false, None));
                            (Instr1(MOV), Param_Rd_Op_Bool_Cond(R2, Operand(Literal(0),NoShift), true, None));
                            (Label("HALFWAY"), NoParam);
                            (Instr1(MOV), Param_Rd_Op_Bool_Cond(R3, Operand(Literal(342),NoShift), false, Some(NE)));
                            (Instr1(MOV), Param_Rd_Op_Bool_Cond(R4, Operand(Literal(999), Left(1)), false, Some(EQ)))
                        ]

        let ast = buildAST parseList
        printfn "ast is:\n%A\n" ast
        let state = MachineState.init(ast)
        let result = reduce state 0 20
        printfn "final result state for this ast is:\n%A\n" result
        printfn "Finished testAST.\n"
        *)
        

    (*------------------------------------------------------------------------------------------------------------------------*)
    //IGNORE ANYTHING BELOW HERE
    (*

    Instruction = T







    *)

    (*------------------------------------------------------------------------------------------------------------------------*)

        (*
    let rec reduce (ast:AST) (state:MachineState) (pc:int) (maxPC:int) =
        if pc <= maxPC then
            // try to find a node with address matching pc, if not found, catch exception from List.find
            try 
                match ast with
                | (nodes, labels) ->
                    let currentNode = List.find (fun x -> match x with | (_,_,_,addr) -> addr = pc) nodes
                    printfn "executing node at pc=%A" pc
                    match currentNode with
                    | (f, p, cond, addr) -> 
                        //evaluate conditional code
                        if evaluateCondition cond state then
                            match f, p with
                            | ADD, Param_Rd_Rn_Op_Bool((regD, regN, op2, setFlags)) ->
                                reduce ast (Instructions.add_ (regD, regN, op2, state, setFlags)) (pc+1) maxPC
                            | ADC, Param_Rd_Rn_Op_Bool((regD, regN, op2, setFlags)) ->
                                reduce ast (Instructions.adc_ (regD, regN, op2, state, setFlags)) (pc+1) maxPC
                            | MOV, Param_Rd_Op_Bool((regD, op2, setFlags)) ->
                                reduce ast (Instructions.mov (regD, op2, state, setFlags)) (pc+1) maxPC
                            | MVN, Param_Rd_Op_Bool((regD, op2, setFlags)) ->
                                reduce ast (Instructions.mvn (regD, op2, state, setFlags)) (pc+1) maxPC
                            | ORR, Param_Rd_Rn_Op_Bool((regD, regN, op2, setFlags)) ->
                                reduce ast (Instructions.orr (regD, regN, op2, state, setFlags)) (pc+1) maxPC
                            | AND, Param_Rd_Rn_Op_Bool((regD, regN, op2, setFlags)) ->
                                reduce ast (Instructions.andOp (regD, regN, op2, state, setFlags)) (pc+1) maxPC
                            | EOR, Param_Rd_Rn_Op_Bool((regD, regN, op2, setFlags)) ->
                                reduce ast (Instructions.eOR (regD, regN, op2, state, setFlags)) (pc+1) maxPC
                            | BIC, Param_Rd_Rn_Op_Bool((regD, regN, op2, setFlags)) ->
                                reduce ast (Instructions.bic (regD, regN, op2, state, setFlags)) (pc+1) maxPC
                            | LSL, Param_Rd_Op_Bool((regD, op2, setFlags))
                            | LSR, Param_Rd_Op_Bool((regD, op2, setFlags)) ->
                                reduce ast (Instructions.mov (regD, op2, state, setFlags)) (pc+1) maxPC
                            | ASR, Param_Rd_Input_Int_Bool((regD, input, shift, setFlags)) ->
                                reduce ast (Instructions.arithmeticRightShift (regD, input, shift, state, setFlags)) (pc+1) (maxPC)
                            | SUB, Param_Rd_Rn_Op_Bool((regD, regN, op2, setFlags)) ->
                                reduce ast (Instructions.sub_ (regD, regN, op2, state, setFlags)) (pc+1) maxPC
                            | SBC, Param_Rd_Rn_Op_Bool((regD, regN, op2, setFlags)) ->
                                reduce ast (Instructions.sbc_ (regD, regN, op2, state, setFlags)) (pc+1) maxPC
                            | RSB, Param_Rd_Rn_Op_Bool((regD, regN, op2, setFlags)) ->
                                reduce ast (Instructions.rsb_ (regD, regN, op2, state, setFlags)) (pc+1) maxPC
                            | RSC, Param_Rd_Rn_Op_Bool((regD, regN, op2, setFlags)) ->
                                failwithf "Please add missing implementation for node, %A" currentNode
                            | CMP, Param_Rd_Op((regD, op2)) ->
                                reduce ast (Instructions.cmp_ (regD, op2, state)) (pc+1) maxPC
                            | CMN, Param_Rd_Op((regD, op2)) ->
                                reduce ast (Instructions.cmn_ (regD, op2, state)) (pc+1) maxPC
                            | TST, Param_Rd_Op((regD, op2)) ->
                                reduce ast (Instructions.tst_ (regD, op2, state)) (pc+1) maxPC
                            | TEQ, Param_Rd_Op((regD, op2)) ->
                                reduce ast (Instructions.teq_ (regD, op2, state)) (pc+1) maxPC
                            | _ -> failwithf "Could not execute node: %A" currentNode
                        else
                            reduce ast state (pc+1) maxPC
            with
                | _ -> reduce ast state (pc+1) maxPC
        else
            state
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
        | _ -> failwithf "Not a valid node" *)

    (*--------------------------------------------------------TESTING--------------------------------------------------------*)
    (*
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