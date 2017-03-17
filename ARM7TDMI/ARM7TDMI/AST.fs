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
    let private addInstruction (ast:AST) (f:Instr) (addr:Address) =
        ast @ [(f, addr)]

    ///adds a label node to the AST, address should be an int and assigned in the parser (increase addr by 4 bytes for each instr. parsed).
    let private addLabel (labelMap: LabelMap) (name:string) (addr:Address) =
        labelMap.Add (name, addr)

    ///Build the AST and label map from the list given by Parser
    let buildAST (parseLst:(Instr list)) =
        let rec addNode (lst:(Instr list)) (ast:AST) (labelMap: LabelMap) (pc:int) =
            match lst with
            | [] -> ast, labelMap
            | node::t ->
                match node with
                | (JLabel(s)) -> addNode t ast (addLabel labelMap s pc) (pc+4)
                | instr -> addNode t (addInstruction ast instr pc) labelMap (pc+4)
        addNode parseLst [] Map.empty<string, Address> 0

    ///executes instructions in an AST and returns the final MachineState (need to add all instructions)
    let step (state:MachineState) =
        // try to find a node with address matching pc, if not found, catch exception from List.find
        let pc = ( ^. ) R15 state
        try 
            let currentNode = List.find (fun x -> match x with | (_,addr) -> addr = pc) (( ^% ) state)
            printfn "executing node at pc=%A" pc
            match currentNode with
            | (f, addr) -> 
                match f with
                | JInstr1(MOV, setFlags, cond, rd, rn) ->
                    InstructionsInterfaces.mov_ (rd, rn, state, (if setFlags = None then false else true), cond)
                | JInstr1(MVN, setFlags, cond, rd, rn) ->
                    InstructionsInterfaces.mvn_ (rd, rn, state, (if setFlags = None then false else true), cond)
                | JInstr2(ADR, setFlags, cond, rd) ->
                    failwithf "ADR not done yet"
                | JInstr3(ADD, setFlags, cond, rd, rn, op2) ->
                    InstructionsInterfaces.add_ (rd, rn, op2, state, (if setFlags = None then false else true), cond)
                | JInstr3(ADC, setFlags, cond, rd, rn, op2) ->
                    InstructionsInterfaces.adc_ (rd, rn, op2, state, (if setFlags = None then false else true), cond)
                | JInstr3(SUB, setFlags, cond, rd, rn, op2) ->
                    InstructionsInterfaces.sub_ (rd, rn, op2, state, (if setFlags = None then false else true), cond)
                | JInstr3(SBC, setFlags, cond, rd, rn, op2) ->
                    InstructionsInterfaces.sbc_ (rd, rn, op2, state, (if setFlags = None then false else true), cond)
                | JInstr3(RSB, setFlags, cond, rd, rn, op2) ->
                    InstructionsInterfaces.rsb_ (rd, rn, op2, state, (if setFlags = None then false else true), cond)
                | JInstr3(RSC, setFlags, cond, rd, rn, op2) ->
                    InstructionsInterfaces.rsc_ (rd, rn, op2, state, (if setFlags = None then false else true), cond)
                | JInstr3(AND, setFlags, cond, rd, rn, op2) ->
                    InstructionsInterfaces.and_ (rd, rn, op2, state, (if setFlags = None then false else true), cond)
                | JInstr3(EOR, setFlags, cond, rd, rn, op2) ->
                    InstructionsInterfaces.eor_ (rd, rn, op2, state, (if setFlags = None then false else true), cond)
                | JInstr3(BIC, setFlags, cond, rd, rn, op2) ->
                    InstructionsInterfaces.bic_ (rd, rn, op2, state, (if setFlags = None then false else true), cond)
                | JInstr3(ORR, setFlags, cond, rd, rn, op2) ->
                    InstructionsInterfaces.orr_ (rd, rn, op2, state, (if setFlags = None then false else true), cond)
                | JInstr4(LSL, setFlags, cond, rd, rn, Literal(shift)) ->
                    InstructionsInterfaces.lsl_ (rd, Operand(ID(rn), ShiftDirection.Left(shift)), state, (if setFlags = None then false else true), cond) //need to test
                | JInstr4(LSL, setFlags, cond, rd, rn, ID(reg)) ->
                    InstructionsInterfaces.lsl_ (rd, Operand(ID(rn), ShiftDirection.Left(( ^. ) reg state)), state, (if setFlags = None then false else true), cond) //need to test
                | JInstr4(LSR, setFlags, cond, rd, rn, Literal(shift)) ->
                    InstructionsInterfaces.lsr_ (rd, Operand(ID(rn), ShiftDirection.RightL(shift)), state, (if setFlags = None then false else true), cond) //need to test
                | JInstr4(LSR, setFlags, cond, rd, rn, ID(reg)) ->
                    InstructionsInterfaces.lsr_ (rd, Operand(ID(rn), ShiftDirection.RightL(( ^. ) reg state)), state, (if setFlags = None then false else true), cond) //need to test
                | JInstr4(ASR, setFlags, cond, rd, rn, Literal(shift)) ->
                    InstructionsInterfaces.asr_ (rd, Operand(ID(rn), ShiftDirection.RightA(shift)), state, (if setFlags = None then false else true), cond) //need to test
                | JInstr4(ASR, setFlags, cond, rd, rn, ID(reg)) ->
                    InstructionsInterfaces.asr_ (rd, Operand(ID(rn), ShiftDirection.RightA(( ^. ) reg state)), state, (if setFlags = None then false else true), cond) //need to test
                | JInstr4(ROR_, setFlags, cond, rd, rn, Literal(shift)) ->
                    InstructionsInterfaces.ror_ (rd, Operand(ID(rn), ShiftDirection.ROR(shift)), state, (if setFlags = None then false else true), cond) //need to test
                | JInstr4(ROR_, setFlags, cond, rd, rn, ID(reg)) ->
                    InstructionsInterfaces.ror_ (rd, Operand(ID(rn), ShiftDirection.ROR(( ^. ) reg state)), state, (if setFlags = None then false else true), cond) //need to test
                | JInstr5(RRX_, setFlags, cond, rd, op2) ->
                    InstructionsInterfaces.rrx_ (rd, Operand(op2, ShiftDirection.RRX), state, (if setFlags = None then false else true), cond) // need to test
                | JInstr6(CMP, cond, rd, op2) ->
                    InstructionsInterfaces.cmp_ (rd, op2, state, cond)
                | JInstr6(CMN, cond, rd, op2) ->
                    InstructionsInterfaces.cmn_ (rd, op2, state, cond)
                | JInstr6(TST, cond, rd, op2) ->
                    InstructionsInterfaces.tst_ (rd, op2, state, cond)
                | JInstr6(TEQ, cond, rd, op2) ->
                    InstructionsInterfaces.teq_ (rd, op2, state, cond)
                | _ -> failwithf "Could not execute node in the ast: %A" currentNode
                    
        with
            | _ -> state

    ///reduces an ast in a MachineState by executing the nodes between pc and maxPC
    let rec reduce (state:MachineState) (pc:int) (maxPC) =
        if pc <= maxPC then
            let updateStateR15 = ( ^= ) R15 pc state
            let newState = step updateStateR15
            reduce (newState) (pc+1) maxPC
        else
            state

    ///execute an entire ast in a MachineState
    let execute (state:MachineState) = 
     //get address of last node
     let ast = ( ^% ) state
     let maxPC = snd (ast |> List.rev |> List.head)
     reduce state 0 maxPC

    (*--------------------------------------------------------TESTING--------------------------------------------------------*)

    let testAST () =
        printfn "Running testAST:"
        let parseList = [
                            (JLabel("START"));
                            (JInstr1(MOV, None, None, R1, Operand(Literal(1), NoShift)));
                            (JInstr1(MOV, Some(S), None, R2, Operand(Literal(0), NoShift)));
                            (JLabel("LABEL2"));
                            (JInstr1(MOV, None, Some(NE), R3, Operand(Literal(342), NoShift)));
                            (JInstr1(MOV, None, Some(EQ), R4, Operand(Literal(999), Left(1))));
                            (JInstr3(ADD, None, None, R5, R4, Operand(Literal(1000), NoShift)));
                            (JInstr4(LSL, None, None, R6, R5, Literal(2)));
                            (JInstr4(LSL, None, None, R7, R5, ID(R1)))
                        ]

        let astLabelMap = buildAST parseList
        printfn "ast and label map are:\n%A\n" astLabelMap
        let state = MachineState.init(astLabelMap)
        //let result = reduce state 0 24
        let result = execute state
        printfn "final result state for this ast is:\n%A\n" result

        printfn "Finished testAST.\n"

        

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