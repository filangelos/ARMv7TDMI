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
    let private ( ^% ) = Optics.get MachineState.AST_
    let private ( ^& ) = Optics.get Memory.AST_
    let private ( ^~ ) = Optics.set Memory.AST_
    let private ( ^* ) = Optics.get Memory.Storage_
    let private ( ^- ) = Optics.set Memory.Storage_
    let private ( ^> ) = Optics.get Memory.Labels_
    let private ( ^< ) = Optics.set Memory.Labels_

    ///adds an instruction node to the AST, address should be an int and assigned in the parser (increase addr by 4 bytes for each instr. and label parsed).
    let private addInstruction (ast:AST) (f:Instr) (addr:Address) =
        ast @ [(f, addr)]

    ///adds a label node to the AST, address should be an int and assigned in the parser (increase addr by 4 bytes for each instr. parsed).
    let private addLabel (labelMap: LabelMap) (name:string) (addr:Address) =
        labelMap.Add (name, addr)

    ///Build the AST and label map from the list given by Parser
    (*
    let buildAST (parseLst:(Instr list)) =
        let rec addNode (lst:(Instr list)) (ast:AST) (labelMap: LabelMap) (pc:int) =
            match lst with
            | [] -> ast, labelMap
            | node::t ->
                match node with
                | (JError(s)) ->
                        printfn "Found error while parsing: %A." s
                        failwithf "Found error while parsing: %A." s
                | (JLabel(s)) -> addNode t ast (addLabel labelMap s pc) (pc+4)
                | instr -> addNode t (addInstruction ast instr pc) labelMap (pc+4)
        addNode parseLst [] Map.empty<string, Address> 0

    *)

    let private addDCD (mem:Memory) (label:string) (dataLst:(int list)) =
        printfn "data list is %A" dataLst
        let newMemLabel = ( ^< ) (addLabel ((^>) mem) label (Memory.next mem)) mem
        printfn "added label"
        let rec addData (m:Memory) (lst:(int list)) =
            match lst with
            | [] -> m
            | d::t -> addData (Memory.push d m) t
        addData newMemLabel dataLst

    //let private addFILL (mem:Memory) (label:Option<string>) (data)

    ///Build the Memory for MachineState from the list given by Parser
    let buildMemory (parseLst:(Instr list)) =
        let rec addNode (lst:(Instr list)) (mem:Memory) (pc:int) =
            match lst with
            | [] -> mem
            | node::t ->
                match node with
                | JInstrEOF -> addNode t mem pc
                | (JError(s)) ->
                        failwithf "Found error while parsing: %A." s
                | (JLabel(s)) -> 
                    let newMem = ( ^< ) (addLabel ((^>) mem) s pc) mem 
                    addNode t newMem (pc+4)
                | JInstrDCD((label, _), dataLst) ->      
                    let newMem = addDCD mem label dataLst
                    printfn "doing this"
                    addNode t newMem pc
                | JInstrEQU((label, _), d) ->
                    let newMemLabel = ( ^< ) (addLabel ((^>) mem) label (Memory.next mem)) mem
                    let newMem = Memory.push d newMemLabel
                    addNode t newMem pc
                //| JInstrFILL((label, _), fillSize) ->
                    
                | instr ->
                    let newMem = ( ^~ ) (addInstruction ((^&) mem) instr pc) mem 
                    addNode t newMem (pc+4)
        addNode parseLst (makeEmpty ()) 0


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
                | JInstr1((((MOV, setFlags), cond), rd), rn) ->
                    InstructionsInterfaces.mov_ (rd, rn, state, (if setFlags = None then false else true), cond)
                | JInstr1((((MVN, setFlags), cond), rd), rn) ->
                    InstructionsInterfaces.mvn_ (rd, rn, state, (if setFlags = None then false else true), cond)
                | JInstr2(((ADR, cond), rd), expr) ->
                    InstructionsInterfaces.adr_(rd, expr, state, cond)
                | JInstr3(((((ADD, setFlags), cond), rd), rn), op2) ->
                    InstructionsInterfaces.add_ (rd, rn, op2, state, (not (setFlags = None)), cond)
                | JInstr3(((((ADC, setFlags), cond), rd), rn), op2) ->
                    InstructionsInterfaces.adc_ (rd, rn, op2, state, (not (setFlags = None)), cond)
                | JInstr3(((((SUB, setFlags), cond), rd), rn), op2) ->
                    InstructionsInterfaces.sub_ (rd, rn, op2, state, (not (setFlags = None)), cond)
                | JInstr3(((((SBC, setFlags), cond), rd), rn), op2) ->
                    InstructionsInterfaces.sbc_ (rd, rn, op2, state, (not (setFlags = None)), cond)
                | JInstr3(((((RSB, setFlags), cond), rd), rn), op2) ->
                    InstructionsInterfaces.rsb_ (rd, rn, op2, state, (not (setFlags = None)), cond)
                | JInstr3(((((RSC, setFlags), cond), rd), rn), op2) ->
                    InstructionsInterfaces.rsc_ (rd, rn, op2, state, (not (setFlags = None)), cond)
                | JInstr3(((((AND, setFlags), cond), rd), rn), op2) ->
                    InstructionsInterfaces.and_ (rd, rn, op2, state, (not (setFlags = None)), cond)
                | JInstr3(((((EOR, setFlags), cond), rd), rn), op2) ->
                    InstructionsInterfaces.eor_ (rd, rn, op2, state, (not (setFlags = None)), cond)
                | JInstr3(((((BIC, setFlags), cond), rd), rn), op2) ->
                    InstructionsInterfaces.bic_ (rd, rn, op2, state, (not (setFlags = None)), cond)
                | JInstr3(((((ORR, setFlags), cond), rd), rn), op2) ->
                    InstructionsInterfaces.orr_ (rd, rn, op2, state, (not (setFlags = None)), cond)
                | JInstr4(((((LSL, setFlags), cond), rd), rn), Literal(shift)) ->
                    InstructionsInterfaces.lsl_ (rd, Operand(ID(rn), ShiftDirection.Left(shift)), state, (not (setFlags = None)), cond)
                | JInstr4(((((LSL, setFlags), cond), rd), rn), ID(reg)) ->
                    InstructionsInterfaces.lsl_ (rd, Operand(ID(rn), ShiftDirection.Left(( ^. ) reg state)), state, (not (setFlags = None)), cond)
                | JInstr4(((((LSR, setFlags), cond), rd), rn), Literal(shift)) ->
                    InstructionsInterfaces.lsr_ (rd, Operand(ID(rn), ShiftDirection.RightL(shift)), state, (not (setFlags = None)), cond)
                | JInstr4(((((LSR, setFlags), cond), rd), rn), ID(reg)) ->
                    InstructionsInterfaces.lsr_ (rd, Operand(ID(rn), ShiftDirection.RightL(( ^. ) reg state)), state, (not (setFlags = None)), cond)
                | JInstr4(((((ASR, setFlags), cond), rd), rn), Literal(shift)) ->
                    InstructionsInterfaces.asr_ (rd, Operand(ID(rn), ShiftDirection.RightA(shift)), state, (not (setFlags = None)), cond)
                | JInstr4(((((ASR, setFlags), cond), rd), rn), ID(reg)) ->
                    InstructionsInterfaces.asr_ (rd, Operand(ID(rn), ShiftDirection.RightA(( ^. ) reg state)), state, (not (setFlags = None)), cond)
                | JInstr4(((((ROR_, setFlags), cond), rd), rn), Literal(shift)) ->
                    InstructionsInterfaces.ror_ (rd, Operand(ID(rn), ShiftDirection.ROR(shift)), state, (not (setFlags = None)), cond)
                | JInstr4(((((ROR_, setFlags), cond), rd), rn), ID(reg)) ->
                    InstructionsInterfaces.ror_ (rd, Operand(ID(rn), ShiftDirection.ROR(( ^. ) reg state)), state, (not (setFlags = None)), cond)
                | JInstr5((((RRX_, setFlags), cond), rd), op2) ->
                    InstructionsInterfaces.rrx_ (rd, Operand(op2, ShiftDirection.RRX), state, (not (setFlags = None)), cond)
                | JInstr6(((CMP, cond), rd), op2) ->
                    InstructionsInterfaces.cmp_ (rd, op2, state, cond)
                | JInstr6(((CMN, cond), rd), op2) ->
                    InstructionsInterfaces.cmn_ (rd, op2, state, cond)
                | JInstr6(((TST, cond), rd), op2) ->
                    InstructionsInterfaces.tst_ (rd, op2, state, cond)
                | JInstr6(((TEQ, cond), rd), op2) ->
                    InstructionsInterfaces.teq_ (rd, op2, state, cond)
                | JInstr7((((LDR, None), cond), rd), addrReg) ->
                    InstructionsInterfaces.ldr_(rd, addrReg, state, cond)
                | JInstr7((((LDR, Some(B)), cond), rd), addrReg) ->
                    InstructionsInterfaces.ldrb_(rd, addrReg, state, cond)
                | JInstr7((((STR, None), cond), rd), addrReg) ->
                    InstructionsInterfaces.str_(rd, addrReg, state, cond)
                | JInstr7((((STR, Some(B)), cond), rd), addrReg) ->
                    InstructionsInterfaces.strb_(rd, addrReg, state, cond)
                | JInstr8(((((LDM, dir), cond), rd), writeBack), regList) ->
                    InstructionsInterfaces.ldm_(dir, rd, regList, state, writeBack, cond)
                | JInstr8(((((STM, dir), cond), rd), writeBack), regList) ->
                    InstructionsInterfaces.stm_(dir, rd, regList, state, writeBack, cond)
                | JInstr9((B_,cond), label) ->
                    InstructionsInterfaces.b_(label, state, cond)
                | _ -> failwithf "Could not execute node in the ast: %A" currentNode
                    
        with
            | _ -> state

    ///reduces an ast in a MachineState by executing the nodes between pc and maxPC
    let rec reduce (state:MachineState) (pc:int) (maxPC:int) =
        if pc <= maxPC then
            let updateStateR15 = ( ^= ) R15 pc state
            let newState = step updateStateR15
            let newPC = ( ^. ) R15 newState
            reduce (newState) (newPC+1) maxPC
        else
            state

    ///execute an entire ast in a MachineState
    let execute (state:MachineState) = 
        //get address of last node
        let ast = ( ^% ) state
        if not ast.IsEmpty then
            let maxPC = snd (ast |> List.rev |> List.head)
            reduce state 0 maxPC
        else
            printfn "Error: AST is empty."
            state

    (*--------------------------------------------------------TESTING--------------------------------------------------------*)

    let testAST () =
        printfn "Running testAST:"
        let parseList = [
                            (JLabel("START"));
                            (JInstr1((((MOV, None), None), R1), Operand(Literal(1), NoShift)));
                            (JInstr1((((MOV, Some(S)), None), R2), Operand(Literal(0), NoShift)));
                            (JLabel("LABEL2"));
                            (JInstr1((((MOV, None), Some(NE)), R3), Operand(Literal(342), NoShift)));
                            (JInstr1((((MOV, None), Some(EQ)), R4), Operand(Literal(999), Left(1))));
                            (JInstr3(((((ADD, None), None), R5), R4), Operand(Literal(1000), NoShift)));
                            (JInstr4(((((LSL, None), None), R6), R5), Literal(2)));
                            (JInstr4(((((ROR_, None), None), R7), R5), ID(R1)));
                            (JInstr5((((RRX_, None), None), R8), ID(R5)));
                            (JInstr6(((CMP, None), R1), Operand(Literal(2), NoShift)))
                        ]

        let mem = buildMemory parseList
        printfn "Memory is:\n%A\n" mem
        let state = MachineState.initTMP(mem)
        //let result = reduce state 0 24
        let result = execute state
        printfn "final result state for this ast is:\n%A\n" result

        let testBranchList = [
                                (JInstr1((((MOV, None), None), R0), Operand(Literal(5), NoShift)));
                                (JLabel("JUMP"));
                                (JInstr3(((((SUB, Some(S)), None), R0), R0), Operand(Literal(1), NoShift)));
                                (JInstr9((B_, Some(NE)), "JUMP"));
                                (JInstr1((((MOV, None), None), R1), Operand(Literal(99), NoShift)));
                             ]

        printfn "Testing branching loop:"
        let mem = buildMemory testBranchList
        printfn "Memory is:\n%A\n" mem
        let state = MachineState.initTMP(mem)
        //let result = reduce state 0 24
        let result = execute state
        printfn "final result state after branching is:\n%A\n" result

        let testMemList =    [
                                JInstrDCD(("data1", DCD),[1;5;20]);
                                JInstrEQU(("data2", EQU), 100);
                                JInstr2(((ADR, None), R3), Expression.Lab("data2"))
                                JInstr7((((LDR, None), None), R4), {register= R3; offset= NoOffset} )
                            ]
        
        printfn "Testing memory instructions:"
        let mem = buildMemory testMemList
        printfn "Memory is:\n%A\n" mem
        let state = MachineState.initTMP(mem)
        //let result = reduce state 0 24
        let result = execute state
        printfn "final result state after branching is:\n%A\n" result

        printfn "Finished testAST.\n"

