namespace ARM7TDMI

(* 
    High Level Programming @ Imperial College London # Spring 2017
    Project: A user-friendly ARM7TDMI assembler and simulator in F# and Web Technologies ( Github Electron & Fable Compliler )

    Contributors: Youssef Rizk

    Module: Artihmetic Instructions
    Description: ALU Instructions
*)

module Instructions =

    open MachineState

    // Stortcuts
    let ( ^. ) = Optics.get MachineState.Register_
    let ( ^= ) = Optics.set MachineState.Register_
    let ( ^* ) = Optics.get MachineState.Flag_
    let ( ^- ) = Optics.set MachineState.Flag_

    let opVal state (x: Input) =  match x with
                                    | ID(register) -> (^.) register state
                                    | Literal(data) -> data 

    //Extracts operand value
    let getOperandVal setFlags state = function
                      | Operand(op2Val, NoShift) when setFlags -> MachineState.setCarryRShift (opVal state op2Val) 0 state 
                      | Operand(op2Val, RightL x) when setFlags -> MachineState.setCarryRShift (int (uint32 (opVal state op2Val) >>> x)) ((opVal state op2Val) <<< (32 - x)) state
                      | Operand(op2Val, RightA(x)) when setFlags -> MachineState.setCarryRShift ((opVal state op2Val) >>> x) ((opVal state op2Val) <<< (32 - x)) state
                      | Operand(op2Val, Left x) when setFlags -> MachineState.setCarryL (uint64(uint32(opVal state op2Val)) <<< x) state
                      | Operand(op2Val, ROR x) when setFlags -> MachineState.setCarryRShift (((opVal state op2Val) >>> x) ||| ((opVal state op2Val) <<< (32 - x))) (((opVal state op2Val) >>> x) ||| ((opVal state op2Val) <<< (32 - x))) state
                      | Operand(op2Val, RRX) when setFlags -> MachineState.setCarryRShift ((int (uint32 (opVal state op2Val) >>> 1)) ||| ((System.Convert.ToInt32 ((^*) C state)) <<< 31)) ((opVal state op2Val) <<< 31) state
                      | Operand(op2Val, NoShift) -> (opVal state op2Val), state
                      | Operand(op2Val, RightL x) -> int ((uint32 (opVal state op2Val)) >>> x), state
                      | Operand(op2Val, RightA(x)) -> ((opVal state op2Val) >>> x), state
                      | Operand(op2Val, Left x) -> ((opVal state op2Val) <<< x), state
                      | Operand(op2Val, ROR x) -> (((opVal state op2Val) >>> x) ||| ((opVal state op2Val) <<< (32 - x))), state
                      | Operand(op2Val, RRX) -> (((opVal state op2Val) >>> 1) ||| ((System.Convert.ToInt32 ((^*) C state)) <<< 31)), state

                                   
    let mov ((regD: RegisterID), (op2: Operand), (state: MachineState), (setFlags: bool)) =

        let op2ValTuple = getOperandVal setFlags state op2
         
        let op2Value = fst op2ValTuple

        let state0 = snd op2ValTuple

        //Obtaining state reflecting sign of result
        let state1 = if setFlags then MachineState.setNegative op2Value state0 else state0

        //Obtaining state reflecting zero status
        let finState = if setFlags then MachineState.setZero op2Value state1 else state1

        (^=) (regD) (op2Value) (finState)

    let addWithCarryS ((regD: RegisterID), (regN: RegisterID), (op2: Operand), (state: MachineState), (includeCarry: bool), (setFlags: bool))=

        //extracting operand values
        let regNValue = (^.) regN state

        let op2Value = fst (getOperandVal false state op2)
        
        //including the value of carry into the first register and producing a new state : (newRegisterValue:Data,newState:MachineState)
        let newRegVal =
            match setFlags && includeCarry && ( ^* ) C state with
            |true ->  MachineState.setCarryA (+) regNValue (Data 1) state
            |false when includeCarry && ( ^* ) C state -> fst (MachineState.setCarryA (+) regNValue (Data 1) state), state
            |false when setFlags -> MachineState.setCarryA (+) regNValue (Data 0) state
            |false -> (regNValue, state)
       
        //updating the state encapsulation
        let state1 = snd newRegVal
        let newReg = fst newRegVal

        //Producing result of the operation, along with a state that reflects the change by the carry : (finalResult: Data, newState: MachineState)
        //Under correct execution, the C flag of state1 should only reflect
        let finVal = match setFlags && ( ^* ) C state1 with
                     |true  -> (fst (MachineState.setCarryA (+) newReg (op2Value) state1)),state1
                     |false when setFlags -> MachineState.setCarryA (+) newReg (op2Value) state1
                     |false -> (fst (MachineState.setCarryA (+) newReg (op2Value) state1)),state1

        //updating the state encapsulation again     
        let state2 = snd finVal

        let result = fst finVal

        //Obtaining state reflecting signed overflow
        let state3 = if setFlags then (MachineState.setOverflow regNValue op2Value state2) else state2

        //Obtaining state reflecting sign of result
        let state4 = if setFlags then MachineState.setNegative result state3 else state3

        //Obtaining state reflecting zero status
        let finState = if setFlags then MachineState.setZero result state4 else state4

        (^=) (regD) (result) (finState)
      
    let mvn ((regD: RegisterID), (op2: Operand), (state: MachineState), (setFlags: bool)) =
        let op2Value1 = getOperandVal setFlags state op2

        let op2Value = ~~~ (fst op2Value1)
        let state0 = snd op2Value1
        
        //Obtaining state reflecting sign of result
        let state1 = if setFlags then MachineState.setNegative op2Value state0 else state0

        //Obtaining state reflecting zero status
        let finState = if setFlags then MachineState.setZero op2Value state1 else state1

        (^=) (regD) (op2Value) (finState)

    let orr ((regD: RegisterID), (regN: RegisterID), (op2: Operand), (state: MachineState), (setFlags: bool)) =

        //extracting operand values
        let regNValue = (^.) regN state

        let op2Value = getOperandVal setFlags state op2

        let state0 = snd op2Value

        //performing ORR instruction
        let result = regNValue ||| fst (op2Value)

        let state1 = if setFlags then MachineState.setNegative result state0 else state0

        //Obtaining state reflecting zero status
        let finState = if setFlags then MachineState.setZero result state1 else state1

        (^=) (regD) (result) (finState)


    let andOp ((regD: RegisterID), (regN: RegisterID), (op2: Operand), (state: MachineState), (setFlags: bool)) =

        //extracting operand values
        let regNValue = (^.) regN state

        let op2Value = getOperandVal setFlags state op2

        let state0 = snd op2Value

        //Performing AND Instruction
        let result = regNValue &&& fst (op2Value)

        let state1 = if setFlags then MachineState.setNegative result state else state

        //Obtaining state reflecting zero status
        let finState = if setFlags then MachineState.setZero result state1 else state1

        (^=) (regD) (result) (finState)

    let eOR ((regD: RegisterID), (regN: RegisterID), (op2: Operand), (state: MachineState), (setFlags: bool)) =

        //extracting operand values
        let regNValue = (^.) regN state

        let op2Value = getOperandVal setFlags state op2

        let state0 = snd op2Value

        //Performing EOR Instruction
        let result = regNValue ^^^ fst (op2Value)

        let state1 = if setFlags then MachineState.setNegative result state else state

        //Obtaining state reflecting zero status
        let finState = if setFlags then MachineState.setZero result state1 else state1

        (^=) (regD) (result) (finState)

    let bic ((regD: RegisterID), (regN: RegisterID), (op2: Operand), (state: MachineState), (setFlags: bool)) =

        //extracting operand values
        let regNValue = (^.) regN state

        let op2Value = getOperandVal setFlags state op2

        let state0 = snd op2Value
        //Performing BIC Instruction
        let result = regNValue &&& (~~~ fst(op2Value))

        let state1 = if setFlags then MachineState.setNegative result state else state

        //Obtaining state reflecting zero status
        let finState = if setFlags then MachineState.setZero result state1 else state1

        (^=) (regD) (result) (finState)   

    //wrapper for LSL/LSR (S) functions
    let logicalShift ((regD: RegisterID), (op2: Operand), (state: MachineState), (setFlags: bool)) =
        mov (regD,op2,state,setFlags)

    
    let arithmeticRightShift ((regD: RegisterID), (op2: Input), (shift: int), (state: MachineState), (setFlags: bool)) =
        mov (regD, Operand(op2, RightA shift), state, setFlags)

    let subtractWithCarryS ((regD: RegisterID), (regN: Operand), (op2: Operand), (state: MachineState), (includeCarry: bool), (setFlags: bool)) =

      //extracting operand values
      let regNValue = fst (getOperandVal false state regN)

      let op2Value = fst (getOperandVal false state op2)
      
      let newRegVal =
          match setFlags && includeCarry && ( ^* ) C state with
          |true ->  MachineState.setCarryA (+) regNValue (Data 1) state
          |false when setFlags && not includeCarry ->  MachineState.setCarryA (+) regNValue (Data 1) state
          |false when not setFlags && includeCarry && not (( ^* ) C state ) -> regNValue, state
          |false when not setFlags -> fst (MachineState.setCarryA (+) regNValue (Data 1) state), state// implementing Rn + not(Op2) + 1
          |false -> MachineState.setCarryA (+) regNValue (Data 0) state
       
        //updating the state encapsulation
      let state1 = snd newRegVal
      let newReg = fst newRegVal

        //Producing result of the operation, along with a state that reflects the change by the carry : (finalResult: Data, newState: MachineState)
        //Under correct execution, the C flag of state1 should only reflect
      let finVal = match setFlags && ( ^* ) C state1 with
                   |true  -> (fst (MachineState.setCarryA (+) newReg (~~~op2Value) state1)),state1
                   |false when setFlags -> MachineState.setCarryA (+) newReg (~~~op2Value) state1
                   |false -> (fst (MachineState.setCarryA (+) newReg (~~~op2Value) state1)),state1

        //updating the state encapsulation again     
      let state2 = snd finVal

      let result = fst finVal

        //Obtaining state reflecting signed overflow
      let state3 = if setFlags then (MachineState.setOverflow regNValue op2Value state2) else state2

        //Obtaining state reflecting sign of result
      let state4 = if setFlags then MachineState.setNegative result state3 else state3

        //Obtaining state reflecting zero status
      let finState = if setFlags then MachineState.setZero result state4 else state4

      (^=) (regD) (result) (finState) 

    //wrappers for sub, sbc, cmp & rsb functions
    let sub_ ((regD: RegisterID), (regN: RegisterID), (op2: Operand), (state: MachineState), (setFlags: bool)) = 
        subtractWithCarryS (regD, Operand(ID(regN),NoShift), op2, state, false, setFlags)
     
    let sbc_ ((regD: RegisterID), (regN: RegisterID), (op2: Operand), (state: MachineState), (setFlags: bool)) = 
        subtractWithCarryS (regD, Operand(ID(regN),NoShift), op2, state, true, setFlags)

    let rsb_ ((regD: RegisterID), (regN: RegisterID), (op2: Operand), (state: MachineState), (setFlags: bool)) = 
        subtractWithCarryS (regD, op2, Operand(ID(regN),NoShift), state, false, setFlags)

    let rsc_ ((regD: RegisterID), (regN: RegisterID), (op2: Operand), (state: MachineState), (setFlags: bool)) = 
        subtractWithCarryS (regD, op2, Operand(ID(regN),NoShift), state, true, setFlags)

    let cmp_ ((regN: RegisterID), (op2: Operand), (state: MachineState)) = 
        let flagState = subtractWithCarryS (R10, Operand(ID(regN),NoShift), op2, state, false, true)
        state |> ( ^- ) V (( ^* ) V flagState) |> ( ^- ) C (( ^* ) C flagState) |> ( ^- ) N (( ^* ) N flagState) |> ( ^- ) Z (( ^* ) Z flagState)

    let cmn_ ((regN: RegisterID), (op2: Operand), (state: MachineState)) = 
        let flagState = addWithCarryS (R10, regN, op2, state, false, true)
        state |> ( ^- ) V (( ^* ) V flagState) |> ( ^- ) C (( ^* ) C flagState) |> ( ^- ) N (( ^* ) N flagState) |> ( ^- ) Z (( ^* ) Z flagState)

    let tst_ ((regN: RegisterID), (op2: Operand), (state: MachineState)) = 
        let flagState = andOp (R10, regN, op2, state, true)
        state |> ( ^- ) C (( ^* ) C flagState) |> ( ^- ) N (( ^* ) N flagState) |> ( ^- ) Z (( ^* ) Z flagState)
     
    let teq_ ((regN: RegisterID), (op2: Operand), (state: MachineState)) = 
        let flagState = eOR (R10, regN, op2, state, true)
        state |> ( ^- ) C (( ^* ) C flagState) |> ( ^- ) N (( ^* ) N flagState) |> ( ^- ) Z (( ^* ) Z flagState)

    let ror_ ((regD: RegisterID), (op2: Input), (shift: int), (state: MachineState), (setFlags: bool)) =
        mov (regD, Operand(op2,ROR shift),state,setFlags)

    let rrx_ ((regD: RegisterID), (op2: Input), (state: MachineState), (setFlags: bool)) =
        mov (regD, Operand(op2,RRX),state,setFlags)

    ////test code for addWithCarry Function
    //let a = MachineState.make()
    //let b = mov (R0, Operand(Literal(-1),NoShift), a, true)
    //let c = (^=) R1 2147483647 b
    //let d = ( ^- ) C false c
    //let e = ( ^- ) V false d
    //let f = ( ^- ) N false e
    //let g = ( ^- ) Z false f
    //let z = addWithCarryS (R3,R1,Operand(ID R1,NoShift),g, false, true)
    //let i = addWithCarryS (R3,R0,Operand(ID R0,NoShift),z, false, true)
    //printfn "%A" b
    //printfn "%A" c
    //printfn "%A" z
    //printfn "%A" i

    ////test code for mov Function
    //let a1 = MachineState.make()
    //let b1 = mov (R0, Literal(-1073741824), a1, true)
    //let c1 = mov (R0, Literal(1),b1,true)
    //printfn "%A" c1

    //test code for mvn Function
    //let a2 = MachineState.make()
    //let b2 = mvn (R0, Literal(-1), a2, true, RightL 9)
    //printfn "%A" b2
    //let c2 = mvn (R0, Literal(-1),b2,true,RightA 9)
    //printfn "%A" c2

    ////test code for ORR Function
    //let a3 = MachineState.make()
    //let b3 = mov (R0, Operand(Literal(-1),NoShift), a3, false)
    //let c3 = mov (R1, Operand(Literal(-1),NoShift),b3,false)
    //let d3 = orr (R2, R1, Operand(ID R0,RightA 3), c3, true)
    //printfn "%A" d3

    ////test code for AND Function
    //let a3 = MachineState.make()
    //let b3 = mov (R0, Operand(Literal(-1),NoShift), a3, false)
    //let c3 = mov (R1, Operand(Literal(0),NoShift),b3,false)
    //let d3 = andOp (R2, R1, Operand(ID R0, NoShift), c3, true)
    //printfn "%A" d3

    ////test code for EOR Function
    //let a3 = MachineState.make()3
    //let b3 = mov (R0, Literal(-1), a3, false)
    //let c3 = mov (R1, Literal(1),b3,false)
    //let d3 = eOR (R2, R1, ID R0, c3, true)
    //printfn "%A" d3

    ////test code for BIC Function
    //let a3 = MachineState.make()
    //let b3 = mov (R0, Literal(-1), a3, false)
    //let c3 = mov (R1, Literal(1),b3,false)
    //let d3 = bic (R2, R1, ID R0, c3, true)
    //printfn "%A" d3

    ////test code for LSL(S) Function
    //let a3 = MachineState.make()
    //let b3 = logicalShift (R0, Literal(-1), a3, true, Left 1)
    //printfn "%A" b3
    //let c3 = logicalShift (R1, Literal(1),b3,true, Right 1)
    //printfn "%A" c3

    //test code for ASR(S) Function
    //let a3 = MachineState.make()
    //let b3 = arithmeticRightShift (R0, Literal(-1), 1, a3, true)
    //printfn "%A" b3
    //let c3 = arithmeticRightShift (R1, Literal(1),1, b3,true)
    //printfn "%A" c3

    //test code for subtractWithCarry Function
    //let a = MachineState.make()
    //let b = mov (R0, Operand(Literal(0),NoShift), a, false)
    //let c = (^=) R1 5 b
    //let d = ( ^- ) C false c
    //let e = ( ^- ) V false d
    //let f = ( ^- ) N false e
    //let g = ( ^- ) Z false f
    //let z = mov (R0, Operand(Literal(-1),NoShift), g, false)
    //let h = subtractWithCarryS (R3,Operand(ID R0, NoShift),Operand(Literal 0, NoShift),z, true, true)
    //let i = subtractWithCarryS (R2,Operand(ID(R0),NoShift),Operand(ID(R8),NoShift),h, true, true)
    //printfn "%A" z
    //printfn "%A" h
    //printfn "%A" i

    ////test code for rsb Function
    //let a = MachineState.make()
    //let b = mov (R0, Operand(Literal(0),NoShift), a, false)
    //let c = (^=) R1 5 b
    //let d = ( ^- ) C false c
    //let e = ( ^- ) V false d
    //let f = ( ^- ) N false e
    //let g = ( ^- ) Z false f
    //let z = mov (R0, Operand(Literal(-1),NoShift), g, false)
    //let h = cmp_ (R0,Operand(Literal 0, NoShift),z)
    //let i = cmp_ (R0,Operand(Literal -1, NoShift),h)
    //printfn "%A" z
    //printfn "%A" h
    //printfn "%A" 

    ////test code for ror function
    //let a3 = MachineState.make()
    //let b3 = ror_ (R0, Literal(-1), 1, a3, true)
    //printfn "%A" b3
    //let c3 = ror_ (R1, Literal(1),1, b3,true)
    //printfn "%A" c3

    ////test code for rrx function
    //let a3 = MachineState.make()
    //let b3 = rrx_ (R0, Literal(-1), a3, true)
    //printfn "%A" a3
    //printfn "%A" b3
    //let c3 = rrx_ (R1, Literal(1), b3,true)
    //printfn "%A" c3

    //-------------------------------------------------TESTING----------------------------------------------------------

    //Testing mov{s} instruction
    let test_mov () = 
        printf "Starting Testing of Mov Instruction. Compare against visUAL."
        let a_mov = MachineState.make()
        let b_mov = mov (R0, Operand(Literal(-1073741824),NoShift), a_mov, false) //Moves value into Register [x]
        printfn "%A" b_mov
        let c_mov = mov (R1, Operand(ID R0,NoShift),b_mov,false) //Moves value of Register into another Register [x]
        printfn "%A" c_mov
        let d_mov = mov (R1, Operand(ID R0,NoShift),c_mov,true) //Sets N flag [x]
        printfn "%A" d_mov
        let e_mov = mov (R1, Operand(Literal 0,NoShift),d_mov,true) //Sets Z flag [x]
        printfn "%A" e_mov
        let f_mov = mov (R1, Operand(ID R0, Left 1),e_mov,true) //Sets C flag (Left Shift) [x]
        printfn "%A" f_mov
        let g_mov = mov (R1, Operand(ID R0, RightL 31),f_mov,true) //Sets C flag (Right logical Shift) [x]
        printfn "%A" g_mov
        let h_mov = mov (R1, Operand(ID R0, RightA 31),g_mov,true) //Sets C flag (Right arithmetic Shift) [x]
        printfn "%A" h_mov
        printfn "End of mov testing"


    //Testing add{s} instruction
    let test_add () = 
        printfn "Starting Testing of Add Instruction. Compare against visUAL."
        let a = MachineState.initWithFlags "0110"
//        let b = add_ (R0, R0, Operand(Literal -1, NoShift), a, true)
        printfn "Quick Test"
//        printfn "%A" b
        //let a_add = MachineState.make()
        //let b_add = mov (R0, Operand(Literal(-1073741824),NoShift), a_add, false) //Moving preliminary values
        //let c_add = mov (R1, Operand(Literal 1,NoShift),b_add,false)
        //let d_add = add_ (R2, R1, Operand(ID R0,NoShift),c_add,false) //Correct addition [x]
        //printfn "%A" d_add
        //let e_add = add_ (R2, R0, Operand(ID R0,NoShift),d_add,true) //Sets C flag [x]
        //printfn "%A" e_add
        //let f_add = add_ (R2, R2, Operand(ID R2, NoShift),e_add,true) //Sets V flag [x]
        //printfn "%A" f_add
        //let g_add = add_ (R2, R2, Operand(ID R2, NoShift),f_add,true) //Sets Z flag [x]
        //printfn "%A" g_add
        //let h_add = add_ (R2, R0, Operand(ID R2, NoShift),g_add,true)//Sets N flag [x]
        //printfn "%A" h_add
        printfn "End of Add testing"

    test_add ()
