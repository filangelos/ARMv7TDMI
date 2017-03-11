namespace ARM7TDMI

(* 
    High Level Programming @ Imperial College London # Spring 2017
    Project: A user-friendly ARM7TDMI assembler and simulator in F# and Web Technologies ( Github Electron & Fable Compliler )

    Contributors: Youssef Rizk

    Module: Instructions
    Description: 
*)

module Instructions =

    open MachineState

    // stortcuts
    let ( ^. ) = Optics.get MachineState.Register_
    let ( ^= ) = Optics.set MachineState.Register_
    let ( ^* ) = Optics.get MachineState.Flag_
    let ( ^- ) = Optics.set MachineState.Flag_

    //NOTE: Check if V flag is set by shift operations
    //Checks overflow. Returns tuple containing value of addition (not used) and new state reflecting new overflow flag
    let setOverflow a b state =
        try ((Checked.(+) a b),(( ^- ) V false state)) |> snd with
        e -> (1,(( ^- ) V true state)) |> snd

    let setOverflow1 a b state =
        let newA = int64 a
        let newB = int64 b
        let sum = newA + newB
        match sum < (-2147483648L) with
        | true ->  (( ^- ) V true state)
        | false when sum >= (2147483648L) ->  (( ^- ) V true state)
        | false -> ( ^- ) V false state
           
    //sets Zero flag. Returns a state
    let setZero a state =
        if a = 0 then (( ^- ) Z true state) else (( ^- ) Z false state)

    //sets N flag. Returns a state
    let setNegative a state =
        if a < 0 then ( ^- ) N true state else ( ^- ) N false state

    //sets Carry flag by representing inputs as uint64 and applying logical operations, returns result in 32-bits
    // NOTE: This implementation means that subtraction has to be implemented as addition of a negative
    let setCarryA operation a b state =
        let newA = uint64 (uint32 a)
        let newB = uint64 (uint32 b)
        (int ((operation newA newB) &&& 4294967295uL),( ^- ) C (((operation newA newB) &&& (1UL <<< 32)) > 0uL) state)

    let setCarryL state a =
        (int (a &&& 4294967295uL),( ^- ) C ((a &&& (1uL <<< 32)) > 0uL) state)

    let setCarryRShift state a b =
        (a,( ^- ) C ((uint32 (b &&& (1 <<< 31))) > 0u) state)


    let opVal state (x: Input) =  match x with
                                    | ID(register) -> (^.) register state
                                    | Literal(data) -> data 

    

    //Op2 can be either a register or a literal
    //type OP2 = Reg of Register | Op of Data
    //right now implementing adcs
    let addWithCarryS ((regD: RegisterID), (regN: RegisterID), (op2: Operand), (state: MachineState), (includeCarry: bool), (setFlags: bool))=

        //extracting operand values
        let regNValue = (^.) regN state

        let op2Value = match op2 with
                       | Operand(op2Val, NoShift) -> opVal state op2Val
                       | Operand(op2Val, RightL(x)) -> int ((uint32 (opVal state op2Val)) >>> x)
                       | Operand(op2Val, RightA(x)) -> ((opVal state op2Val) >>> x)
                       | Operand(op2Val, Left(x)) -> (opVal state op2Val) <<< x
        
        //including the value of carry into the first register and producing a new state : (newRegisterValue:Data,newState:MachineState)
        let newRegVal =
            match setFlags && includeCarry && ( ^* ) C state with
            |true ->  setCarryA (+) regNValue (Data 1) state
            |false when includeCarry && ( ^* ) C state -> fst (setCarryA (+) regNValue (Data 1) state), state
            |false -> (regNValue, state)
       
        //updating the state encapsulation
        let state1 = snd newRegVal
        let newReg = fst newRegVal

        //Producing result of the operation, along with a state that reflects the change by the carry : (finalResult: Data, newState: MachineState)
        //Under correct execution, the C flag of state1 should only reflect
        let finVal = match setFlags && ( ^* ) C state1 with
                     |true |false when not setFlags -> (fst (setCarryA (+) newReg op2Value state1)),state1
                     |false when setFlags -> setCarryA (+) newReg op2Value state1
                     | _ -> failwithf "This will never happen"

        //updating the state encapsulation again     
        let state2 = snd finVal

        let result = fst finVal

        //Obtaining state reflecting signed overflow
        let state3 = if setFlags then (setOverflow regNValue op2Value state2) else state2

        //Obtaining state reflecting sign of result
        let state4 = if setFlags then setNegative result state3 else state3

        //Obtaining state reflecting zero status
        let finState = if setFlags then setZero result state4 else state4

        (^=) (regD) (result) (finState)

    let add_ ((regD: RegisterID), (regN: RegisterID), (op2: Operand), (state: MachineState)) =
        addWithCarryS (regD, regN, op2, state, false, false)

    let add_S ((regD: RegisterID), (regN: RegisterID), (op2: Operand), (state: MachineState)) =
        addWithCarryS (regD, regN, op2, state, false, true)

    let adc_ ((regD: RegisterID), (regN: RegisterID), (op2: Operand), (state: MachineState)) =
        addWithCarryS (regD, regN, op2, state, true, false)

    let adc_S ((regD: RegisterID), (regN: RegisterID), (op2: Operand), (state: MachineState)) =
        addWithCarryS (regD, regN, op2, state, true, true)

    let mov ((regD: RegisterID), (op2: Operand), (state: MachineState), (setFlags: bool)) =

        let op2ValTuple = match op2 with 
                          | Operand(op2Val, NoShift) when setFlags -> setCarryRShift state (opVal state op2Val) 0
                          | Operand(op2Val, RightL x) when setFlags -> setCarryRShift state (int (uint32 (opVal state op2Val) >>> x)) ((opVal state op2Val) <<< (32 - x)) 
                          | Operand(op2Val, RightA(x)) when setFlags -> setCarryRShift state ((opVal state op2Val) >>> x) ((opVal state op2Val) <<< (32 - x)) 
                          | Operand(op2Val, Left x) when setFlags -> (uint64(uint32(opVal state op2Val)) <<< x) |> setCarryL state
                          | Operand(op2Val, NoShift) -> (opVal state op2Val), state
                          | Operand(op2Val, RightL x) -> int ((uint32 (opVal state op2Val)) >>> x), state
                          | Operand(op2Val, RightA(x)) -> ((opVal state op2Val) >>> x), state
                          | Operand(op2Val, Left x) -> ((opVal state op2Val) <<< x), state
        
        let op2Value = fst op2ValTuple

        let state0 = snd op2ValTuple

        //Obtaining state reflecting sign of result
        let state1 = if setFlags then setNegative op2Value state0 else state0

        //Obtaining state reflecting zero status
        let finState = if setFlags then setZero op2Value state1 else state1

        (^=) (regD) (op2Value) (finState)

    let mvn ((regD: RegisterID), (op2: Operand), (state: MachineState), (setFlags: bool)) =
        let op2Value1 = match op2 with
                        | Operand(op2Val, NoShift) when setFlags -> setCarryRShift state (opVal state op2Val) 0
                        | Operand(op2Val, RightL x) when setFlags -> setCarryRShift state (int (uint32 (opVal state op2Val) >>> x)) ((opVal state op2Val) <<< (32 - x)) 
                        | Operand(op2Val, RightA(x)) when setFlags -> setCarryRShift state ((opVal state op2Val) >>> x) ((opVal state op2Val) <<< (32 - x)) 
                        | Operand(op2Val, Left x) when setFlags -> (uint64(uint32(opVal state op2Val)) <<< x) |> setCarryL state
                        | Operand(op2Val, NoShift) -> (opVal state op2Val), state
                        | Operand(op2Val, RightL x) -> int ((uint32 (opVal state op2Val)) >>> x), state
                        | Operand(op2Val, RightA(x)) -> ((opVal state op2Val) >>> x), state
                        | Operand(op2Val, Left x) -> ((opVal state op2Val) <<< x), state

        let op2Value = ~~~ (fst op2Value1)
        let state0 = snd op2Value1
        
        //Obtaining state reflecting sign of result
        let state1 = if setFlags then setNegative op2Value state0 else state0

        //Obtaining state reflecting zero status
        let finState = if setFlags then setZero op2Value state1 else state1

        (^=) (regD) (op2Value) (finState)

    let orr ((regD: RegisterID), (regN: RegisterID), (op2: Operand), (state: MachineState), (setFlags: bool)) =

        //extracting operand values
        let regNValue = (^.) regN state

        let op2Value = match op2 with
                       | Operand(op2Val, NoShift) -> opVal state op2Val
                       | Operand(op2Val, RightL x) -> int ((uint32 (opVal state op2Val)) >>> x)
                       | Operand(op2Val, RightA(x)) -> ((opVal state op2Val) >>> x)
                       | Operand(op2Val, Left x) -> (opVal state op2Val) <<< x

        //performing ORR instruction
        let result = regNValue ||| op2Value

        let state1 = if setFlags then setNegative result state else state

        //Obtaining state reflecting zero status
        let finState = if setFlags then setZero result state1 else state1

        (^=) (regD) (result) (finState)


    let andOp ((regD: RegisterID), (regN: RegisterID), (op2: Operand), (state: MachineState), (setFlags: bool)) =

        //extracting operand values
        let regNValue = (^.) regN state

        let op2Value = match op2 with
                       | Operand(op2Val, NoShift) -> opVal state op2Val
                       | Operand(op2Val, RightL x) -> int ((uint32 (opVal state op2Val)) >>> x)
                       | Operand(op2Val, RightA(x)) -> ((opVal state op2Val) >>> x)
                       | Operand(op2Val, Left x) -> (opVal state op2Val) <<< x

        //Performing AND Instruction
        let result = regNValue &&& op2Value

        let state1 = if setFlags then setNegative result state else state

        //Obtaining state reflecting zero status
        let finState = if setFlags then setZero result state1 else state1

        (^=) (regD) (result) (finState)

    let eOR ((regD: RegisterID), (regN: RegisterID), (op2: Operand), (state: MachineState), (setFlags: bool)) =

        //extracting operand values
        let regNValue = (^.) regN state

        let op2Value = match op2 with
                       | Operand(op2Val, NoShift) -> opVal state op2Val
                       | Operand(op2Val, RightL x) -> int ((uint32 (opVal state op2Val)) >>> x)
                       | Operand(op2Val, RightA(x)) -> ((opVal state op2Val) >>> x)
                       | Operand(op2Val, Left x) -> (opVal state op2Val) <<< x

        //Performing EOR Instruction
        let result = regNValue ^^^ op2Value

        let state1 = if setFlags then setNegative result state else state

        //Obtaining state reflecting zero status
        let finState = if setFlags then setZero result state1 else state1

        (^=) (regD) (result) (finState)

    let bic ((regD: RegisterID), (regN: RegisterID), (op2: Operand), (state: MachineState), (setFlags: bool)) =

        //extracting operand values
        let regNValue = (^.) regN state

        let op2Value = match op2 with
                       | Operand(op2Val, NoShift) -> opVal state op2Val
                       | Operand(op2Val, RightL x) -> int ((uint32 (opVal state op2Val)) >>> x)
                       | Operand(op2Val, RightA(x)) -> ((opVal state op2Val) >>> x)
                       | Operand(op2Val, Left x) -> (opVal state op2Val) <<< x

        printfn "%A" (~~~op2Value)
        //Performing BIC Instruction
        let result = regNValue &&& (~~~op2Value)
        printfn "%A" (result)

        let state1 = if setFlags then setNegative result state else state

        //Obtaining state reflecting zero status
        let finState = if setFlags then setZero result state1 else state1

        (^=) (regD) (result) (finState)   

    //wrapper for LSL/LSR (S) functions
    let logicalShift ((regD: RegisterID), (op2: Operand), (state: MachineState), (setFlags: bool)) =
        mov (regD,op2,state,setFlags)

    
    let arithmeticRightShift ((regD: RegisterID), (op2: Input), (shift: int), (state: MachineState), (setFlags: bool)) =
        mov (regD, Operand(op2, RightA shift), state, setFlags)

    let subtractWithCarryS ((regD: RegisterID), (regN: Operand), (op2: Operand), (state: MachineState), (includeCarry: bool), (setFlags: bool)) =

      //extracting operand values
      let regNValue = match regN with
                      | Operand(op2Val, NoShift) -> opVal state op2Val
                      | Operand(op2Val, RightL x) -> int ((uint32 (opVal state op2Val)) >>> x)
                      | Operand(op2Val, RightA(x)) -> ((opVal state op2Val) >>> x)
                      | Operand(op2Val, Left x) -> (opVal state op2Val) <<< x

      let op2Value = match op2 with
                     | Operand(op2Val, NoShift) -> opVal state op2Val
                     | Operand(op2Val, RightL x) -> int ((uint32 (opVal state op2Val)) >>> x)
                     | Operand(op2Val, RightA(x)) -> ((opVal state op2Val) >>> x)
                     | Operand(op2Val, Left x) -> (opVal state op2Val) <<< x
      
      let newRegVal =
          match setFlags && includeCarry && ( ^* ) C state with
          |true ->  setCarryA (+) regNValue (Data 1) state
          |false when setFlags && not includeCarry ->  setCarryA (+) regNValue (Data 1) state
          |false when not setFlags && includeCarry && not (( ^* ) C state ) -> regNValue, state
          |false when not setFlags -> fst (setCarryA (+) regNValue (Data 1) state), state// implementing Rn + not(Op2) + 1
          |false -> setCarryA (+) regNValue (Data 0) state
       
        //updating the state encapsulation
      let state1 = snd newRegVal
      let newReg = fst newRegVal

        //Producing result of the operation, along with a state that reflects the change by the carry : (finalResult: Data, newState: MachineState)
        //Under correct execution, the C flag of state1 should only reflect
      let finVal = match setFlags && ( ^* ) C state1 with
                   |true  -> (fst (setCarryA (+) newReg (~~~op2Value) state1)),state1
                   |false when setFlags -> setCarryA (+) newReg (~~~op2Value) state1
                   |false -> (fst (setCarryA (+) newReg (~~~op2Value) state1)),state1

        //updating the state encapsulation again     
      let state2 = snd finVal

      let result = fst finVal

        //Obtaining state reflecting signed overflow
      let state3 = if setFlags then (setOverflow1 regNValue op2Value state2) else state2

        //Obtaining state reflecting sign of result
      let state4 = if setFlags then setNegative result state3 else state3

        //Obtaining state reflecting zero status
      let finState = if setFlags then setZero result state4 else state4

      (^=) (regD) (result) (finState) 

    //wrappers for sub, sbc, cmp & rsb functions
    let sub_ ((regD: RegisterID), (regN: RegisterID), (op2: Operand), (state: MachineState)) = 
        subtractWithCarryS (regD, Operand(ID(regN),NoShift), op2, state, false, false)

    let sub_S ((regD: RegisterID), (regN: RegisterID), (op2: Operand), (state: MachineState)) = 
        subtractWithCarryS (regD, Operand(ID(regN),NoShift), op2, state, false, true)
     
    let sbc_ ((regD: RegisterID), (regN: RegisterID), (op2: Operand), (state: MachineState)) = 
        subtractWithCarryS (regD, Operand(ID(regN),NoShift), op2, state, true, false)

    let sbc_S ((regD: RegisterID), (regN: RegisterID), (op2: Operand), (state: MachineState)) = 
        subtractWithCarryS (regD, Operand(ID(regN),NoShift), op2, state, true, true)

    let rsb_ ((regD: RegisterID), (regN: RegisterID), (op2: Operand), (state: MachineState)) = 
        subtractWithCarryS (regD, op2, Operand(ID(regN),NoShift), state, false, false)

    let rsb_S ((regD: RegisterID), (regN: RegisterID), (op2: Operand), (state: MachineState)) = 
        subtractWithCarryS (regD, op2, Operand(ID(regN),NoShift), state, false, true)

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
    //let b3 = mov (R0, Literal(-2147483648), a3, false)
    //let c3 = mov (R1, Literal(0),b3,false)
    //let d3 = orr (R2, R1, ID R0, c3, true)
    //printfn "%A" d3

    ////test code for AND Function
    //let a3 = MachineState.make()
    //let b3 = mov (R0, Literal(-1), a3, false)
    //let c3 = mov (R1, Literal(1),b3,false)
    //let d3 = andOp (R2, R1, ID R0, c3, true)
    //printfn "%A" d3

    ////test code for EOR Function
    //let a3 = MachineState.make()
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

    ////test code for subtractWithCarry Function
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

    //test code for rsb Function
    let a = MachineState.make()
    let b = mov (R0, Operand(Literal(0),NoShift), a, false)
    let c = (^=) R1 5 b
    let d = ( ^- ) C false c
    let e = ( ^- ) V false d
    let f = ( ^- ) N false e
    let g = ( ^- ) Z false f
    let z = mov (R0, Operand(Literal(-1),NoShift), g, false)
    let h = cmp_ (R0,Operand(Literal 0, NoShift),z)
    let i = cmp_ (R0,Operand(Literal -1, NoShift),h)
    printfn "%A" z
    printfn "%A" h
    printfn "%A" i