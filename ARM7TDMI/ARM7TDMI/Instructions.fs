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
     
    //sets Zero flag. Returns a state
    let setZero a state =
        if a = 0 then (( ^- ) Z true state) else (( ^- ) Z false state)

    //sets N flag. Returns a state
    let setNegative a state =
        if a < 0 then ( ^- ) N true state else ( ^- ) N false state

    //sets Carry flag by representing inputs as uint64 and applying logical operations, returns result in 32-bits
    let setCarryA operation a b state =
        let newA = uint64 (uint32 a)
        let newB = uint64 (uint32 b)
        (int ((operation newA newB) &&& 4294967295uL),( ^- ) C (((operation newA newB) &&& (1UL <<< 32)) > 0uL) state)

    let setCarryL state a =
        (int (a &&& 4294967295uL),( ^- ) C ((a &&& (1uL <<< 32)) > 0uL) state)

    let opVal state (x: Operand) =  match x with
                                    | ID(register) -> (^.) register state
                                    | Literal(data) -> data 

    

    //Op2 can be either a register or a literal
    //type OP2 = Reg of Register | Op of Data
    //right now implementing adcs
    let addWithCarryS ((regD: RegisterID), (regN: RegisterID), (op2: Operand), (state: MachineState), (includeCarry: bool), (setFlags: bool), (shift: ShiftDirection))=

        //extracting operand values
        let regNValue = (^.) regN state

        let op2Value = match shift with
                       | NoShift -> opVal state op2
                       | Right x -> int ((uint32 (opVal state op2)) >>> x)
                       | Left x -> (opVal state op2) <<< x
        
        printfn "%A" op2Value
        //including the value of carry into the first register and producing a new state : (newRegisterValue:Data,newState:MachineState)
        let newRegVal =
            match setFlags && includeCarry && ( ^* ) C state with
            |true ->  setCarryA (+) regNValue (Data 1) state
            |false when includeCarry && ( ^* ) C state -> fst (setCarryA (+) regNValue (Data 1) state), state
            |false -> (regNValue, state)
       
        //updating the state encapsulation
        let state1 = snd newRegVal

        //Producing result of the operation, along with a state that reflects the change by the carry : (finalResult: Data, newState: MachineState)
        //Under correct execution, the C flag of state1 should only reflect
        let finVal = match setFlags && ( ^* ) C state1 with
                     |true |false when not setFlags -> (fst (setCarryA (+) regNValue op2Value state1)),state1
                     |false when setFlags -> setCarryA (+) regNValue op2Value state1
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

    let mov ((regD: RegisterID), (op2: Operand), (state: MachineState), (setFlags: bool), (shift: ShiftDirection)) =

        let op2ValTuple = match shift with 
                          | Left x when setFlags -> (uint64(uint32(opVal state op2)) <<< x) |> setCarryL state
                          | Left x -> ((opVal state op2) <<< x),state
                          | Right x when setFlags-> (uint64(uint32(opVal state op2)) >>> x) |> setCarryL state
                          | Right x -> ((opVal state op2) >>> x),state
                          | NoShift when setFlags-> uint64(uint32(opVal state op2)) |> setCarryL state
                          | NoShift -> (opVal state op2),state
        
        let op2Value = fst op2ValTuple

        let state0 = snd op2ValTuple

        //Obtaining state reflecting sign of result
        let state1 = if setFlags then setNegative op2Value state0 else state0

        //Obtaining state reflecting zero status
        let finState = if setFlags then setZero op2Value state1 else state1

        (^=) (regD) (op2Value) (finState)

    let mvn ((regD: RegisterID), (op2: Operand), (state: MachineState), (setFlags: bool),(shift: ShiftDirection)) =

        let op2Value = ~~~ (opVal state op2)
        
        //Obtaining state reflecting sign of result
        let state1 = if setFlags then setNegative op2Value state else state

        //Obtaining state reflecting zero status
        let finState = if setFlags then setZero op2Value state1 else state1

        (^=) (regD) (op2Value) (finState)

    let orr ((regD: RegisterID), (regN: RegisterID), (op2: Operand), (state: MachineState), (setFlags: bool),(shift: ShiftDirection)) =

        //extracting operand values
        let regNValue = (^.) regN state

        let op2Value = opVal state op2

        //performing ORR instruction
        let result = regNValue ||| op2Value

        let state1 = if setFlags then setNegative result state else state

        //Obtaining state reflecting zero status
        let finState = if setFlags then setZero result state1 else state1

        (^=) (regD) (result) (finState)


    let andOp ((regD: RegisterID), (regN: RegisterID), (op2: Operand), (state: MachineState), (setFlags: bool),(shift: ShiftDirection)) =

        //extracting operand values
        let regNValue = (^.) regN state

        let op2Value = opVal state op2

        //Performing AND Instruction
        let result = regNValue &&& op2Value

        let state1 = if setFlags then setNegative result state else state

        //Obtaining state reflecting zero status
        let finState = if setFlags then setZero result state1 else state1

        (^=) (regD) (result) (finState)

    let eOR ((regD: RegisterID), (regN: RegisterID), (op2: Operand), (state: MachineState), (setFlags: bool),(shift: ShiftDirection)) =

        //extracting operand values
        let regNValue = (^.) regN state

        let op2Value = opVal state op2

        //Performing EOR Instruction
        let result = regNValue ^^^ op2Value

        let state1 = if setFlags then setNegative result state else state

        //Obtaining state reflecting zero status
        let finState = if setFlags then setZero result state1 else state1

        (^=) (regD) (result) (finState)

    let bic ((regD: RegisterID), (regN: RegisterID), (op2: Operand), (state: MachineState), (setFlags: bool),(shift: ShiftDirection)) =

        //extracting operand values
        let regNValue = (^.) regN state

        let op2Value = opVal state op2

        printfn "%A" (~~~op2Value)
        //Performing BIC Instruction
        let result = regNValue &&& (~~~op2Value)
        printfn "%A" (result)

        let state1 = if setFlags then setNegative result state else state

        //Obtaining state reflecting zero status
        let finState = if setFlags then setZero result state1 else state1

        (^=) (regD) (result) (finState)   

    //wrapper for LSL/LSR (S) functions
    let logicalShift ((regD: RegisterID), (op2: Operand), (state: MachineState), (setFlags: bool), (shift: ShiftDirection)) =
        mov (regD,op2,state,setFlags,shift)

    
    let arithmeticRightShift ((regD: RegisterID), (op2: Operand), (shift: int), (state: MachineState), (setFlags: bool)) =

        let op2Value = opVal state op2

        let result = op2Value >>> shift

        let state1 = if setFlags then ( ^- ) C false state else state

        let state2 = if setFlags then setNegative result state1 else state1

        //Obtaining state reflecting zero status
        let finState = if setFlags then setZero result state2 else state2

        (^=) (regD) (result) (finState)

    let subtractWithCarryS ((regD: RegisterID), (regN: RegisterID), (op2: Operand), (state: MachineState), (includeCarry: bool), (setFlags: bool), (shift: ShiftDirection))=

        //extracting operand values
        let regNValue = (^.) regN state

        let op2Value = match shift with
                       | NoShift -> opVal state op2
                       | Right x -> int ((uint32 (opVal state op2)) >>> x)
                       | Left x -> (opVal state op2) <<< x
        
        let carryVal = ( ^* ) C state

        //including the value of carry into the first register and producing a new state : (newRegisterValue:Data,newState:MachineState)
        let newRegVal =
            match setFlags && includeCarry && carryVal with
            |true ->  setCarryA (+) regNValue (Data 1) state
            |false when includeCarry && carryVal -> fst (setCarryA (+) regNValue (Data 1) state), state
            |false -> (regNValue, state)
       
        //updating the state encapsulation
        let state1 = snd newRegVal

        //Producing result of the operation, along with a state that reflects the change by the carry : (finalResult: Data, newState: MachineState)
        //Under correct execution, the C flag of state1 should only reflect
        let finVal = match setFlags && ( ^* ) C state1 with
                     |true |false when not setFlags -> (fst (setCarryA (+) regNValue op2Value state1)),state1
                     |false when setFlags -> setCarryA (+) regNValue op2Value state1
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

    
    ////test code for addWithCarry Function
    //let a = MachineState.make()
    //let b = mov (R0, Literal(-1073741824), a, true, NoShift)
    //let c = (^=) R1 -268435456 b
    //let d = ( ^- ) C false c
    //let e = ( ^- ) V false d
    //let f = ( ^- ) N false e
    //let g = ( ^- ) Z false f
    //let h = addWithCarryS (R0,R0,ID(R0),g, false, true,Right 16)
    //let i = addWithCarryS (R2,R0,ID(R1),h, true, true,NoShift)
    //printfn "%A" b
    //printfn "%A" c
    //printfn "%A" h
    //printfn "%A" i

    ////test code for mov Function
    //let a1 = MachineState.make()
    //let b1 = mov (R0, Literal(-1073741824), a1, true)
    //let c1 = mov (R0, Literal(1),b1,true)
    //printfn "%A" c1

    ////test code for mvn Function
    //let a2 = MachineState.make()
    //let b2 = mvn (R0, Literal(0), a2, true)
    //printfn "%A" b2
    //let c2 = mvn (R0, Literal(1),b2,true)
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

    //test code for LSL(S) Function
    let a3 = MachineState.make()
    let b3 = arithmeticRightShift (R0, Literal(-1), 1, a3, true)
    printfn "%A" b3
    let c3 = arithmeticRightShift (R1, Literal(1),1, b3,true)
    printfn "%A" c3