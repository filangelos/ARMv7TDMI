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

    //extracts value from register or the value of the literal
    let opVal state (x: Input) =  match x with
                                    | ID(register) -> (^.) register state
                                    | Literal(data) -> data 

    //Extracts operand value by applying the shift if there is any and setting the flags if specified
    let getOperandVal setFlags state = function
                      | Operand(op2Val, NoShift) when setFlags -> (opVal state op2Val), state
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
                      | Operand(op2Val, RRX) -> ((int (uint32 (opVal state op2Val) >>> 1)) ||| ((System.Convert.ToInt32 ((^*) C state)) <<< 31)), state

                                                     
    let mov ((regD: RegisterID), (op2: Operand), (state: MachineState), (setFlags: bool)) =

        let op2ValTuple = getOperandVal setFlags state op2 // obtains final value of op2 after applying the shifts and appropriately setting the flags
         
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

        let op2Value = fst (getOperandVal false state op2) // carry flag should not be set by the shift, but rather only by the arithmetic operation itself
        
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
        //Need to account for the C flag from the previous stage i.e. newRegVal
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

        //setting final result
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

    let orr ((regD: RegisterID), (regN: RegisterID), (op2: Operand), (state: MachineState), (setFlags: bool)) = //NOTE: for logical instructions, carry flag can be set by flexible second operand

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

        //Obtaining state reflecting sign status
        let state1 = if setFlags then MachineState.setNegative result state0 else state0

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

        //Obtaining state reflecting sign status
        let state1 = if setFlags then MachineState.setNegative result state0 else state0

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

        let state1 = if setFlags then MachineState.setNegative result state0 else state0

        //Obtaining state reflecting zero status
        let finState = if setFlags then MachineState.setZero result state1 else state1

        (^=) (regD) (result) (finState)   

    let subtractWithCarryS ((regD: RegisterID), (regN: Operand), (op2: Operand), (state: MachineState), (includeCarry: bool), (setFlags: bool)) = //Implemented as a - b = a + not(b) +1

      //extracting operand values
      let regNValue = fst (getOperandVal false state regN)

      let op2Value = fst (getOperandVal false state op2)

      //correctly adding an offset of 1 if necessary
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

 //-------------------------------------------------TESTING----------------------------------------------------------
   
    let testAddWithCarryS () = 
        let a = MachineState.make()
        let b = mov (R0, Operand(Literal(-1),NoShift), a, true)
        let c = (^=) R1 2147483647 b
        let d = ( ^- ) C false c
        let e = ( ^- ) V false d
        let f = ( ^- ) N false e
        let g = ( ^- ) Z false f
        let z = addWithCarryS (R3,R1,Operand(ID R1,NoShift),g, false, true)
        let i = addWithCarryS (R3,R0,Operand(ID R0,NoShift),z, false, true)
        printfn "%A" b
        printfn "%A" c
        printfn "%A" z
        printfn "%A" i


    let testMov () = 
        let a1 = MachineState.make()
        let b1 = mov (R0, Operand(Literal(-1073741824),NoShift), a1, true)
        let c1 = mov (R0, Operand(Literal(1),Left 3),b1,true)
        printfn "%A" c1


    let testMvn () =
        let a2 = MachineState.make()
        let b2 = mvn (R0, Operand(Literal(-1), RightL 9), a2, true)
        printfn "%A" b2
        let c2 = mvn (R0, Operand(Literal(-1),RightA 9),b2,true)
        printfn "%A" c2

    let testOrr () = 
        let a3 = MachineState.make()
        let b3 = mov (R0, Operand(Literal(-1),NoShift), a3, false)
        let c3 = mov (R1, Operand(Literal(-1),NoShift),b3,false)
        let d3 = orr (R2, R1, Operand(ID R0,RightA 3), c3, true)
        printfn "%A" d3

    let testAnd () =
        let a3 = MachineState.make()
        let b3 = mov (R0, Operand(Literal(-1),NoShift), a3, false)
        let c3 = mov (R1, Operand(Literal(0),NoShift),b3,false)
        let d3 = andOp (R2, R1, Operand(ID R0, NoShift), c3, true)
        printfn "%A" d3

    let testEor () =
        let a3 = MachineState.make()
        let b3 = mov (R0, Operand(Literal(-1),NoShift), a3, false)
        let c3 = mov (R1, Operand(Literal(1),NoShift),b3,false)
        let d3 = eOR (R2, R1, Operand(ID R0, NoShift), c3, true)
        printfn "%A" d3


    let testBic () =
        let a3 = MachineState.make()
        let b3 = mov (R0, Operand(Literal(-1),NoShift), a3, false)
        let c3 = mov (R1, Operand(Literal(1),NoShift),b3,false)
        let d3 = bic (R2, R1,Operand(ID R0, NoShift), c3, true)
        printfn "%A" d3

    let testSubtractWithCarryS () = 
        let a = MachineState.make()
        let b = mov (R0, Operand(Literal(0),NoShift), a, false)
        let c = (^=) R1 5 b
        let d = ( ^- ) C false c
        let e = ( ^- ) V false d
        let f = ( ^- ) N false e
        let g = ( ^- ) Z false f
        let z = mov (R0, Operand(Literal(-1),NoShift), g, false)
        let h = subtractWithCarryS (R3,Operand(ID R0, NoShift),Operand(Literal 0, NoShift),z, true, true)
        let i = subtractWithCarryS (R2,Operand(ID(R0),NoShift),Operand(ID(R8),NoShift),h, true, true)
        printfn "%A" z
        printfn "%A" h
        printfn "%A" i


    //Further testing of mov function because it is a vital function that implements many instructions
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
