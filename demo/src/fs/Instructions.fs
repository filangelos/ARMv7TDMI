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
                      | Operand(op2Val, RRX) -> ((int (uint32 (opVal state op2Val) >>> 1)) ||| ((System.Convert.ToInt32 ((^*) C state)) <<< 31)), state

                                   
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