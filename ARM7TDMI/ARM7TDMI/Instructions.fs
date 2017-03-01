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
        try (Checked.(+) a b),state with
        e -> 1,(( ^- ) V true state)
     
    //sets Zero flag. Returns a state
    let setZero a state =
        if a = 0 then (( ^- ) Z true state) else (( ^- ) Z false state)

    //sets N flag. Returns a state
    let setNegative a state =
        if a < 0 then ( ^- ) N true state else ( ^- ) N false state

    //sets Carry flag by representing inputs as uint64 and applying logical operations, returns result in 32-bits
    let setCarry operation a b state =
        let newA = uint64 (uint32 a)
        let newB = uint64 (uint32 b)
        (int ((operation newA newB) &&& 4294967295uL),( ^- ) C (((operation newA newB) &&& (1UL <<< 32)) > 0uL) state)

    

    //Op2 can be either a register or a literal
    //type OP2 = Reg of Register | Op of Data
    //right now implementing adcs
    let addWithCarry ((regD: RegisterID), (regN: RegisterID), (op2: Operand), (state: MachineState), (carry: bool))=

        //extracting operand values
        let regNValue = (^.) regN state

        let op2Value = 
            match op2 with 
            | ID(register) -> (^.) register state
            | Literal(data) -> data

        //including the value of carry into the first register and producing a new state : (newRegisterValue:Data,newState:MachineState)
        let newRegVal =
            match carry && ( ^* ) C state with
            |true ->  setCarry (+) regNValue (Data 1) state
            |false -> (regNValue, state)
       
        //updating the state encapsulation
        let state1 = snd newRegVal

        //Producing result of the operation, along with a state that reflects the change by the carry : (finalResult: Data, newState: MachineState)
        let finVal = match ( ^* ) C state1 with
                     |true -> (fst (setCarry (+) regNValue op2Value state1)),state1
                     |false -> setCarry (+) regNValue op2Value state1

        //updating the state encapsulation again     
        let state2 = snd finVal

        let result = fst finVal

        let state3 = snd (setOverflow regNValue op2Value state2)

        let state4 = setNegative result state3

        //Obtaining final state reflecting signed overflow
        let finState = setZero result state4

        (^=) (regD) (result) (finState)

    //let subtractWithCarry ((regD: RegisterID), (regN: RegisterID), (op2: RegisterID), (state: MachineState)) =


    

    //test code for add without carry
    let a = MachineState.make()
    let b = (^=) R0 -2147483648 a
    let c = (^=) R1 -2147483648 b
    let d = ( ^- ) C false c
    let e = ( ^- ) V false d
    let f = ( ^- ) N false e
    let g = ( ^- ) Z false f



    let inputs = [(R0,R0,ID(R1),g, false)]

    let testOutput = List.map addWithCarry inputs

    printfn "%A" testOutput