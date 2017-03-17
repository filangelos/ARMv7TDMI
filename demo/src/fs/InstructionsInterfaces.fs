namespace ARM7TDMI

(* 
    High Level Programming @ Imperial College London # Spring 2017
    Project: A user-friendly ARM7TDMI assembler and simulator in F# and Web Technologies ( Github Electron & Fable Compliler )

    Contributors: Youssef Rizk

    Module: Instructions Interfaces
    Description: All the interfaces to the various instructions, pulling from ALU, Memory, and Branching instructions
*)

module InstructionsInterfaces =

    open MachineState
    open Instructions
    open MemoryInstructions

    let executeOrNot (cond: ConditionCode option) (state: MachineState) =
        match cond with
        | Some EQ when ((^*) Z state) = true -> true
        | Some NE when ((^*) Z state) = false -> true
        | Some CS | Some HS when ((^*) C state) = true -> true
        | Some CC | Some LO when ((^*) C state) = false -> true
        | Some MI when ((^*) N state) = true -> true
        | Some EQ when ((^*) N state) = false -> true
        | Some VS when ((^*) V state) = true -> true
        | Some VC when ((^*) V state) = false -> true
        | Some HI when (((^*) C state) = true) && (((^*) Z state) = false) -> true
        | Some LS when (((^*) C state) = false) || (((^*) Z state) = true) -> true
        | Some GE when ((^*) N state) = ((^*) V state) -> true
        | Some LT when ((^*) N state) <> ((^*) V state) -> true
        | Some GT when (((^*) Z state) = false) && (((^*) N state) = ((^*) V state)) -> true
        | Some LE when (((^*) Z state) = true) || (((^*) N state) <> ((^*) V state)) -> true
        | Some AL -> true
        | None -> true
        | _ -> false

    
    //ALU Instructions
           
    let mov_ ((regD: RegisterID), (op2: Operand), (state: MachineState), (setFlags: bool), (cond: ConditionCode option)) = 
        if (executeOrNot (cond) state) then mov ((regD: RegisterID), (op2: Operand), (state: MachineState), (setFlags: bool)) else state

    let mvn_ ((regD: RegisterID), (op2: Operand), (state: MachineState), (setFlags: bool), (cond: ConditionCode option)) =
        if (executeOrNot (cond) state) then mvn ((regD: RegisterID), (op2: Operand), (state: MachineState), (setFlags: bool)) else state

    let add_ ((regD: RegisterID), (regN: RegisterID), (op2: Operand), (state: MachineState), (setFlags: bool), (cond: ConditionCode option)) =
        if (executeOrNot (cond) state) then addWithCarryS (regD, regN, op2, state, false, setFlags) else state

    let adc_ ((regD: RegisterID), (regN: RegisterID), (op2: Operand), (state: MachineState), (setFlags: bool), (cond: ConditionCode option)) =
        if (executeOrNot (cond) state) then addWithCarryS (regD, regN, op2, state, true, setFlags) else state

    let sub_ ((regD: RegisterID), (regN: RegisterID), (op2: Operand), (state: MachineState), (setFlags: bool), (cond: ConditionCode option)) = 
        if (executeOrNot (cond) state) then subtractWithCarryS (regD, Operand(ID(regN),NoShift), op2, state, false, setFlags) else state 
     
    let sbc_ ((regD: RegisterID), (regN: RegisterID), (op2: Operand), (state: MachineState), (setFlags: bool), (cond: ConditionCode option)) = 
        if (executeOrNot (cond) state) then subtractWithCarryS (regD, Operand(ID(regN),NoShift), op2, state, true, setFlags) else state

    let rsb_ ((regD: RegisterID), (regN: RegisterID), (op2: Operand), (state: MachineState), (setFlags: bool), (cond: ConditionCode option)) = 
        if (executeOrNot (cond) state) then subtractWithCarryS (regD, op2, Operand(ID(regN),NoShift), state, false, setFlags) else state

    let rsc_ ((regD: RegisterID), (regN: RegisterID), (op2: Operand), (state: MachineState), (setFlags: bool), (cond: ConditionCode option)) = 
        if (executeOrNot (cond) state) then subtractWithCarryS (regD, op2, Operand(ID(regN),NoShift), state, true, setFlags) else state

    let cmp_ ((regN: RegisterID), (op2: Operand), (state: MachineState), (cond: ConditionCode option)) = 
        let flagState = subtractWithCarryS (R10, Operand(ID(regN),NoShift), op2, state, false, true)
        if (executeOrNot (cond) state) then state |> ( ^- ) V (( ^* ) V flagState) |> ( ^- ) C (( ^* ) C flagState) |> ( ^- ) N (( ^* ) N flagState) |> ( ^- ) Z (( ^* ) Z flagState) else state

    let cmn_ ((regN: RegisterID), (op2: Operand), (state: MachineState), (cond: ConditionCode option)) = 
        let flagState = addWithCarryS (R10, regN, op2, state, false, true)
        if (executeOrNot (cond) state) then state |> ( ^- ) V (( ^* ) V flagState) |> ( ^- ) C (( ^* ) C flagState) |> ( ^- ) N (( ^* ) N flagState) |> ( ^- ) Z (( ^* ) Z flagState) else state

    let tst_ ((regN: RegisterID), (op2: Operand), (state: MachineState), (cond: ConditionCode option)) = 
        let flagState = andOp (R10, regN, op2, state, true)
        if (executeOrNot (cond) state) then state |> ( ^- ) C (( ^* ) C flagState) |> ( ^- ) N (( ^* ) N flagState) |> ( ^- ) Z (( ^* ) Z flagState) else state
     
    let teq_ ((regN: RegisterID), (op2: Operand), (state: MachineState), (cond: ConditionCode option)) = 
        let flagState = eOR (R10, regN, op2, state, true)
        if (executeOrNot (cond) state) then state |> ( ^- ) C (( ^* ) C flagState) |> ( ^- ) N (( ^* ) N flagState) |> ( ^- ) Z (( ^* ) Z flagState) else state

    let ror_ ((regD: RegisterID), (op2: Input), (shift: int), (state: MachineState), (setFlags: bool), (cond: ConditionCode option)) =
        if (executeOrNot (cond) state) then mov (regD, Operand(op2,ROR shift),state,setFlags) else state

    let rrx_ ((regD: RegisterID), (op2: Input), (state: MachineState), (setFlags: bool), (cond: ConditionCode option)) =
        if (executeOrNot (cond) state) then mov (regD, Operand(op2,RRX),state,setFlags) else state

    let lsl_ ((regD: RegisterID), (op2: Operand), (state: MachineState), (setFlags: bool), (cond: ConditionCode option)) = //op2 must have Left shift
        if (executeOrNot (cond) state) then mov (regD,op2,state,setFlags) else state

    let lsr_ ((regD: RegisterID), (op2: Operand), (state: MachineState), (setFlags: bool), (cond: ConditionCode option)) = //op2 must have RightL shift
        if (executeOrNot (cond) state) then mov (regD,op2,state,setFlags) else state

    let asr_ ((regD: RegisterID), (op2: Operand), (state: MachineState), (setFlags: bool), (cond: ConditionCode option)) = //op2 must have RightA shift
        if (executeOrNot (cond) state) then mov (regD, op2, state, setFlags) else state

    let orr_ ((regD: RegisterID), (regN: RegisterID), (op2: Operand), (state: MachineState), (setFlags: bool), (cond: ConditionCode option)) =
        if (executeOrNot (cond) state) then orr (regD, regN, op2, state, setFlags) else state
  
    let and_ ((regD: RegisterID), (regN: RegisterID), (op2: Operand), (state: MachineState), (setFlags: bool), (cond: ConditionCode option)) =
        if (executeOrNot (cond) state) then andOp (regD, regN, op2, state, setFlags) else state

    let eor_ ((regD: RegisterID), (regN: RegisterID), (op2: Operand), (state: MachineState), (setFlags: bool), (cond: ConditionCode option)) =
        if (executeOrNot (cond) state) then eOR (regD, regN, op2, state, setFlags) else state

    let bic_ ((regD: RegisterID), (regN: RegisterID), (op2: Operand), (state: MachineState), (setFlags: bool), (cond: ConditionCode option)) =
        if (executeOrNot (cond) state) then bic (regD, regN, op2, state, setFlags) else state


    //Memory Instructions

    let ldr_ ((regD: RegisterID), (regN: AddressRegister), (state: MachineState), (cond: ConditionCode option)) = 
        if (executeOrNot (cond) state) then loadInstructionW (regD, regN, state) else state

    let ldrb_ ((regD: RegisterID), (regN: AddressRegister), (state: MachineState), (cond: ConditionCode option)) = 
        if (executeOrNot (cond) state) then loadInstructionB (regD, regN, state) else state

    let str_ ((result: Data), (regN: AddressRegister), (state: MachineState), (cond: ConditionCode option)) = 
        if (executeOrNot (cond) state) then storeInstructionW (result, regN, state) else state
    
    let strb_ ((result: Data), (regN: AddressRegister), (state: MachineState), (cond: ConditionCode option)) = 
        if (executeOrNot (cond) state) then storeInstructionB (result, regN, state) else state

    let ldm_ ((addMode: AddressMode), (addRegister: RegisterID), (regList: RegisterID list), (state: MachineState), (writeBack: bool), (cond: ConditionCode option)) = 
        if (executeOrNot (cond) state) then loadMultiple (addMode, addRegister, regList, state, writeBack) else state

    let stm_ ((addMode: AddressMode), (addRegister: RegisterID), (regList: RegisterID list), (state: MachineState), (writeBack: bool), (cond: ConditionCode option)) = 
        if (executeOrNot (cond) state) then storeMultiple (addMode, addRegister, regList, state, writeBack) else state

    let pseudo_ldr_ ((regD: RegisterID), (expr: Expression), (state: MachineState), (cond: ConditionCode option)) = 
        if (executeOrNot (cond) state) then ldrPseudo (regD, expr, state) else state

    let adr_ ((regD: RegisterID), (expr: Expression), (state: MachineState), (cond: ConditionCode option)) = //expr has to be label
        if (executeOrNot (cond) state) then ldrPseudo (regD, expr, state) else state



// test to check against VisUAL 
    let a = MachineState.make()
    let b = add_ (R0, R0, Operand(Literal -1, NoShift), a, true, None)
    printfn "Quick Test"
    printfn "%A" b