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



    let add_ ((regD: RegisterID), (regN: RegisterID), (op2: Operand), (state: MachineState), (setFlags: bool)) =
        addWithCarryS (regD, regN, op2, state, false, setFlags)

    let adc_ ((regD: RegisterID), (regN: RegisterID), (op2: Operand), (state: MachineState), (setFlags: bool)) =
        addWithCarryS (regD, regN, op2, state, true, setFlags)

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

    let lsl_ ((regD: RegisterID), (op2: Operand), (state: MachineState), (setFlags: bool)) = //op2 must have Left shift
        mov (regD,op2,state,setFlags)

    let lsr_ ((regD: RegisterID), (op2: Operand), (state: MachineState), (setFlags: bool)) = //op2 must have Right shift
        mov (regD,op2,state,setFlags)

    let asr_ ((regD: RegisterID), (op2: Input), (shift: int), (state: MachineState), (setFlags: bool)) =
        mov (regD, Operand(op2, RightA shift), state, setFlags)

