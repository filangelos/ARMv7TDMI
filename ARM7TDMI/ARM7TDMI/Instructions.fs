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
    //Do not care about actual result
    let setOverflow a b state =
        try Checked.(+) a b with
        e -> (( ^- ) V true state) |> ignore ; 1

    //sets Carry flag by representing inputs as uint64 and applying logical operations, returns result in 32-bits
    let setCarry operation a b state =
        let newA = uint64 (uint32 a)
        let newB = uint64 (uint32 b)
        ( ^- ) C (((operation newA newB) &&& (1UL <<< 32)) > 0uL) state |> ignore;
        (operation newA newB) &&& 4294967295uL
    

    //Op2 can be either a register or a literal
    //type OP2 = Reg of Register | Op of Data

    let addWithCarry ((regD: RegisterID), (regN: RegisterID), (op2: Operand), (state: MachineState))=
        let regNValue = (^.) regN state

        let op2Value = 
            match op2 with 
            | ID(register) -> (^.) register state
            | Literal(data) -> data

        let (carryVal: Data) =
            match ( ^* ) C state with
            |true ->  Data 1
            |false -> Data 0
       

        (^=) (regD) (carryVal) (state)

    //let subtractWithCarry ((regD: RegisterID), (regN: RegisterID), (op2: RegisterID), (state: MachineState)) =


    

    //test code for add without carry
    let a = MachineState.make()
    let b = (^=) R0 -2147483648 a
    let c = (^=) R1 -2147483648 b


    let inputs = [(R0,R5,Literal(7),c);(R0,R1,ID(R2),c);(R10,R1,ID(R5),c);(R4,R1,ID(R7),c);(R5,R12,ID(R2),c)]

    let testOutput = List.map addWithCarry inputs

    printfn "%A" testOutput