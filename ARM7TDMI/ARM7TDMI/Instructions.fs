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
    open Common

    // stortcuts
    let ( ^. ) = Optics.get MachineState.Register_
    let ( ^= ) = Optics.set MachineState.Register_
    let ( ^* ) = Optics.get MachineState.Flag_
    let ( ^- ) = Optics.set MachineState.Flag_

    //Op2 can be either a register or a literal
    //type OP2 = Reg of Register | Op of Data

    let addWithCarryR ((regD: RegisterID), (regN: RegisterID), (op2: Operand), (state: MachineState))=
        let regNValue = (^.) regN state

        let op2Value = 
            match op2 with 
            | ID(register) -> (^.) register state
            | Literal(data) -> data

        let (carryVal: Data) =
            match ( ^* ) C state with
            |true -> regNValue + op2Value + Data 1
            |false -> regNValue + op2Value
       

        (^=) (regD) (carryVal) (state)

    //let subtractWithCarry ((regD: RegisterID), (regN: RegisterID), (op2: RegisterID), (state: MachineState)) =


    

    //test code for add without carry
    let a = MachineState.make()
    let b = (^=) R0 1 a
    let c = (^=) R1 2 b
    let d = (^=) R2 3 c
    let e = (^=) R3 4 d
    let f = (^=) R4 5 e
    let g = (^=) R5 6 f
    let h = (^=) R6 57 g
    let i = (^=) R7 23 h
    let j = (^=) R8 67 i
    let k = (^=) R9 34 j
    let l = (^=) R10 24 k
    let m = (^=) R11 7 l
    let n = (^=) R12 9 m


    let inputs = [(R0,R5,Literal(7),n);(R0,R1,ID(R2),n);(R10,R1,ID(R5),n);(R4,R1,ID(R7),n);(R5,R12,ID(R2),n)]

    let testOutput = List.map addWithCarryR inputs

    printfn "%A" testOutput