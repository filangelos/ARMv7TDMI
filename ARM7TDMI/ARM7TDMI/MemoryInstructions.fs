namespace ARM7TDMI


(* 
    High Level Programming @ Imperial College London # Spring 2017
    Project: A user-friendly ARM7TDMI assembler and simulator in F# and Web Technologies ( Github Electron & Fable Compliler )

    Contributors: Youssef Rizk

    Module: Memory Instructions
    Description: Memory Instructions for LDR/STR operations, etc.
*)

module MemoryInstructions =


    type Offset = 
        | TempOffset of int // LDR     R8, [R10, #4] 
        | PreIndex of int // LDR     R8, [R10, #4]!
        | PostIndex of int // LDR     R8, [R10], #4
        | NoOffset // syntax: LDR     R8, [R10] 

    type AddressRegister = 
        {register: RegisterID; offset: Offset}

    open MachineState
    open Instructions

    //Memory-specific shortcuts
    let ( gb ) = Optics.get MachineState.Byte_
    let ( sb ) = Optics.set MachineState.Byte_
    let ( gw ) = Optics.get MachineState.Word_
    let ( sw ) = Optics.set MachineState.Word_
    //let ( ^. ) = Optics.get MachineState.Register_
    //let ( ^= ) = Optics.set MachineState.Register_
    //let ( ^* ) = Optics.get MachineState.Flag_
    //let ( ^- ) = Optics.set MachineState.Flag_

    let addressWithOffset state = function
                                  | {register = R_; offset = PreIndex x} | {register = R_; offset = TempOffset x} -> ((^.) R_ state) + x
                                  | {register = R_; offset = _} -> (^.) R_ state


    let loadInstruction ((regD: RegisterID), (regN: AddressRegister), (state: MachineState)) =

        let address = addressWithOffset state regN

        let state1 = (^=) regD ((gw) address state) state

        match regN with
        | {register = R_; offset = PreIndex x} | {register = R_; offset = PostIndex x} -> (^=) R_ (((^.) R_ state) + x) state1
        | _ -> state1



    let storeInstruction ((result: Data), (regN: AddressRegister), (state: MachineState)) =

        let address = addressWithOffset state regN

        let state1 = (sw) address result state

        match regN with
        | {register = R_; offset = PreIndex x} | {register = R_; offset = PostIndex x} -> (^=) R_ (((^.) R_ state) + x) state1
        | _ -> state1

  


    //testing loadInstruction
    let a3 = MachineState.make()
    let b3 = storeInstruction (2, {register= R0; offset= TempOffset 4}, a3)
    printfn "%A" a3
    printfn "%A" b3
    let c3 = storeInstruction (1, {register= R0; offset= PreIndex 8}, b3)
    printfn "%A" c3



    ////testing loadInstruction
    //let a3 = MachineState.make()
    //let b3 = loadInstruction (R2, {register= R0; offset= TempOffset 4}, a3)
    //printfn "%A" a3
    //printfn "%A" b3
    //let c3 = loadInstruction (R1, {register= R0; offset= PreIndex 4}, b3)
    //printfn "%A" c3

         
